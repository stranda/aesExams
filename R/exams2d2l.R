exams2d2l <- function (file, n = 1L, nsamp = NULL, dir = ".", name = NULL, 
    quiet = TRUE, edir = NULL, tdir = NULL, sdir = NULL, verbose = FALSE, 
    rds = FALSE, resolution = 100, width = 4, height = 4, svg = FALSE, 
    encoding = "UTF-8", num = NULL, mchoice = NULL, schoice = mchoice, 
    string = NULL, cloze = NULL, template = "qti21", duration = NULL, 
    stitle = NULL, ititle = NULL, adescription = "Please solve the following exercises.", 
    sdescription = "", maxattempts = 1, cutvalue = NULL, solutionswitch = TRUE, 
    casesensitive = TRUE, cloze_schoice_display = "auto", navigation = "nonlinear", 
    allowskipping = TRUE, allowreview = FALSE, allowcomment = FALSE, 
    shufflesections = FALSE, zip = TRUE, points = NULL, eval = list(partial = TRUE, 
        negative = FALSE), converter = NULL, envir = NULL, base64 = TRUE, 
    mode = "hex", include = NULL, selection = c("pool", "exam"), 
    ...) 
{
    if (is.null(converter)) {
        converter <- if (any(tolower(tools::file_ext(unlist(file))) == 
            "rmd")) 
            "pandoc"
        else "ttm"
    }
    htmltransform <- if (converter %in% c("tth", "ttm")) {
        make_exercise_transform_html(converter = converter, ..., 
            base64 = base64, mode = mode)
    }
    else {
        make_exercise_transform_html(converter = converter, ..., 
            base64 = base64)
    }
    if (is.null(name)) 
        name <- file_path_sans_ext(basename(template))
    name <- gsub("\\s", "_", name)
    name_base <- if (is_number1(name)) 
        paste0("_", name)
    else name
    if (isTRUE(rds)) 
        rds <- name
    is.xexam <- FALSE
    if (is.list(file)) {
        if (any(grepl("exam1", names(file)))) 
            is.xexam <- TRUE
    }
    if (!is.xexam) {
        exm <- xexams(file, n = n, nsamp = nsamp, driver = list(sweave = list(quiet = quiet, 
            pdf = FALSE, png = !svg, svg = svg, resolution = resolution, 
            width = width, height = height, encoding = encoding, 
            envir = envir), read = NULL, transform = htmltransform, 
            write = NULL), dir = dir, edir = edir, tdir = tdir, 
            sdir = sdir, verbose = verbose, rds = rds, points = points)
    }
    else {
        exm <- file
        rm(file)
    }
    itembody <- list(num = num, mchoice = mchoice, schoice = schoice, 
        cloze = cloze, string = string)
    for (i in c("num", "mchoice", "schoice", "cloze", "string")) {
        if (is.null(itembody[[i]])) 
            itembody[[i]] <- list()
        if (is.list(itembody[[i]])) {
            if (is.null(itembody[[i]]$eval)) 
                itembody[[i]]$eval <- eval
            if (i == "cloze" && is.null(itembody[[i]]$eval$rule)) 
                itembody[[i]]$eval$rule <- "none"
            if (is.null(itembody[[i]]$solutionswitch)) 
                itembody[[i]]$solutionswitch <- solutionswitch
            if (is.null(itembody[[i]]$casesensitive)) 
                itembody[[i]]$casesensitive <- casesensitive
            if (i == "cloze" && is.null(itembody[[i]]$cloze_schoice_display)) 
                itembody[[i]]$cloze_schoice_display <- cloze_schoice_display
            itembody[[i]] <- do.call("make_itembody_qti21", itembody[[i]])
        }
        if (!is.function(itembody[[i]])) 
            stop(sprintf("wrong specification of %s", sQuote(i)))
    }
    dir <- path.expand(dir)
    if (is.null(tdir)) {
        dir.create(tdir <- tempfile())
        on.exit(unlink(tdir))
    }
    else {
        tdir <- path.expand(tdir)
    }
    if (!file.exists(tdir)) 
        dir.create(tdir)
    pkg_dir <- find.package("exams")
    template <- path.expand(template)
    template <- ifelse(tolower(substr(template, nchar(template) - 
        3L, nchar(template))) != ".xml", paste(template, ".xml", 
        sep = ""), template)
    template <- ifelse(file.exists(template), template, file.path(pkg_dir, 
        "xml", basename(template)))
    if (!all(file.exists(template))) {
        stop(paste("The following files cannot be found: ", paste(basename(template)[!file.exists(template)], 
            collapse = ", "), ".", sep = ""))
    }
    is_exam <- match.arg(selection) == "exam"
    xml <- readLines(template[1L])
    if (length(start <- grep("<assessmentTest", xml, fixed = TRUE)) != 
        1L || length(end <- grep("</assessmentTest>", xml, fixed = TRUE)) != 
        1L) {
        stop(paste("The XML template", template, "must contain exactly one opening and closing <assessmentTest> tag!"))
    }
    assessment_xml <- xml[start:end]
    if (length(start <- grep("<assessmentSection", xml, fixed = TRUE)) != 
        1L || length(end <- grep("</assessmentSection>", xml, 
        fixed = TRUE)) != 1L) {
        stop(paste("The XML template", template, "must contain exactly one opening and closing <assessmentSection> tag!"))
    }
    section_xml <- xml[start:end]
    if (length(start <- grep("<manifest", xml, fixed = TRUE)) != 
        1L || length(end <- grep("</manifest>", xml, fixed = TRUE)) != 
        1L) {
        stop(paste("The XML template", template, "must contain exactly one opening and closing <manifest> tag!"))
    }
    manifest_xml <- xml[start:end]
    nx <- length(exm)
    nq <- if (!is.xexam) 
        length(exm[[1L]])
    else length(exm)
    make_test_ids <- function(n, type = c("test", "section", 
        "item")) {
        switch(type, test = paste(name_base, make_id(9), sep = "_"), 
            paste(type, formatC(1:n, flag = "0", width = nchar(n)), 
                sep = "_"))
    }
    test_id <- make_test_ids(type = "test")
    sec_ids <- paste(test_id, make_test_ids(nq, type = "section"), 
        sep = "_")
    stitle2 <- if (!is.null(stitle)) 
        rep(stitle, length.out = nx)
    else stitle
    if (!is.null(stitle)) 
        stitle <- rep(stitle, length.out = nq)
    if (!is.null(ititle)) 
        ititle <- rep(ititle, length.out = nq)
    if (is.null(adescription)) 
        adescription <- ""
    if (is.null(sdescription) || identical(sdescription, FALSE)) 
        sdescription <- ""
    sdescription <- rep(sdescription, length.out = nq)
    sdescription[sdescription != ""] <- sprintf("<rubricBlock view=\"candidate\"><p>%s</p></rubricBlock>", 
        sdescription[sdescription != ""])
    maxattempts[!is.finite(maxattempts) | maxattempts < 0] <- 0
    if (length(maxattempts) > 1L) {
        maxattempts <- rep(maxattempts, length.out = nq)
        sdescription <- paste0(sprintf("<itemSessionControl maxAttempts=\"%s\"/>", 
            round(as.numeric(maxattempts))), "\n", sdescription)
    }
    dir.create(test_dir <- file.path(file_path_as_absolute(tdir), 
        name))
    items <- sec_xml <- sec_items_D <- sec_items_R <- sec_xml_mat <- NULL
    maxscore <- 0
    for (j in 1:nq) {
        sxmlj <- section_xml
        stj <- stitle[j]
        if (isTRUE(stj)) 
            stj <- as.character(j)
        if (is.null(stj) || isFALSE(stj)) 
            stj <- ""
        if (stj == "") 
            sxmlj <- gsub("visible=\"true\"", "visible=\"false\"", 
                sxmlj, fixed = TRUE)
        sec_xml <- c(sec_xml, gsub("##SectionId##", sec_ids[j], 
            sxmlj, fixed = TRUE))
        sec_xml <- gsub("##SectionTitle##", stj, sec_xml, fixed = TRUE)
        sec_xml <- gsub("##SectionDescription##", sdescription[j], 
            sec_xml, fixed = TRUE)
        if (is.xexam) 
            nx <- length(exm[[j]])
        item_ids <- paste(sec_ids[j], make_test_ids(nx, type = "item"), 
            sep = "_")
        sec_items_A <- NULL
        for (i in 1:nx) {
            if (is.xexam) {
                if (i < 2) 
                  jk <- j
                j <- i
                i <- jk
            }
            if (i < 2) {
                tpts <- if (is.null(exm[[i]][[j]]$metainfo$points)) 
                  1
                else exm[[i]][[j]]$metainfo$points
                maxscore <- maxscore + sum(tpts)
            }
            type <- exm[[i]][[j]]$metainfo$type
            iname <- paste(item_ids[if (is.xexam) 
                j
            else i], type, sep = "_")
            exm[[i]][[j]]$metainfo$id <- iname
            if (!is.null(ititle)) {
                if (is.logical(ititle[j])) {
                  if (!ititle[j]) 
                    exm[[i]][[j]]$metainfo$name <- ""
                  else exm[[i]][[j]]$metainfo$name <- as.character(j)
                }
                else {
                  exm[[i]][[j]]$metainfo$name <- ititle[j]
                }
            }
            else {
                if (!is.null(exm[[i]][[j]]$metainfo$title)) {
                  exm[[i]][[j]]$metainfo$name <- exm[[i]][[j]]$metainfo$title
                }
                else {
                  exm[[i]][[j]]$metainfo$name <- as.character(j)
                }
            }
            if (FALSE) {
                exm[[i]][[j]]$question <- "Here is the questiontext..."
                exm[[i]][[j]]$solution <- "This is the solutiontext..."
                exm[[i]][[j]]$solutionlist <- NA
            }
            exm[[i]][[j]]$converter <- converter
            ibody <- itembody[[type]](exm[[i]][[j]])
            exm[[i]][[j]]$converter <- NULL
            if (length(exm[[i]][[j]]$supplements)) {
                if (!base64) {
                  if (!file.exists(media_dir <- file.path(test_dir, 
                    "media"))) 
                    dir.create(media_dir)
                  sj <- 1
                  while (file.exists(file.path(media_dir, sup_dir <- paste("supplements", 
                    sj, sep = "")))) {
                    sj <- sj + 1
                  }
                  dir.create(ms_dir <- file.path(media_dir, sup_dir))
                }
                for (si in seq_along(exm[[i]][[j]]$supplements)) {
                  f <- basename(exm[[i]][[j]]$supplements[si])
                  if (base64) {
                    replacement <- fileURI(exm[[i]][[j]]$supplements[si])
                    if (any(grepl(dirname(exm[[i]][[j]]$supplements[si]), 
                      ibody))) {
                      ibody <- gsub(dirname(exm[[i]][[j]]$supplements[si]), 
                        replacement, ibody, fixed = TRUE)
                    }
                    else {
                      if (any(grepl(f, ibody))) {
                        ibody <- gsub(paste(f, "\"", sep = ""), 
                          paste(replacement, "\"", sep = ""), 
                          ibody, fixed = TRUE)
                      }
                    }
                  }
                  else {
                    file.copy(exm[[i]][[j]]$supplements[si], 
                      file.path(ms_dir, f))
                    fid <- gsub("\\", "", gsub("/", "_", file.path("media", 
                      sup_dir, f), fixed = TRUE), fixed = TRUE)
                    fhref <- file.path("media", sup_dir, f)
                    sec_items_R <- c(sec_items_R, paste("<resource identifier=\"", 
                      paste(fid, "id", sep = "_"), "\" type=\"imsqti_item_xmlv2p1\" href=\"", 
                      fhref, "\">", sep = ""), paste("<file href=\"", 
                      fhref, "\"/>", sep = ""), "</resource>")
                    if (any(grepl(dirname(exm[[i]][[j]]$supplements[si]), 
                      ibody))) {
                      ibody <- gsub(dirname(exm[[i]][[j]]$supplements[si]), 
                        file.path("media", sup_dir), ibody, fixed = TRUE)
                    }
                    else {
                      if (any(grepl(f, ibody))) {
                        ibody <- gsub(paste(f, "\"", sep = ""), 
                          paste("media/", sup_dir, "/", f, "\"", 
                            sep = ""), ibody, fixed = TRUE)
                      }
                    }
                  }
                }
            }
            writeLines(c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", 
                ibody), file.path(test_dir, paste(iname, "xml", 
                sep = ".")))
            sec_items_A <- c(sec_items_A, paste("<assessmentItemRef identifier=\"", 
                iname, "\" href=\"", iname, ".xml\" fixed=\"false\"/>", 
                sep = ""))
            sec_items_D <- c(sec_items_D, paste("<dependency identifierref=\"", 
                paste(iname, "id", sep = "_"), "\"/>", sep = ""))
            sec_items_R <- c(sec_items_R, paste("<resource identifier=\"", 
                paste(iname, "id", sep = "_"), "\" type=\"imsqti_item_xmlv2p1\" href=\"", 
                iname, ".xml\">", sep = ""), paste("<file href=\"", 
                iname, ".xml\"/>", sep = ""), "</resource>")
        }
        if (is_exam) {
            sec_xml_mat <- rbind(sec_xml_mat, sec_items_A)
        }
        else {
            sec_xml <- gsub("##SectionItems##", paste(sec_items_A, 
                collapse = "\n"), sec_xml, fixed = TRUE)
        }
    }
    if (is_exam) {
        select <- 1
        etitle <- NULL
        qtitle <- NULL
        if (is.null(etitle) || isFALSE(etitle)) 
            etitle <- ""
        test_id_exam <- paste(test_id, "Exam", sep = "_")
        sec_xml <- c(paste0("<assessmentSection identifier=\"", 
            test_id_exam, "\" fixed=\"false\" title=\"", etitle, 
            "\" visible=\"", if (etitle != "") "true" else "false", 
            "\">"), paste0("<selection select=\"", select, "\"/>"), 
            paste0("<ordering shuffle=\"", if (TRUE) "true" else "false", 
                "\"/>"))
        for (j in 1:ncol(sec_xml_mat)) {
            test_id_exam_j <- paste(test_id_exam, j, sep = "_")
            stj2 <- stitle2[j]
            if (is.null(stj2) || isFALSE(stj2)) 
                stj2 <- ""
            vis <- if (stj2 == "") 
                "false"
            else "true"
            sec_xml <- c(sec_xml, paste0("<assessmentSection identifier=\"", 
                test_id_exam_j, "\" fixed=\"false\" title=\"", 
                stj2, "\" visible=\"", vis, "\">"), paste0("<ordering shuffle=\"", 
                if (!identical(shufflesections, FALSE)) "true" else "false", 
                "\"/>"))
            for (i in 1:length(sec_xml_mat[, j])) {
                sec_xml <- c(sec_xml, paste0("<assessmentSection identifier=\"", 
                  paste0(test_id_exam_j, "_exercise_", i), "\" fixed=\"false\" title=\"", 
                  qtitle[i], "\" visible=\"", if (is.null(qtitle[i]) || 
                    (qtitle[i] == "")) "false" else "true", "\">"), 
                  sec_xml_mat[i, j], "</assessmentSection>")
            }
            sec_xml <- c(sec_xml, "</assessmentSection>")
        }
        sec_xml <- c(sec_xml, "</assessmentSection>")
    }
    if (!identical(shufflesections, FALSE) & !is_exam) {
        shufflesections <- if (identical(shufflesections, TRUE)) 
            ""
        else as.character(shufflesections)
        sec_outer_xml <- section_xml[1L]
        sec_outer_xml <- gsub("##SectionId##", paste(test_id, 
            "part1", "sections", sep = "_"), sec_outer_xml, fixed = TRUE)
        sec_outer_xml <- gsub("##SectionTitle##", shufflesections, 
            sec_outer_xml, fixed = TRUE)
        sec_xml <- c(sec_outer_xml, "<ordering shuffle=\"true\"/>", 
            sec_xml, "</assessmentSection>")
    }
    manifest_xml <- gsub("##AssessmentId##", test_id, manifest_xml, 
        fixed = TRUE)
    manifest_xml <- gsub("##AssessmentTitle##", name, manifest_xml, 
        fixed = TRUE)
    manifest_xml <- gsub("##ManifestItemDependencies##", paste(sec_items_D, 
        collapse = "\n"), manifest_xml, fixed = TRUE)
    manifest_xml <- gsub("##ManifestItemRessources##", paste(sec_items_R, 
        collapse = "\n"), manifest_xml, fixed = TRUE)
    manifest_xml <- gsub("##AssessmentDescription##", adescription, 
        manifest_xml, fixed = TRUE)
    manifest_xml <- gsub("##Date##", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), 
        manifest_xml, fixed = TRUE)
    if (any(maxattempts != 1L) && solutionswitch) {
        warning("if solutionswitch is TRUE, maxattempts should typically be 1 so that the solution cannot be copied by participants")
    }
    assessment_xml <- gsub("##AssessmentId##", test_id, assessment_xml, 
        fixed = TRUE)
    assessment_xml <- gsub("##TestpartId##", paste(test_id, "part1", 
        sep = "_"), assessment_xml, fixed = TRUE)
    assessment_xml <- gsub("##TestTitle##", name, assessment_xml, 
        fixed = TRUE)
    assessment_xml <- gsub("##AssessmentSections##", paste(sec_xml, 
        collapse = "\n"), assessment_xml, fixed = TRUE)
    assessment_xml <- gsub("##Score##", "0.0", assessment_xml, 
        fixed = TRUE)
    assessment_xml <- gsub("##MaxScore##", maxscore, assessment_xml, 
        fixed = TRUE)
    if (!is.null(cutvalue) && is.na(cutvalue)) 
        cutvalue <- NULL
    if (!is.null(cutvalue)) {
        j <- grep("</outcomeDeclaration>", assessment_xml, fixed = TRUE)
        j <- j[length(j)]
        assessment_xml[j] <- paste("</outcomeDeclaration>", "<outcomeDeclaration identifier=\"PASS\" cardinality=\"single\" baseType=\"boolean\">", 
            "<defaultValue>", "<value>false</value>", "</defaultValue>", 
            "</outcomeDeclaration>", sep = "\n")
        j <- grep("</setOutcomeValue>", assessment_xml, fixed = TRUE)
        j <- j[length(j)]
        assessment_xml[j] <- paste("</setOutcomeValue>", "<outcomeCondition>", 
            "<outcomeIf>", "<gte>", "<sum>", "<testVariables variableIdentifier=\"SCORE\"/>", 
            "</sum>", "<baseValue baseType=\"float\">##CutValue##</baseValue>", 
            "</gte>", "<setOutcomeValue identifier=\"PASS\">", 
            "<baseValue baseType=\"boolean\">true</baseValue>", 
            "</setOutcomeValue>", "</outcomeIf>", "<outcomeElse>", 
            "<setOutcomeValue identifier=\"PASS\">", "<baseValue baseType=\"boolean\">false</baseValue>", 
            "</setOutcomeValue>", "</outcomeElse>", "</outcomeCondition>", 
            sep = "\n")
        assessment_xml <- gsub("##CutValue##", round(as.numeric(cutvalue), 
            digits = 8), assessment_xml, fixed = TRUE)
    }
    assessment_xml <- gsub("##MaxAttempts##", round(as.numeric(maxattempts[1L])), 
        assessment_xml, fixed = TRUE)
    assessment_xml <- gsub("##ShowSolution##", if (solutionswitch) 
        "true"
    else "false", assessment_xml, fixed = TRUE)
    assessment_xml <- gsub("##NavigationMode##", match.arg(navigation, 
        c("nonlinear", "linear")), assessment_xml, fixed = TRUE)
    assessment_xml <- gsub("##AllowComment##", if (allowcomment) 
        "true"
    else "false", assessment_xml, fixed = TRUE)
    assessment_xml <- gsub("##AllowSkipping##", if (allowskipping) 
        "true"
    else "false", assessment_xml, fixed = TRUE)
    assessment_xml <- gsub("##AllowReview##", if (allowreview) 
        "true"
    else "false", assessment_xml, fixed = TRUE)
    if (!is.null(duration)) {
        dursecs <- round(duration * 60)
        duration <- paste("<timeLimits maxTime=\"", dursecs, 
            "\"/>", sep = "")
    }
    else {
        duration <- ""
    }
    assessment_xml <- gsub("##TimeLimits##", duration, assessment_xml, 
        fixed = TRUE)
    writeLines(c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", 
        manifest_xml), file.path(test_dir, "imsmanifest.xml"))
    writeLines(c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", 
        assessment_xml), file.path(test_dir, paste(test_id, "xml", 
        sep = ".")))
    if (!is.null(include)) {
        if (is.list(include) && !is.null(names(include))) {
            for (i in names(include)) writeLines(include[[i]], 
                file.path(test_dir, i))
        }
        else if (is.character(include) && all(file.exists(include))) {
            if (any(!file.exists(include))) {
                if (all(file.exists(file.path(edir, include)))) 
                  include <- file.path(edir, include)
            }
            if (any(!file.exists(include))) {
                owd <- getwd()
                if (all(file.exists(file.path(owd, include)))) 
                  include <- file.path(owd, include)
            }
            file.copy(include, file.path(test_dir, basename(include)))
        }
        else {
            warning("ignoring 'include' argument due to unknown specification")
        }
    }
    if (zip) {
        owd <- getwd()
        setwd(test_dir)
        zip(zipfile = zipname <- paste(name, "zip", sep = "."), 
            files = list.files(test_dir))
        setwd(owd)
    }
    else zipname <- list.files(test_dir)
    file.copy(file.path(test_dir, zipname), dir, recursive = TRUE)
    attr(exm, "test_id") <- test_id
    invisible(exm)
}
