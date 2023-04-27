exams2brightspace <- function(file, dir = ".", n = 1L, nsamp = NULL,
  name = "brightspace", quiet = TRUE, edir = NULL, tdir = NULL, sdir = NULL, verbose = FALSE,
  resolution = 100, width = 4, height = 4, svg = FALSE,
  converter = "pandoc", scoring = "AllOrNothing", ...)
{
  ## create exam with HTML text
  htmltransform <- make_exercise_transform_html(converter = converter, ...)
  rval <- xexams(file,n = n,
    driver = list(sweave = list(quiet = quiet, pdf = FALSE, png = !svg, svg = svg,
      resolution = resolution, width = width, height = height),
      read = NULL, transform = htmltransform, write = NULL),
    dir = dir, edir = edir, tdir = tdir, sdir = sdir, verbose = verbose)


  ## convenience functions
  cleanup <- function(x) gsub('"', '""', paste(x, collapse = "\n"), fixed = TRUE)
  bs_csv <- function(x, prefix = "") {
    if(is.null(x$solution)) {
      sol <- ""
    } else {
      sol <- x$solution
      if(!is.null(x$solutionlist)) sol <- c(sol, '', c('<ol type = "a">', paste('<li>', x$solutionlist, '</li>'), '</ol>'))
    }
    c(
      sprintf('NewQuestion,%s,,,', ifelse(x$metainfo$type == "schoice", "SA", ifelse(x$metainfo$type=="mchoice","MS","WR"))),
      sprintf('ID,"%s",,,', paste0(cleanup(x$metainfo$file))),
      sprintf('Title,"%s",,,', paste0(cleanup(x$metainfo$name))),
      sprintf('QuestionText,"%s",HTML,,', cleanup(x$question)),
      sprintf('Points,%s,,,', if(is.null(x$metainfo$points)) 1 else x$metainfo$points),
      'Difficulty,1,,,',
      'Image,,,,',
      sprintf('Scoring,%s,,,', scoring),
      sprintf('Option,%s,%s,HTML,%s,HTML',
        as.numeric(x$metainfo$solution) * ifelse(x$metainfo$type == "schoice", 100, ifelse(x$metainfo$type == "mchoice",1,100)),
        sapply(x$questionlist, cleanup),
        if(is.null(x$solutionlist)) rep.int("", length(x$questionlist)) else sapply(x$solutionlist, cleanup)
      ),
      'Hint,,,,',
      sprintf('Feedback,"%s",HTML,,', cleanup(sol)),
      ''
    )
  }

  ## cycle through all exercises
  n <- length(rval)
  m <- length(rval[[1L]]) 
  csv <- vector("list", length = n * m)
  for(j in 1L:m) {
    for(i in 1L:n) {
      csv[[(j - 1L) * n + i]] <- bs_csv(rval[[i]][[j]], prefix = sprintf("R%s Q%s : ", i, j))
    }
  }
  
  ## collapse and write to CSV
  csv <- unlist(csv)
  writeLines(csv, file.path(dir, paste0(name, ".csv")))

  ## return xexams list invisibly
  invisible(rval)
}

