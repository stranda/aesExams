.ess_get_completions <-
function(string, end){
  if(getRversion() > '2.14.1'){
    comp <- compiler::enableJIT(0L)
    olderr <- getOption('error')
    options(error=NULL)
    on.exit({options(error = olderr)
             compiler::enableJIT(comp)})
  }
  utils:::.assignLinebuffer(string)
  utils:::.assignEnd(end)
  utils:::.guessTokenFromLine()
  utils:::.completeToken()
  c(get('token', envir=utils:::.CompletionEnv),
    utils:::.retrieveCompletions())
  }
.ess.funargs <-
function(object){
  funname <- deparse(substitute(object))
  if(getRversion() > '2.14.1'){
    comp <- compiler::enableJIT(0L)
    olderr <- getOption('error')
    options(error=NULL)
    on.exit({
      compiler::enableJIT(comp)
      options(error = olderr)
    })
  }
  fun <- tryCatch(object, error=function(e) NULL) ## works for special objects also
  if(is.null(fun)) NULL
  else if(is.function(fun)) {
    special <- grepl('[:$@[]', funname)
    args <- if(!special){
      fundef <- paste(funname, '.default',sep='')
      do.call('argsAnywhere', list(fundef))
    }

    if(is.null(args))
      args <- args(fun)
    if(is.null(args))
      args <- do.call('argsAnywhere', list(funname))

    fmls <- formals(args)
    fmls_names <- names(fmls)
    fmls <- gsub('\"', '\\\"', as.character(fmls), fixed=TRUE)
    args_alist <- sprintf("'(%s)", paste("(\"", fmls_names, "\" . \"", fmls, "\")", sep = '', collapse = ' '))
    allargs <-
      if(special) fmls_names
      else tryCatch(gsub('=', '', utils:::functionArgs(funname, ''), fixed = T), error=function(e) NULL)
    allargs <- sprintf("'(\"%s\")", paste(allargs, collapse = '\" \"'))
    envname <- environmentName(environment(fun))
    cat(sprintf('(list \"%s\" %s %s)\n', envname, args_alist, allargs))
  }
}
.help.ESS <-
function (topic, package = NULL, lib.loc = NULL, verbose = getOption("verbose"), 
    try.all.packages = getOption("help.try.all.packages"), help_type = getOption("help_type")) 
{
    types <- c("text", "html", "pdf")
    if (!missing(package)) 
        if (is.name(y <- substitute(package))) 
            package <- as.character(y)
    if (missing(topic)) {
        if (!missing(package)) {
            help_type <- if (!length(help_type)) 
                "text"
            else match.arg(tolower(help_type), types)
            if (interactive() && help_type == "html") {
                if (tools:::httpdPort == 0L) 
                  tools::startDynamicHelp()
                if (tools:::httpdPort <= 0L) 
                  return(library(help = package, lib.loc = lib.loc, 
                    character.only = TRUE))
                browser <- if (.Platform$GUI == "AQUA") {
                  get("aqua.browser", envir = as.environment("tools:RGUI"))
                }
                else getOption("browser")
                browseURL(paste0("http://127.0.0.1:", tools:::httpdPort, 
                  "/library/", package, "/html/00Index.html"), 
                  browser)
                return(invisible())
            }
            else return(library(help = package, lib.loc = lib.loc, 
                character.only = TRUE))
        }
        if (!missing(lib.loc)) 
            return(library(lib.loc = lib.loc))
        topic <- "help"
        package <- "utils"
        lib.loc <- .Library
    }
    ischar <- tryCatch(is.character(topic) && length(topic) == 
        1L, error = identity)
    if (inherits(ischar, "error")) 
        ischar <- FALSE
    if (!ischar) {
        reserved <- c("TRUE", "FALSE", "NULL", "Inf", "NaN", 
            "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_")
        stopic <- deparse(substitute(topic))
        if (!is.name(substitute(topic)) && !stopic %in% reserved) 
            stop("'topic' should be a name, length-one character vector or reserved word")
        topic <- stopic
    }
    help_type <- if (!length(help_type)) 
        "text"
    else match.arg(tolower(help_type), types)
    paths <- index.search(topic, find.package(package, lib.loc, 
        verbose = verbose))
    tried_all_packages <- FALSE
    if (!length(paths) && is.logical(try.all.packages) && !is.na(try.all.packages) && 
        try.all.packages && missing(package) && missing(lib.loc)) {
        for (lib in .libPaths()) {
            packages <- .packages(TRUE, lib)
            packages <- packages[is.na(match(packages, .packages()))]
            paths <- c(paths, index.search(topic, file.path(lib, 
                packages)))
        }
        paths <- paths[paths != ""]
        tried_all_packages <- TRUE
    }
    paths <- unique(paths)
    attributes(paths) <- list(call = match.call(), topic = topic, 
        tried_all_packages = tried_all_packages, type = help_type)
    class(paths) <- "help_files_with_topic"
    paths
}
