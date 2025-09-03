#' Expand Cloze Questions into Sub-Questions
#'
#' This function processes a list of exercises, expanding any cloze-type exercises 
#' into their individual sub-questions. Each sub-question is treated as a separate
#' exercise with its own metadata. Exercises that are not cloze-type are left unchanged.
#'
#' @param e A list of exercises, where each exercise is expected to be a list containing
#'   at least a `$metainfo` list with a `$type` attribute. Cloze-type exercises should
#'   include a `$metainfo$solution` list that contains the answers for the sub-questions.
#'
#' @return A list of exercises where cloze-type exercises are expanded into multiple 
#'   exercises based on the number of solutions specified in the `$metainfo$solution` list.
#'   Each sub-question maintains a link to the original exercise's file and markup details
#'   and is individually identified by a sub-question identifier.
#'
#' @examples
#' # Assuming `exercises` is your list of exercises that includes cloze-type and other types of exercises
#' expanded_exercises = expandCloze(exercises)
#'
#' @details
#' The function first counts the number of cloze-type exercises. If any are found, it then counts 
#' the total number of sub-questions across all cloze-type exercises. It creates a new list of the appropriate 
#' length to accommodate all exercises after expansion. Each non-cloze exercise is copied as is, 
#' while cloze exercises are expanded into multiple entries, one for each sub-question. Sub-questions are
#' augmented with a unique identifier and maintain links to the original exercise's metadata.
#'
expandCloze <- function(e) {
  origtype = sapply(e, function(x) {x$metainfo$type})
  numcloze = sum(as.numeric(origtype == "cloze"))
  if (numcloze > 0) {
    ## count total number of subquestions in all of the clozes
    subq = sum(sapply(e[origtype == "cloze"], function(cl) {length(cl$metainfo$solution)}))
    ret = vector("list", length(origtype) - numcloze + subq) # make a vector the expanded length
    rq = 1
    for (i in 1:length(e)) {
      if (e[[i]]$metainfo$type != "cloze")  ## assumes all string subquestions
      {
        ret[[rq]] = e[[i]]
        rq = rq + 1
      } else {
        for (j in 1:length(e[[i]]$metainfo$solution)) {
          ret[[rq]] = list(
            question = e[[i]]$question,
            questionlist = NULL,
            solution = NULL,
            solutionlist = NULL,
            metainfo = list(
              file = e[[i]]$metainfo$file,
              markup = e[[i]]$metainfo$markup,
              type = "string", # need to address this if you want to add more complex cloze
              name = paste(e[[i]]$metainfo$name, "Sub-Question ", letters[j]),
              title = paste(e[[i]]$metainfo$name, "Sub-Question ", letters[j]),
              section = NULL,
              version = NULL,
              solution = e[[i]]$metainfo$solution[j],
              shuffle = FALSE,
              length = 1,
              string = paste("answer to", letters[j], " is ", e[[i]]$metainfo$solution[j])),
            supplements = e[[i]]$supplements
          )
          rq = rq + 1
        }
      }
    }
  } else {
    ret = e
  }
  return(ret)
}
