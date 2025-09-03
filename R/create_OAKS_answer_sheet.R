#' Create CSV String for Single Answer Question
#'
#' This function creates a string formatted as a CSV entry for a single answer question to be uploaded to OAKS. It is designed to produce a formatted string based on the question number, points, (optionally) the sub-question number, and question name.
#'
#' @param num Integer, the question number.
#' @param points Integer, the number of points the question is worth. Default is 1.
#' @param clozenum Integer or NULL, the sub-question number if applicable. NULL by default.
#' @param qname String, the name of the question. Optional, empty by default.
#' 
#' @return A character string formatted as a CSV entry for uploading a single answer question to OAKS.
#' @export
#'
#' @examples
#' string2csv(num = 1, points = 2, qname = "Sample Question")
string2csv = function(num, points = 1, clozenum = NULL, qname = "") {
    if (is.null(clozenum))
        qstr = paste0("Enter the value you calculated for question ", num)
    else
        qstr = paste0("Enter the value you calculated for question ", num, "-", clozenum)

    paste("NewQuestion, SA",
          paste0("ID,OAKS test answer question", num, clozenum, " ", qname),
          paste0("Title,", qstr),
          paste("QuestionText,", qstr, "Enter only the value. No units or percent symbols please. If the number is a fraction enter in decimal form."),
          paste("Points,", points),
          "Difficulty,1",
          "Answer,",
          "Hint,",
          "Feedback,",
          sep = "\n")
}

#' Create CSV String for Multiple Choice Question
#'
#' This function generates a string formatted as a CSV entry for a multiple-choice question to be uploaded to OAKS. It caters to formatting based on question number, points, the number of options, (optionally) the sub-question number, and the question name.
#'
#' @param num Integer, the question number.
#' @param points Integer, the number of points the question is worth. Default is 1.
#' @param numquest Integer, the number of options for the multiple-choice question. Default is 5.
#' @param clozenum Integer or NULL, the sub-question number if applicable. NULL by default.
#' @param qname String, the name of the question. Optional, empty by default.
#'
#' @return A character string formatted as a CSV entry for uploading a multiple-choice question to OAKS.
#' @export
#'
#' @examples
#' mchoice2csv(num = 1, points = 2, numquest = 4, qname = "MCQ Sample")
mchoice2csv = function(num, points = 1, numquest = 5, clozenum = NULL, qname = "") {
    if (is.null(clozenum))
        qstr = paste0("Enter the choice for question ", num)
    else
        qstr = paste0("Enter the choice for question ", num, "-", clozenum)

    opts = paste(sapply(1:numquest, function(i) {
       paste("Option, 1, This is option", letters[i], ", No feedback\n") 
    }), collapse = "")
    
    paste("NewQuestion, MS",
          paste0("ID,OAKS test answer question", num, clozenum, " ", qname),
          paste0("Title,", qstr),
          paste("QuestionText,", qstr, ""),
          paste("Points,", points),
          "Difficulty,1",
          "Image,",
          "Scoring,RightAnswers",
          opts,
          "Hint,",
          "Feedback,",
          sep = "\n")
}

#' Generate Answer Sheet for OAKS
#'
#' This function generates an answer sheet as a series of CSV formatted strings based on the metadata provided in an RDS file. It supports multiple choice, single choice, and single answer question types.
#'
#' @param metaFN String, the file name of the RDS file containing the metadata for the exam or quiz.
#'
#' @return A character vector, each element of which is a CSV formatted string for a question to be uploaded to OAKS.
#' @export
#'
#' @examples
#' makeAnsSheet(metaFN = "exam_data.RDS")
makeAnsSheet = function(metaFN = "exam2.RDS") {
    meta = readRDS(metaFN)
    
    retstr = NULL
    for (i in 1:length(meta[[1]])) {
        q = meta[[1]][[i]]
        if (q$metainfo$type %in% c("mchoice", "schoice")) # multiple choice question
        {
            retstr = c(retstr, mchoice2csv(num = i, points = q$metainfo$points, numquest = length(q$metainfo$solution), qname = q$metainfo$name))
        } else if (q$metainfo$type %in% c("string","num")) # single numerical answer
        {
            retstr = c(retstr, string2csv(num = i, points = q$metainfo$points, qname = q$metainfo$name))
        } else if (q$metainfo$type == "cloze") # multiple sub-questions (could be strings or mchoice)
        {
            retstr = c(retstr, paste(sapply(1:length(q$metainfo$clozetype), function(j) {
                if (q$metainfo$clozetype[j]  %in% c("string","num")) {
                    string2csv(num = i, points = q$metainfo$points, qname = q$metainfo$name, clozenum = letters[j])
                } else stop("can only deal with string/num cloze at the moment")
            }), collapse = "\n"))
        } else stop("don't recognize the question type")
    }
    retstr
}

