#' Create Emails for individualized questions
#' 
#' This function prepares and potentially sends graded emails to students based on their exam submissions. It uses feedback generated for each student, attaches relevant files, and sets a subject line for the email.
#'
#' @param studentfn the name of the student file.  The order is used to assign exam numbers 
#' @param examPath A character string to be used as the subject line of the emails, defaulting to "Graded genetics exam".
#' @param examName A character string to be used as the subject line of the emails, defaulting to "Graded genetics exam".
#' @param subject This is the subject of the email sent to each student
#' @param body This is the body of the email sent to each student
#' @return a data frame with the studetn information and the name of the files and solutions
#' @details The function leverages the `gmailr` package for email composition and attachment handling. Before using this function, ensure that the `gmailr` setup has been completed and the necessary authentication is in place to interact with Google's email services.
#' @export
#'
createQuestionEmails = function(studentfn,examPath=".",examName="Exam1",subject="", body="")
{
    exam=readRDS(paste0(examName,".RDS"))
    
    students=read.csv(studentfn)
    students$QuestionFN=paste0(examName,sprintf(students$examNum,fmt="%02i"),".pdf")
    students$SolutionFN=paste0(examName,"-solutions",sprintf(students$examNum,fmt="%02i"),".pdf")
    students$QuestionFN=paste(examPath,students$QuestionFN,sep="/")
    students$SolutionFN=paste(examPath,students$SolutionFN,sep="/")
    
  ret=lapply(1:nrow(students),function(i)
  {
      x=sapply(exam[[students$examNum[i]]],function(x)
      {
          r=x$supplements[grep("*.csv",x$supplements)];if (length(r)<1) NULL else r
      },simplify=T)
      x=unlist(x[!sapply(x,is.null)])
      x = c(students$QuestionFN[i],x)
      
      email <- gmailr::gm_mime() |>
          gmailr::gm_to(students$Email[i]) |>
          gmailr::gm_subject(subject) |>
          gmailr::gm_text_body(body)
      for (n in x)
          email <- email |> gmailr::gm_attach_file(n)
      email
  })
    ret
}
