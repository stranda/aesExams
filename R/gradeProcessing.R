#' Create Grade Upload File
#'
#' Generates a CSV file for uploading grades, containing student IDs, usernames, and their respective grades.
#'
#' @param s A list of student objects, each containing student information including the student ID and username.
#' @param g A list of corresponding grades for the students.
#' @param fn The filename for the generated CSV file, default is "gradeUpload.csv".
#' @param examname A string indicating the name of the exam (for d2l grade sheet), default is "ExamIB".
#' @return dataframe, same as written to disk
#' @examples
#' students <- list(student1, student2)
#' grades <- list(grade1, grade2)
#' createGradeUpload(students, grades, "grades.csv")
#' @export
createGradeUpload = function(s,g, fn="gradeUpload.csv",examname="ExamIB")
{
    if (length(s)!=length(g)) stop("The students and the grades should be the same length")
    outstr = c(
        paste0("OrgDefinedId , Username ,", examname," Points Grade, End-of-Line Indicator")
    )
    for(i in 1:length(s))
    {
        outstr = c(outstr,
                   paste0(s[[i]]$student$id,",",gsub("#","",s[[i]]$student$Username),",", sum(g[[i]]),",#")
                   )
    }

    cat(file=fn,append=F,paste(outstr,collapse="\n"))
    read.csv(fn)
}

#' Create Grade Feedback
#'
#' Generates feedback for each student based on their answers and the corresponding grades for an exam. The feedback includes a summary of the student's performance by question, with specifics on their answers and the correct answers.
#'
#' @param s A list of student objects, each containing detailed information including answers to exam questions.
#' @param g A list of corresponding grades for each question for the students.
#' @param examname A string indicating the name of the exam, default is "ExamIB".
#' @return A list where each element is a vector of strings composing the feedback for a single student. This feedback is not currently saved to a file within the function and might need to be captured or processed further by the calling code.
#' @examples
#' students <- list(student1, student2)
#' grades <- list(grades1, grades2)
#' feedback <- createGradeFeedback(students, grades, "Final Exam")
#' @export
createGradeFeedback = function(s,g, examname="ExamIB")
{
    lapply(1:length(s),function(i)
    {
        st = s[[i]]
        gt = g[[i]]
        txt = paste0("Dear ",st$student$First.Name,",\n\n")
        txt = c(txt,
                paste0("This is a summary of your ",examname," by question.  There is also a solution document attached\n\n"))
        if (length(st$l.ans)>0)
            for (j in 1:length(st$exam))
            {
                print(paste(i,j))
                txt = c(txt,
                        paste0("Question number: ",j,", Points awarded: ",gt[j]))
                
                if (st$exam[[j]]$metainfo$type=="mchoice")
                {
                    numchoice=length(st$l.ans[[j]]$AnswerMatch)
                    txt = c(txt,
                            paste0("\tYour answer for this question:",
                                   paste(letters[1:numchoice],st$l.ans[[j]]$AnswerMatch,sep='=',collapse=" "))
                            )
                    txt = c(txt,
                            paste0("\tThe correct answer:",
                                   paste(letters[1:numchoice],st$exam[[j]]$metainfo$solution,sep='=',collapse=" ")))
                } else {
                    txt = c(txt,
                            paste0("\tYour answer for this question: ",
                                   paste(string2num(st$l.ans[[j]]$AnswerMatch))))
                    txt = c(txt,
                            paste0("\tThe correct answer: ",
                                   paste(st$exam[[j]]$metainfo$solution)))
                }
                
                
            } else txt = c(txt,"There does not seem to be a set of answers for your exam")
        
        txt = c(txt, paste("\n\nPlease look at the attached solutions to get a better idea about answers"))
        txt
    })
}


#' Create Graded Emails for Students
#'
#' This function prepares and potentially sends graded emails to students based on their exam submissions. It uses feedback generated for each student, attaches relevant files, and sets a subject line for the email.
#'
#' @param s A list of student objects, each containing detailed information including the student's email and the filename of the solution document to be attached.
#' @param g A list of grades corresponding to each student, intended to match the student list one-to-one.
#' @param subjectLine A character string to be used as the subject line of the emails, defaulting to "Graded genetics exam".
#' @return This function does not return a value but executes an action to prepare emails for sending. Each email is composed with a subject line, body text based on student feedback, and an attachment containing the student's graded exam solutions.
#' @details The function leverages the `gmailr` package for email composition and attachment handling. Before using this function, ensure that the `gmailr` setup has been completed and the necessary authentication is in place to interact with Google's email services.
#' @examples
#' # Assuming 'students' is a list of student objects with email and solutions file name,
#' # and 'grades' is a list of corresponding grades for those students:
#' createGradedEmail(students, grades)
#' @note This function assumes that `createGradeFeedback` has been defined and is available for generating the body text of the emails. The actual sending of emails may require additional steps or function calls according to the `gmailr` package documentation.
#' @export
createGradedEmail = function(s,g,subjectLine="Graded genetics exam",examname="Exam1")
{
    if (length(s)!=length(g)) stop("in email creation the students and grades are different lengths")
    bodies = sapply(createGradeFeedback(s,g,examname),function(x){paste0(x,collapse="\n")})
    lapply(1:length(s),function(i)
    {
        email <- gmailr::gm_mime() |>
            gmailr::gm_to(s[[i]]$student$Email) |>
            gmailr::gm_subject(subjectLine) |>
            gmailr::gm_text_body(bodies[[i]]) |>
            gmailr::gm_attach_file(s[[i]]$student$SolutionsFN)
    })
}
