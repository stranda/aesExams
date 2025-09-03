#' Student Answers Processing Function
#'
#' This function processes exam answers from students, associating each student's
#' responses with their ID and the specific questions from the exam. It generates
#' a list of student answers, the questions, and the correct answers from a provided exam.
#'
#' @param path_to_directory The directory path where the grades are stored.
#' @param path_to_exam The directory path where the exam was created.
#' @param ansfn Filename of the answer file from D2L, defaulting to "Exam I Part B (there is also a part A) - Attempt Details.csv".
#' @param idmapfn Filename for the ID mapping file, defaulting to a relative path for the 2024 Spring Genetics course grades export.
#' @param examName The base name for exam answers files.
#' @param solnName The base name for solution files.
#' @param answersFN Filename for the specific answers file in RDS format, default "exam1SpecificAnswers.RDS".
#' @param questions_to_ignore a vector of question numbers _not_ to include in the grading.  Default=NULL
#' @param student_answers_to_ignore a vector of answer numbers _not_ to include in the grading.  Default=NULL
#' @param questions_to_allmatch a vector equal to length of exam questions of the questions to require allmatch Default=NULL means that allmatch is always in play.  Must be length of exam regardless of question type
#' @return A list where each element corresponds to a student, including their answers,
#'         student information, and the correct exam answers.
#'
#' @examples
#' studentAns("path/to/grades", "path/to/exam")
#'
#' @export
studentAns = function(
  path_to_directory, # where are the grades
  path_to_exam,      # where was the exam created?
  ansfn = "Exam I Part B (there is also a part A) - Attempt Details.csv", # answer file from d2l
  idmapfn = "../../2024Spring Genetics (BIOL-305-01)_GradesExport_2024-02-22-01-07.csv", 
  examName = "exam1Answers",
  solnName = "exam1Answers-solutions",
  answersFN = "exam1SpecificAnswers.RDS",
  questions_to_ignore = NULL,
  student_answers_to_ignore = NULL,
  questions_to_allmatch = NULL
  )
{    
    if (FALSE) {
        ansfn = "Exam I Part B (there is also a part A) - Attempt Details.csv"
        idmapfn = "../../2024Spring Genetics (BIOL-305-01)_GradesExport_2024-02-22-01-07.csv"
        examName = "exam1Answers"
        solnName = "exam1Answers-solutions"
        answersFN = "exam1SpecificAnswers.RDS"
        questions_to_ignore = NULL
    }
    
    students = read.csv(idmapfn)
    students$id = as.integer(gsub("\\#","",students[,1]))
    if (!"examNum" %in% names(students))
    {
        students$examID = 1:nrow(students)
        warning("Assigning student exam IDs based on order in the student file rather than explicitly")
    } else {
           students$examID = students$examNum
    }
#    students$examID = students$examNum
    
    students$QuestionsFN = paste0(examName, sprintf(students$examID, fmt = "%02i"), ".pdf")
    students$SolutionsFN = paste0(solnName, sprintf(students$examID, fmt = "%02i"), ".pdf")
    
    students$QuestionsFN = paste0(path_to_exam, "/", students$QuestionsFN)
    students$SolutionsFN = paste0(path_to_exam, "/", students$SolutionsFN)
    
    ans = read.csv(ansfn)
    names(ans) = c("id", gsub("\\.", "", names(ans)[-1]))
    ans = ans[!is.na(ans$Q),]
    if (!is.null(student_answers_to_ignore))
    {
        ans=ans[!ans$Q %in% student_answers_to_ignore,]
        print("removed these answers from the student responses in oaks")
        print(student_answers_to_ignore)
    }
                                        # R/exams format for exam stored in a RDS file (created when the exam created)
    exid = sprintf(min(students$examID):max(students$examID),fmt="%02d")
    exams = readRDS(paste0(path_to_exam, "/", answersFN))
    if ((length(exams)==1) & (length(exid)>1))
    {
        ecp = exams[[1]]
        exams = lapply(exid,function(i){ecp})
        names(exams)=paste0("exam",exid)
    }
    
    if (is.null(questions_to_allmatch))
        questions_to_allmatch=rep(TRUE,length(exams[[1]]))

    if (length(questions_to_allmatch)==length(exams[[1]]))
        exams = lapply(exams,function(ex)
        {
            lapply(1:length(ex),function(i)
            {
                c(ex[[i]],allmatch = questions_to_allmatch[i])
            })
        })    
    else
        stop("the questions_to_allmatch vector is different than the length of the exam questions")
    
    if (!is.null(questions_to_ignore))
    {
        print(paste("removing answers"))
        print(questions_to_ignore)
        ans=ans[!ans$Q %in% questions_to_ignore,]
        exams=lapply(exams,function(ex){ex[-questions_to_ignore]})
    }
    
    print(unique(ans$Q))
    
    studentAns = lapply(1:nrow(students), function(i) {
        print(i)
        ldf = ans[ans$id == students[i, "id"], ]
        ldf=ldf[ldf$Attempt==max(ldf$Attempt),]
        list(
            l.ans = lapply(unique(ldf$Q), function(q) {
                ldf[ldf$Q == q, ]
            }),
            student = students[i, ],
            exam.orig = exams[[students[i,"examID"]]],
            exam = expandCloze(exams[[students[i,"examID"]]])
        )
    })
}
