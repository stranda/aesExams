#' Grade Students' Answers
#'
#' This function grades students' answers against the correct answers for an exam. It supports multiple question types, including multiple choice.
#'
#' @param s A student object that includes demographic information, the student's answers, and the correct answers.
#' @param points points per question or a vector of points for all the questions
#' @param tol Tolerance level for grading numerical answers. Default is 0.01.
#'
#' @return A list where each element corresponds to a student's performance, including scores and potentially feedback on each question.
#' @examples
#' # Assume 'students' is a list of student objects with their answers and the correct answers
#' gradeResults <- gradeStudents(students)
#' @export
gradeStudents=function(s,points=1,tol=0.01) #s is the student object that has demographic, actual answers, and correct answers 
{
    lst=lapply(s,function(st)
    {
        print(st$student$id)
        e=st$exam
        a=st$l.ans
        if (length(a)>0)
            {
                if (length(e)!=length(a)) {stop("there should be the same number of answers as questions")}
                sapply(1:length(e),function(i)
                {
                    if (e[[i]]$metainfo$type %in% c("schoice","mchoice")) #mchoice type question
                    {
                        grade.mchoice(e[[i]],a[[i]],e[[i]]$allmatch)
                    } else if (e[[i]]$metainfo$type=="string") #string type question but for a number only
                    {
                        grade.number(e[[i]],a[[i]],tol=tol)
                    } else {
                        stop(paste(e[[i]]$metainfo$type," undefined question type in gradeStudents"))
                    }
                })
            } else NULL
    })
    lapply(lst,function(l)
    {
        if (length(points)<length(l)) points=rep(points[1],length(l))
        else points=points[1:length(l)]
        l*points
    })
}

