#' Grade Multiple Choice Responses
#'
#' This function calculates the proportion of correctly matched answers for multiple choice questions.
#' It compares a vector of correct answers to a vector of student's answers, allowing for different lengths
#' of answer vectors and adjusting them accordingly.
#'
#' @param correct A list containing the correct answers' metadata, including the solutions.
#' @param answer A list containing the student's answers, specifically whether each option was selected.
#' @param allmatch Logical indicating whether to match all answers exactly. Default is TRUE.
#'
#' @return A numeric value representing the proportion of correct answers out of the total possible correct answers.
#'
#' @examples
#' # Assuming correct and answer are predefined lists with the necessary structure
#' grade.mchoice(correct, answer)
#'
#' @export
grade.mchoice = function(correct,answer,allmatch=TRUE)
{
    ans = answer$AnswerMatch=="Checked"
    cor = correct$metainfo$solution
    la =length(ans)
    lc = length(cor)
    if (la>lc) cor=c(cor,rep(F,la-lc))
    if (lc>la) ans=c(ans,rep(F,lc-la))
    num.match = length(which(cor & (ans==cor)))
    if (allmatch) num.match/sum(cor) else #returns proportion of matches out of possible
     as.numeric(num.match)>0                                    
}

#' Grade Numeric Responses
#'
#' This function grades numeric responses based on a tolerance level. It compares a student's numeric answer to the correct
#' answer, adjusting for a specified tolerance. This is particularly useful for grading answers that might not be exactly
#' identical due to rounding or minor calculation differences.
#'
#' @param correct A list containing the correct numeric answer's metadata, including the solution.
#' @param answer A list containing the student's answer, extracted and converted to numeric format.
#' @param tol The tolerance level for grading numeric answers. Default is 0.01, indicating a 1% tolerance.
#'
#' @return Returns 1 if the student's answer is within the tolerance of the correct answer, otherwise returns 0.
#'
#' @examples
#' # Assuming correct and answer are predefined lists with the necessary structure
#' grade.number(correct, answer)
#'
#' @export
grade.number = function(correct,answer,tol=0.01)
{
    ans = string2num(answer$AnswerMatch)
#    print(length(ans))
    if (is.na(ans)) print(paste("string2num for ans is NA; answer$AnswerMatch=",answer$AnswerMatch))
    sol = as.numeric(correct$metainfo$solution)
    ##if the difference between ans and correct in tolerance return 1 otherwise 0
    if (sol!=0)
    {
        res = abs((sol-ans)/sol)
        if (is.na(res)) res=tol #test below is for less than.  therefore return nomatch
        if ( res < tol) 1 else 0
    } else {
        as.numeric(sol==ans)
    }
}
