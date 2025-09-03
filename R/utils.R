#' Convert String Representations of Numbers to Numeric Format
#'
#' This function takes a vector of strings representing numbers in various formats
#' (e.g., with letters, commas, as fractions, or percentages) and attempts to convert
#' them into numeric format. It handles fractions and percentages specifically, converting
#' them to their decimal equivalents. Other non-numeric characters are removed, and the
#' remaining numeric string is converted to a numeric type. Warnings are suppressed during
#' the conversion process. NA is returned for elements that cannot be converted.
#'
#' @param x A character vector where each element is a string representing a number.
#'   The default is an empty string, which will result in an NA value.
#'
#' @return A numeric vector with the same length as `x`, where each string representation
#'   of a number has been converted to numeric format. Elements that cannot be converted
#'   to a number return NA.
#'
#' @examples
#' string2num(c("10%", "1/4", "3,000", "5 apples", "2.5"))
#' # Expected output: c(0.1, 0.25, 3000, NA, 2.5)
#'
#' @export

string2num = function(x="")
{
    nums = suppressWarnings(as.numeric(gsub("[a-zA-Z ,]*","",x)),)
    problems = which(is.na(nums))
    for (i in problems)
    {
        b = sapply(strsplit(x[i]," "),function(z) z[1])
        if (length(grep("/",b))>0)
        {
            nums[i] = eval(parse(text=b))
        } else if (length(grep("%",b))>0)
        {
            nums[i] = as.numeric(gsub("%","",b))/100
        } else {
            nums[i] = as.numeric(gsub("\\?",".",gsub("\\.","",gsub("(^[0-9]+).([0-9]+)","\\1?\\2",b))))
        }
    }
    return(nums)
}

