#' Compare a vector of patterns with an input string
#' 
#' Returns Boolean vector of length equal to the input vector that indicates which patterns match the string.
#'
#' @param patterns Character vector of regular expressions to evaluate.
#' @param x String to search for expressions.
#'
#' @export
vgrepl <- function(patterns, x) {
    # search a vector for multiple patterns at once
    sapply(1:length(patterns), function(y)
	grepl(patterns[y], x, ignore.case=T))
}
