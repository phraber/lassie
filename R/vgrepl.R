#' @keywords internal
vgrepl <- function(patterns, x) {
    # search a vector for multiple patterns at once
    sapply(1:length(patterns), function(y)
	grepl(patterns[y], x, ignore.case=T))
}
