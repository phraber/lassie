

#' @keywords internal
longest.indel.in.sequence <- function(this_sequence) {
    max(nchar(unlist(strsplit(this_sequence, "([A-Za-z])+"))))
}
