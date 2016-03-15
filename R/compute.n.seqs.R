#' @keywords internal
compute.n.seqs <- function(is_tf=NULL, timepoint_per_sequence=NULL) {

    col.order <- order(as.numeric(gsub("[A-Z]", "", timepoint_per_sequence), ignore.case=T))

    table(ns_per_timepoint)[col.order]
}
