### TO DO: Make this friendly as a helper function
#' @keywords internal
longest.deletion.per.sequence <- function(aln_allcolumns) {
    deletion_lengths <- sapply(1:nrow(aln_allcolumns), function(i)
	longest.indel.in.sequence(seqinr::c2s(aln_allcolumns[i, ])))

    names(deletion_lengths) <- rownames(aln_allcolumns)

    deletion_lengths
}
