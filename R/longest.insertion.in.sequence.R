#' @keywords internal
longest.insertion.in.sequence <- function(aln_allcolumns, 
    this_sequence, tf_index=1) {

    # Create a mock sequence of either '-' (for sites inserted
    # relative to the sequence identified by tf_index) or 'X' (all
    # other sites).  By inverting the notation used for deletions, we
    # can reuse the function longest.indel.in.sequence() to count the
    # insertions.

    insert_sites <- sapply(1:ncol(aln_allcolumns), function(i)
	ifelse(aln_allcolumns[tf_index, i] == "-" & 
	    aln_allcolumns[this_sequence, i] != "-", "-", "X" ) )

    longest.indel.in.sequence(seqinr::c2s(insert_sites))
}
