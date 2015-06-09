### TO DO: Make this friendly as a helper function
#' @keywords internal
longest.insertion.per.sequence <- function(aln_allcolumns, tf_index=1) {

    insertion_lengths <- sapply(1:nrow(aln_allcolumns), function(i)
	longest.insertion.in.sequence(aln_allcolumns, this_sequence=i, 
	    tf_index) )

    names(insertion_lengths) <- rownames(aln_allcolumns)

    insertion_lengths
}

