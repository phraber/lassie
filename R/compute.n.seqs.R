#' @keywords internal
compute.n.seqs <- function(is_tf=NULL, 
    timepoint_per_sequence=NULL, multiplicity=NULL) {
    
    n_seqs_per_timepoint <- rep(NA, length(timepoint_per_sequence))

# tf_freqs: for each timepoint (columns), set non-tf frequency per
# site (rows) NA sites are not counted in denominator but are counted
# in n_seqs_per_timepoint

    if (is.null(multiplicity)) {

	n_seqs_per_timepoint <- 
	    sapply(1:length(timepoint_per_sequence), function(i) 
		length(which(grepl(timepoint_per_sequence[i], 
		     rownames(is_tf)))))
    } else {

# this does not work because grep does not match unambiguously across sequence names
#	n_seqs_per_timepoint <- 
#	    sapply(1:length(timepoint_per_sequence), function(i)
#	        sum(multiplicity[which(grepl(timepoint_per_sequence[i], 
#		    rownames(is_tf)))]))

	n_seqs_per_timepoint <- table(timepoint_per_sequence)
    }

#    names(n_seqs_per_timepoint) <- timepoint_per_sequence

    n_seqs_per_timepoint
}
