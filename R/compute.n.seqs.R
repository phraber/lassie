#' @keywords internal
### TO DO: reorder columns numerically by sample time-point
###  cf. compute.tf.loss()
### NB: order is implied by timepoint_per_sequence?
compute.n.seqs <- function(is_tf=NULL, timepoint_per_sequence=NULL) {
    
    ns_per_timepoint <- rep(NA, length(timepoint_per_sequence))

# cf. tf_freqs: for each timepoint (columns), set non-tf frequency per
# site (rows) NA sites are not counted in denominator but are counted
# in ns_per_timepoint

    ns_per_timepoint <- sapply(1:length(timepoint_per_sequence), function(i) 
	length(which(grepl(timepoint_per_sequence[i], rownames(is_tf)))))

#    names(n_seqs_per_timepoint) <- timepoint_per_sequence

    ns_per_timepoint
}
