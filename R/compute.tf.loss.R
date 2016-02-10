#' @keywords internal

### TO DO: reorder columns numerically by sample time-point
###  cf. compute.n.seqs()
compute.tf.loss <- function(is_tf, n_seqs_per_timepoint=NULL) {

#    if (!any(is_tf))
    if (is.null(n_seqs_per_timepoint) | length(n_seqs_per_timepoint) == 0)
        return ( NULL )

    tf_freqs <- matrix(NA, ncol=length(n_seqs_per_timepoint), nrow=ncol(is_tf))

    colnames(tf_freqs) <- names(n_seqs_per_timepoint)
    rownames(tf_freqs) <- colnames(is_tf)
    # tf_loss matrix has one row per site and one column per timepoint sampled

    # iterate over timepoints
    for (i in 1:length(n_seqs_per_timepoint)) {

        timepoint <- names(n_seqs_per_timepoint)[i]
        # work on subset of rows from timepoint t 

        this_tf <- subset(is_tf, grepl(timepoint, rownames(is_tf)))

        if (nrow(this_tf) > 0)
            tf_freqs[,i] <- sapply(1:ncol(this_tf), function(j)
                length(which(this_tf[, j] == T)) /  # offending line
                length(which(!is.na(this_tf[ ,j]))))
    }

    return ( signif(100*(1 - tf_freqs), 4) ) # columns=timepoints, rows=sites
}
