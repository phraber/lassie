#' @keywords internal
create.tf.loss.matrix <- function(aas_aln=NULL, tf_index, n_seqs, timepoint_per_sequence) {

    is_tf <- dotify.matrix(aas_aln, aas_aln[tf_index, ])

#    if (is.null(multiplicity)) {
    tf_loss <- compute.tf.loss(is_tf, n_seqs, timepoint_per_sequence)
#    } else {
#	tf_loss <- weighted.tf.loss(is_tf, n_seqs, multiplicity)
#    }

    tf_loss
}
