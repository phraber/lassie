#' @keywords internal
compute.peak.tf.loss <- function(tf_loss_matrix) {

    if (is.null(tf_loss_matrix) | 
        class(tf_loss_matrix) != "matrix" | 
        length(tf_loss_matrix) == 1)
        return ( NULL )

    peak_tf_loss <- apply(tf_loss_matrix, 1, max, na.rm=T)

    names(peak_tf_loss) <- rownames(tf_loss_matrix)

    return ( peak_tf_loss ) 
}
