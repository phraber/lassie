#' Summarize selected sites.
#'
#' @param object Swarmtools object
#' @param ... not currently in use but should be soon
#'
#' @return A data frame of selected sites, with one row per ?
#'
#' @export
summary.swarmtools <- function(object, ...) {

# TO DO: if tf_loss_cutoff is a scalar, list more info than if it is a vector

# alignment contains sequences and sites
# summarize tf loss distribution?
#summary(S$peak_tf_loss)

    dots <- list(...)

    if (class(object) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to summary.swarmtools()")

    if (is.null(object$selected_sites)) {
        object$selected_sites <- NA
        my_summary <- data.frame(object$tf_loss_cutoff, NA, NA)
    } else {
        my_summary <- data.frame(object$tf_loss_cutoff, 
	    nrow(object$selected_sites),
	    round(100*nrow(object$selected_sites)/ncol(object$aas_aln), 2))
    }
#    rownames(my_summary) <- paste("TF Loss >=", object$tf_loss_cutoff, "%")
    colnames(my_summary) <-c("%TF Loss at least", "selected sites, n", 
	"selected sites, %")

    my_summary
}
