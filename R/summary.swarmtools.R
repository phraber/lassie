### LICENSING INFO GOES HERE

#' Summarize selected sites.
#'
#' @param Swarmtools object
#' @return A data frame of selected sites, with one row per ?
#' @export
summary.swarmtools <- function(S) {

# TO DO: if tf_loss_cutoff is a scalar, list more info than if it is a vector

# alignment contains sequences and sites
# summarize tf loss distribution?
#summary(S$peak_tf_loss)

    if (class(S) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to summary.swarmtools()")

    my_summary <- data.frame(S$tf_loss_cutoff, nrow(S$selected_sites), 
	round(100*nrow(S$selected_sites)/ncol(S$aas_aln), 2))

#    rownames(my_summary) <- paste("TF Loss >=", S$tf_loss_cutoff, "%")
    colnames(my_summary) <-c("%TF Loss at least", "selected sites, n", 
	"selected sites, %")

    my_summary
}
