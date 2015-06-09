#' Print a table of the selected sites.
#'
#' @param x Swarmtools object
#'
#' @export
print.swarmtools <- function(x) {

    if (class(x) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to print.swarmtools()")

    if (length(x$tf_loss_cutoff) > 1) {
	cat(paste(summary(x), collapse='\n'))
    } else {
        # if tf_loss_cutoff is a vector: 
        cat(paste0("Loss cutoff = ", x$tf_loss_cutoff, "%.\n"))
        cat(paste0("Selected ", nrow(x$selected_sites), " sites:\n"))
	print(x$selected_sites, quote=F)
    }
}

