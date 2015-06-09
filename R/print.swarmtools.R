### LICENSING INFO GOES HERE

#' Print a table of the selected sites.
#'
#' @param Swarmtools object
#' @export
print.swarmtools <- function(S) {

    if (class(S) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to print.swarmtools()")

    if (length(S$tf_loss_cutoff) > 1) {
	cat(paste(summary(S), collapse='\n'))
    } else {
        # if tf_loss_cutoff is a vector: 
        cat(paste0("Loss cutoff = ", S$tf_loss_cutoff, "%.\n"))
        cat(paste0("Selected ", nrow(S$selected_sites), " sites:\n"))
	print(S$selected_sites, quote=F)
    }
}

