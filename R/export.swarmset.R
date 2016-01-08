#' Output full-length sequence text for all selected sequences in swarmset.
#'
#' @param x swarmset
#' @param ... not currently used but could and should be soon
#' @seealso \code{\link{swarmset}}
#'
#' @family swarmset methods
#'
#' @export
export.swarmset <- function(x) { 

    if (class(x) != "swarmset")
	stop("ERROR: please pass as swarmset object to export.swarmset()")

    ins <- which(x$working_swarm$is_included == T)
#    seqname <- names(x$working_swarm$seq_allcolumns)[ins]
    swarmset = x$working_swarm$seq_allcolumns[ins]

    return (data.frame(swarmset))

#    message(paste0("Number of mutations to be represented was initially ", 
#	x$working_swarm$initial_n_variants, "."))

#    if (length(which(c(x$working_swarm$variant_counts) > 0)) == 0) {
#        message("Number of mutations to be represented is now 0.") 
#    } else {
#	message("Missed:")
#        pos.counts <- apply(x$working_swarm$variant_counts, 2, max)
#	r=1

#	for (i in which(pos.counts > 0)) {
#            for (j in which(x$working_swarm$variant_counts[, i] > 0)) {
#                message(paste(r, "\t", 
#		    paste(colnames(x$working_swarm$dot_concatamer)[i],
#		        rownames(x$working_swarm$variant_counts)[j], sep='')))
#	        r=r+1
#            }
#        }
#    }
}
