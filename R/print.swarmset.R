#' List concatamers of selected sites for all working_swarm sequences in swarmset.
#'
#' @param x swarmset
#' @param ... not currently used by could and should be soon
#' @seealso \code{\link{swarmset}}
#'
#' @family swarmset methods
#'
#' @export
print.swarmset <- function(x, ...) { 

    dots <- list(...)
    
    # output the TF concatamer sequence first
    i <- min(which(x$working_swarm$is_included == T))
    if (!is.null(x$tf_index) & x$working_swarm$is_included[x$tf_index] & 
	    x$tf_index != i)
	i <- x$tf_index

    cat(paste(names(x$working_swarm$seq_concatamer[i]), 
	x$working_swarm$seq_concatamer[i], sep='\t'), '\n')

    ins <- which(x$working_swarm$is_included == T)
    for (i in 2:length(ins))
	cat(paste(names(x$working_swarm$dotseq_concatamer[ins[i]]), 
	    x$working_swarm$dotseq_concatamer[ins[i]], sep='\t'), '\n')
}
