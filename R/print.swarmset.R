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


    message(paste0("Number of mutations to be represented was initially ", 
	x$working_swarm$initial_n_variants, "."))

    if (length(which(c(x$working_swarm$variant_counts) > 0)) == 0) {
        message("Number of mutations to be represented is now 0.") 
    } else {
	message("Missed:")
        pos.counts <- apply(x$working_swarm$variant_counts, 2, max)
	r=1

	for (i in which(pos.counts > 0)) {
            for (j in which(x$working_swarm$variant_counts[, i] > 0)) {
                message(paste(r, "\t", 
		    paste(colnames(x$working_swarm$dot_concatamer)[i],
		        rownames(x$working_swarm$variant_counts)[j], sep='')))
	        r=r+1
            }
        }
    }
}
