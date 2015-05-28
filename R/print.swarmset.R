#' List concatamers of selected sites for all working_swarm sequences in swarmset.
#' @param S swarmset
#' @seealso \code{\link{swarmset.default}}
#' @family swarmset methods
#' @export
print.swarmset <- function(S) { 

# output the TF concatamer sequence first
    i <- min(which(S$working_swarm$is_included == T))
    if (!is.null(S$tf_index) & S$working_swarm$is_included[S$tf_index] & 
	    S$tf_index != i)
	i <- S$tf_index

    cat(paste(names(S$working_swarm$seq_concatamer[i]), 
	S$working_swarm$seq_concatamer[i], sep='\t'), '\n')

    ins <- which(S$working_swarm$is_included == T)
    for (i in 2:length(ins))
	cat(paste(names(S$working_swarm$dotseq_concatamer[ins[i]]), 
	    S$working_swarm$dotseq_concatamer[ins[i]], sep='\t'), '\n')
}
