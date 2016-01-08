#' List concatamers of selected sites for all working_swarm sequences in swarmset.
#'
#' @param x swarmset
#' @param ... not currently used by could and should be soon
#' @seealso \code{\link{swarmset}}
#'
#' @family swarmset methods
#'
#' @export
tabulate.swarmset <- function(x) { 

    if (class(x) != "swarmset")
	stop("ERROR: please pass as swarmset object to tabulate.swarmset()")
    

    ins <- which(x$working_swarm$is_included == T)

#    name = names(x$working_swarm$dotseq_concatamer)[ins]
    concatamer = x$working_swarm$dotseq_concatamer[ins]

    # output the full TF concatamer sequence
    i <- min(which(x$working_swarm$is_included == T))
    if (!is.null(x$tf_index) & x$working_swarm$is_included[x$tf_index] & 
	    x$tf_index != i)
	i <- x$tf_index

    concatamer[names(x$working_swarm$dotseq_concatamer)[i]] = seqinr::c2s(x$working_swarm$aln_concatamer[i, ])

    return(data.frame(concatamer))

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
