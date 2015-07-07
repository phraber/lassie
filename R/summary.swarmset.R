#' List names of sequences in working_swarm of swarmset.
#'
#' @param object swarmset object
#' @param ... not currently in use but should be soon
#' #' @seealso \code{\link{swarmset}}
#' @family swarmset methods
#' @export
summary.swarmset <- function(object, ...) { 

    dots = list(...)

    cat(paste0("Selected n=", 
	length(which(object$working_swarm$is_included)), 
	    " sequences:\n"))

# TO DO: format sequence name output, one row per sample timepoint

    ins <- which(object$working_swarm$is_included)
    Ts <- sort(unique(object$working_swarm$seq_times[ins]))
    in.names <- names(object$working_swarm$is_included[ins])

    for (my.time in Ts)
	cat(#paste0(my.time, ": ", ),
	    paste(in.names[which(grepl(my.time, in.names))], 
	    collapse=', '), '\n')

#    cat(paste(names(object$working_swarm$is_included[ins]), collapse='\n'), '\n')

    n.remaining = which(c(object$working_swarm$variant_counts) > 0)

    message(paste("Covered", 
        object$working_swarm$initial_n_variants-length(n.remaining), "of", 
	object$working_swarm$initial_n_variants, "initial mutations."))

    if (length(n.remaining > 0)) {

        message(paste0("Number of mutations to be represented is now ", 
	    length(which(c(object$working_swarm$variant_counts) > 0)),":"))

        pos.counts <- apply(object$working_swarm$variant_counts, 2, max)
	for (i in which(pos.counts > 0)) {

            message(paste0("  column", sprintf("%3s", i), ":", 
		sprintf("%5s", names(pos.counts)[i]), "[", 
		paste(rownames(object$working_swarm$variant_counts)[which(object$working_swarm$variant_counts[, i] > 0)], collapse=""), "]"))

	}
    }
}
