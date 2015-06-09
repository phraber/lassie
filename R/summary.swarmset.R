#' List names of sequences in working_swarm of swarmset.
#'
#' @param object swarmset
#' @seealso \code{\link{swarmset}}
#' @family swarmset methods
#' @export
summary.swarmset <- function(object) { 

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
}
