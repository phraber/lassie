#' List names of sequences in working_swarm of swarmset.
#' @param S swarmset
#' @seealso \code{\link{swarmset.default}}
#' @family swarmset methods
#' @export
summary.swarmset <- function(S) { 

    cat(paste0("Selected n=", 
	length(which(S$working_swarm$is_included)), 
	    " sequences:\n"))

# TO DO: format sequence name output, one row per sample timepoint

    ins <- which(S$working_swarm$is_included)
    Ts <- sort(unique(S$working_swarm$seq_times[ins]))
    in.names <- names(S$working_swarm$is_included[ins])

    for (my.time in Ts)
	cat(#paste0(my.time, ": ", ),
	    paste(in.names[which(grepl(my.time, in.names))], 
	    collapse=', '), '\n')

#    cat(paste(names(S$working_swarm$is_included[ins]), collapse='\n'), '\n')
}
