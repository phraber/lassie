#' @keywords internal
parse.timepoints <- function(seq_names=NULL, uniquify=T, timepoints_parser) {

### THIS WILL FAIL IF THE REFSEQ IS still in the alignment

    if (!is.null(timepoints_parser)) {

        # NB: accepting a user-defined function could be a security hole;
        # to do: wrap the closure within the swarmtools namespace?

	if (!is.function(timepoints_parser))
	    stop("ERROR in parse.timepoints(): timepoints_parser is not a function")

	timepoint_per_sequence <- timepoints_parser(seq_names, do.tests=T)
    }

    if (any(is.null(timepoint_per_sequence)))
	paste("ERROR in parse.timepoints(): Some names not parsed\n",
	    paste(seq_names[which(is.null(timepoint_per_sequence))], 
		collapse=','), '\n')

    if (uniquify) {
	return ( as.character(sort(
#		    as.numeric(
			unique(#gsub("^[A-Za-z]", "", 
	    timepoint_per_sequence))))#) #)
    } else {
	return ( #gsub("^[A-Za-z]", "", 
	    timepoint_per_sequence)# )
    }
}
