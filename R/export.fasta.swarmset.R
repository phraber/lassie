#' Output fasta-formatted full-length sequence text for all selected sequences in swarmset.
#'
#' @param x swarmset
#' @param f Output file name.
#' @seealso \code{\link{swarmset}}
#'
#' @family swarmset methods
#'
#' @export
export.fasta.swarmset <- function(x, f) { 

    if (class(x) != "swarmset")
	stop("ERROR: please pass as swarmset object to export.fasta.swarmset()")

    if (!is.character(f))
	stop("ERROR: please provide an output file name to export.fasta.swarmset()")

    ins <- try(which(x$working_swarm$is_included))

    if (!is.null(ins)) {

        if (length(ins) >= 1) {

            s = x$working_swarm$seq_allcolumns[ins]

            if (length(s) >= 1)
		try(seqinr::write.fasta(s[1], names(s)[1], f, "w", 60))

            if (length(s) > 1)
	        for (i in 2:length(s))
		    try(seqinr::write.fasta(s[i], names(s)[i], f, "a", 60))
        }
    }
    return ( f )
}
