#' Update parser function, which extracts timepoint labels from sequence names.
#'
#' @param S swarmtools object
#' @param f timepoint parser function, created by create.timepoint.parser().
#' @return swarmtools object with updated parser.  If an alignment file is defined, it will be reparsed.
#' @seealso \code{\link{create.timepoint.parser}}
#' @export
set.tp.parser <- function(S, f) {

    if (class(S) != "swarmtools")
        stop("ERROR in set.tp.parser(): Please provide a swarmtools object")

    if (class(f) != "function")
        stop("ERROR in set.tp.parser(): Please provide a parsing function")

    S$timepoints_parser <- f

    ## trigger reparsing of input if defined
#    if (!is.null(S$aas_file))
#        S <- prep.aln(S)

    return ( S )
}
