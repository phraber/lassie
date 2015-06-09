#' Defines alignment file but does not read it, then call prep.aln.
#'
#' Reading the alignment file is done by prep.aln for control of excise.refseq status.
#'
#' @param S swarmtools object
#' @param f alignment file
#' @param alignment_format Format of alignment file/s; must be one of these: \code{"fasta"}, \code{"clustal"}, \code{"phylip"}, \code{"msf"}, or \code{"mase"}.
#' @return swarmtools object with defined aas_aln, refseq_lut, and tf_loss if possible
#' @export
set.alignment.file <- function(S, f, alignment_format="fasta") {

    ### NB: May need to sanitize S and f
    if (class(S) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to set.alignment.file()")

    if (is.null(f))
	return ( S )

    if (!file.exists(f))
        stop(paste0("ERROR: Specified file '", f, "' does not exist."))

    S$alignment_format = alignment_format

    if (is.null(S$aas_file)) {

	S$aas_file = f
        S <- prep.aln(S)

    } else if (f != S$aas_file) {

	if (!is.null(S$aas_aln))
	    warning("WARNING: Changing alignment.\n")

	S$aas_file = f
        S <- prep.aln(S)
    }
    # if the alignment file has already been set, the above logic
    # sames time by not repeating prep.aln needlessly
    return ( S )
}
