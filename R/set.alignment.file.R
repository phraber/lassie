#' Defines alignment file but does not read it, then call prep.aln.
#'
#' Reading the alignment file is done by prep.aln for control of excise.refseq status.
#'
#' @param S swarmtools object
#' @param f alignment file
#' @param pngs2o option to indicate whether or not to mark PNG Ns as O.
#' @param alignment_format Format of alignment file/s; must be one of these: \code{"fasta"}, \code{"clustal"}, \code{"phylip"}, \code{"msf"}, or \code{"mase"}.
#' @return swarmtools object with defined aas_aln, refseq_lut, and tf_loss if possible
#' @export
set.alignment.file <- function(S, f, pngs2o=NULL, 
                               alignment_format="fasta") {

    ### NB: May need to sanitize S and f
    if (class(S) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to set.alignment.file()")

    if (is.null(f))
	return ( S )

    if (!file.exists(f))
        stop(paste0("ERROR: Specified file '", f, "' does not exist."))

    S$aas_file = f

    if (!alignment_format %in% c("fasta", "clustal", "phylip", "msf", "mase"))
        stop("ERROR: Unsupported file format.")

    S$alignment_format = alignment_format

    if (!is.null(pngs2o))
	if (is.logical(pngs2o))
	    S$pngs2o = pngs2o

    if (f != S$aas_file) {
	S$aas_aln = NULL
	S$refseq_lutd_aln = NULL
    }

    S <- prep.aln(S)

    return ( S )
}
