#' Defines alignment file but does not read it.  After setting it, call prep.aln.
#'
#' Reading the alignment file is done by prep.aln for control of excise.refseq status.
#'
#' @param S swarmtools object
#' @param aln_file alignment file
#' @param alignment_format Alignment format
#' @keywords internal
set.alignment <- function(S, a, f) {

    ### NB: May need to sanitize S and f

    if (class(S) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to set.alignment.file()")

#    if (!is.null(f)) {

    if (is.null(a) & !is.null(f))
	if (!file.exists(f))
            stop(paste0("ERROR: Specified file '", f, "' does not exist."))

    if (!is.null(a)) { 
	if (!is.null(S$aas_aln))
	    if (a != S$aas_aln)
		warning("WARNING: Changing alignment.\n")

	S$aas_aln <- a

    } else if (!is.null(f)) {
	if (!is.null(S$aas_file))
	    if (f != S$aas_file)
		warning("WARNING: Changing alignment.\n")

	S$aas_file = f
    }

    S <- prep.aln(S)

    S
}
