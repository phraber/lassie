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
        if (!is.null(S$aas_aln)) {
            if (a != S$aas_aln) {

# pth - 01142016
#                warning("WARNING: Changing alignment.\n")

                S$refseq_lut = NULL
#                S$refseq_name = NULL
            }
        }

        S$aas_aln <- a

    } else if (!is.null(f)) {
        if (!is.null(S$aas_file)) {
            if (f != S$aas_file) {

# pth - 01142016
#                warning("WARNING: Changing alignment.\n")
                S$refseq_lut = NULL
#                S$refseq_name = NULL
            }
        }

        S$aas_file = f
    }

    return ( prep.aln(S) )
}
