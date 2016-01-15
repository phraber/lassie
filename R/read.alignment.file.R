#' Read a fasta-formatted alignment file
#'
#' @param aas_file File containing aligned protein sequences
#' @param alignment_format Format of alignment file/s; must be one of these: \code{fasta}, \code{clustal}, \code{phylip}, \code{msf}, or \code{mase}.
#' @inheritParams seqinr::read.alignment
#' @return A matrix that contains the alignment.
#' @examples
#' \dontrun{
#' aln <- read.alignment.file(system.file("extdata", "CH505-gp160.fasta", 
#'                                         package="lassie"))
#' }
#' @export
read.alignment.file <- function(aas_file=NULL, alignment_format="fasta") {

    if (is.null(aas_file))
        NULL
    else if (!file.exists(aas_file))
        stop(paste0("ERROR in read.alignment.file(): alignment file '", 
            aas_file, "' does not exist"))

    seqinr::as.matrix.alignment(seqinr::read.alignment(aas_file, 
	    alignment_format, forceToLower=F))

    # need to respond in case of error / failure above?
}
