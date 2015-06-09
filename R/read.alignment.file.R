#' Read a fasta-formatted alignment file
#'
#' @param aas_file File containing aligned protein sequences
#' @param aas_prefix Optional alternative to specify input alignment (see examples)
#' @param alignment_format Format of alignment file/s; must be one of these: \code{fasta}, \code{clustal}, \code{phylip}, \code{msf}, or \code{mase}.
#' @inheritParams seqinr::read.alignment
#' @return A matrix that contains the alignment.
#' @examples
#' \dontrun{
#' Equivalent filenames, because the default format is fasta.
#' aln1 <- read.alignment.file("foo.fasta")
#' aln2 <- read.alignment.file(aas_prefix="foo")
#' }
#' @export
read.alignment.file <- function(aas_file=NULL,
    aas_prefix=NULL, 
    alignment_format="fasta") {

    if (is.null(aas_file) & !is.null(aas_prefix))
	aas_file <- paste(aas_prefix, alignment_format, sep='.')

    if (!is.null(aas_file))
	if (!file.exists(aas_file))
	    stop(paste0("ERROR in read.alignment.file(): alignment file '", 
		aas_file, "' does not exist"))

    aas_seqinr <- seqinr::read.alignment(aas_file, alignment_format, forceToLower=F)

    aas_aln <- seqinr::as.matrix.alignment(aas_seqinr)

    aas_aln
}
