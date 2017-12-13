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
        return ( NULL )
    else if (!file.exists(aas_file))
        stop(paste0("ERROR in read.alignment.file(): alignment file '", 
            aas_file, "' does not exist"))

    a = NULL
    try(a <- seqinr::read.alignment(aas_file, alignment_format, forceToLower=F))

    if (!is.null(a)) {

        a$seq <- sapply(1:length(a$seq), function(i) toupper(a$seq)[i])

        if (length(table(nchar(a$seq))) > 1) {

	    a.table <- sort(table(nchar(a$seq)), decreasing=T)

	    length.summary <- paste0("There are ", names(a.table)[1], 
		" sequences are of length ", a.table[1], 
		".  Please review these ", sum(a.table[-1]), 
		" sequences with different lengths: ")

	    if (sum(a.table[-1]) == 1) {
		length.summary <- paste0("There are ", names(a.table)[1], 
		    " sequences are of length ", a.table[1], 
		    ".  Please review this sequence with length ", 
		    names(a.table)[2], ": ")
	    } else if (length(a.table) == 2) {
		length.summary <- paste0("There are ", names(a.table)[1], 
		    " sequences are of length ", a.table[1], 
		    ".  Please review these sequences with length ", 
		    names(a.table)[2], ": ")
            }

	    other.seqnames <- paste(a$nam[which(nchar(a$seq) != 
		    names(a.table)[1])], collapse=",")

            stop(paste0("ERROR in read.alignment.file(): alignment file '", 
		    aas_file, "' contains sequences of different lengths.  ",
		    length.summary, other.seqnames))
	}
    }

    b = NULL
    if (!is.null(a))
        try(b <- seqinr::as.matrix.alignment(a))

    if (!is.null(b)) {

        if (is.list(b))
            stop(paste0("ERROR in read.alignment.file(): alignment file '", 
                aas_file, "' could not be converted to a character matrix."))

        if (any(!is.character(c(b))))
            stop(paste0("ERROR in read.alignment.file(): alignment file '", 
                aas_file, 
		"' contains elements that could not be treated as letters."))
    }
    return ( b )
    # need to respond in case of error / failure above?
}
