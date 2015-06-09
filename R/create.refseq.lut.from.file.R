#' @keywords internal
create.refseq.lut.from.file <- function(refseq_lut_file=NULL) {
### NOT DONE
    if (!is.null(refseq_lut_file)) {
        # TO DO: make refseq lut optional
	if (!file.exists(refseq_lut_file)) {
 	    warning(paste0("ERROR in create.refseq.lut(): refseq_lut_file '", 
		refseq_lut_file, "' does not exist.  To fix this please specify a reference sequence or specify a file that contains a lookup-table." ))
	} else {
	    my_refseq_lut <- read.table(refseq_lut_file, 
		col.names=c('aln', 'l', 'r', 'aa'))
        }
    }
     my_refseq_lut
}
