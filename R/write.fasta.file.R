#' @keywords internal
write.fasta.file <- function(filename, selected_sequences, multiplicity) {

    # not the most elegant, but should work

    # \n works on other platforms or is a linesep?
#    sink (file=filename)

    for (i in 1:length(selected_sequences)) {

	if (is.null(multiplicity) | length(multiplicity) == 0) {

	    cat(paste0(">", names(selected_sequences)[i], '\n',
	        selected_sequences[i], '\n'), file=filename, append=T)

	} else {

	    for (j in 1:multiplicity[i])
		cat(paste0(">", names(selected_sequences)[i], '\n', 
		    selected_sequences[i], '\n'), file=filename, append=T)
	}
    }

#    sink (file=NULL)
}
