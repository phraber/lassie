#' @keywords internal
write.fasta.file <- function(filename, selected_sequences) {#, multiplicity) {

    # not the most elegant, but should work

    # \n works on other platforms or is a linesep?
#    sink (file=filename)
    if (file.exists(filename))
	file.remove(filename)

    outcome = file.create(filename)

#    if (!outcome)
#	warn()
    for (i in 1:length(selected_sequences))
	cat(paste0(">", names(selected_sequences)[i], '\n',
	    selected_sequences[i], '\n'), file=filename, append=T)
}
