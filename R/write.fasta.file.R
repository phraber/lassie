#' @keywords internal
write.fasta.file <- function(filename, selected_sequences) {#, multiplicity) {

    if (file.exists(filename))
	file.remove(filename)

    outcome = file.create(filename)

    for (i in 1:length(selected_sequences))
	cat(paste0(">", names(selected_sequences)[i], '\n',
	    selected_sequences[i], '\n'), file=filename, append=T)

# to do: fix this!
#    seqinr::write.fasta(toupper(selected_sequences), names(selected_sequences), filename, as.string=T)
}
