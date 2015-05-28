#' @keywords internal
write.lut.file <- function(refseq_vector) {

    lut.file <- tempfile()
    l=0
    r=1

    sink(file=lut.file)

    for (i in 1:length(refseq_vector)) {

	if (refseq_vector[i] != "-") # to do: define and use a gapchar variable
	    l = l + 1

	cat(paste(i, l, r, refseq_vector[i], sep='\t'), "\n")

	if (refseq_vector[i] != "-") # to do: define and use a gapchar variable
	    r = r + 1
    }

    sink(file=NULL)

    lut.file
}
