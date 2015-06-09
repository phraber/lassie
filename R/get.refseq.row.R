#' @keywords internal
get.refseq.row <- function(aas_aln, refseq_name, ignore_case=F) {

    if (is.null(refseq_name))
	return(NULL)

    name.hits <- grepl(refseq_name, rownames(aas_aln), ignore.case=ignore_case)
    n.matches <- length(which(name.hits))

    if (n.matches == 1) {

	return(which(name.hits))
    } else if (n.matches == 0) {

	if (ignore_case) { # prevent infinite regress
	    return(NULL)
	} else {
	    get.refseq.row(aas_aln, refseq_name, ignore_case=T)
	}

    } else {
	stop("lassie::get.refseq.row() ERROR: refseq_name found more than once in alignment.  To fix this, delete the extra entries.")
    }
}

