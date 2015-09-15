#' @keywords internal
dotify.matrix <- function(aln_mat, tfseq_vector, as_logical=TRUE) {

    ## compare each sequence to tf (T, F, NA)
    if ( length(tfseq_vector) != ncol(aln_mat) )
	stop("ERROR in dotify.matrix(): TF sequence length does not equal the number of columns in alignment_matrix.  To fix this please ensure the TF sequence is the same width as the alignment you provided.")

    if (as_logical) {

	out_matrix <- matrix(NA, nrow=nrow(aln_mat), ncol=ncol(aln_mat))

	for (i in 1:ncol(aln_mat)) {

            out_matrix[which(aln_mat[ ,i] != tfseq_vector[i]), i] <- F
            out_matrix[which(aln_mat[ ,i] == tfseq_vector[i]), i] <- T
	    out_matrix[which(aln_mat[ ,i] %in% c("X", "Z", "*", "#") &&
 # CONSTANTS - consider using a named vector for non-aa symbols
                    aln_mat[ ,i] != tfseq_vector[i]), i] <- NA
	}

	rownames(out_matrix) <- rownames(aln_mat)
#	colnames(out_matrix) <- colnames(aln_mat)
# NB: this changes concatemerized column names; 
#     use as_logical=F for dot_concatamer 

	colnames(out_matrix) <- sapply(1:ncol(aln_mat), function(i) 
	    paste0(tfseq_vector[i], i))

    } else {

	out_matrix = aln_mat

	for (i in 1:ncol(aln_mat))
            out_matrix[which(aln_mat[ ,i] == tfseq_vector[i]), i] = "."
    }    

    out_matrix
}

