#' @keywords internal
dotify.matrix <- function(aln_mat, tfseq_vector, as_logical=TRUE) {

    ## compare each sequence to tf (T, F, NA)
    if (is.null(aln_mat))
        stop("ERROR in dotify.matrix(): aln_mat is null.")
    if (ncol(aln_mat) == 0)
        stop("ERROR in dotify.matrix(): aln_mat has 0 columns.")
    
    if (is.null(tfseq_vector))
	stop("ERROR in dotify.matrix(): tfseq_vector is null.")

    if (length(tfseq_vector) != ncol(aln_mat) )
	stop("ERROR in dotify.matrix(): TF sequence length does not equal the number of columns in alignment_matrix.  To fix this please ensure the TF sequence is the same width as the alignment you provided.")

    if (as_logical) {

	out_matrix <- matrix(NA, nrow=nrow(aln_mat), ncol=ncol(aln_mat))

	for (i in 1:ncol(aln_mat)) {

            try(out_matrix[which(aln_mat[ ,i] != tfseq_vector[i]), i] <- F)
            try(out_matrix[which(aln_mat[ ,i] == tfseq_vector[i]), i] <- T)
	    try(out_matrix[which(aln_mat[ ,i] %in% c("X", "Z", "*", "#") &
                    aln_mat[ ,i] != tfseq_vector[i]), i] <- NA)
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
	    if (any(aln_mat[, i] == tfseq_vector[i]))
                out_matrix[which(aln_mat[ , i] == tfseq_vector[i]), i] = "."
    }    

    out_matrix
}

