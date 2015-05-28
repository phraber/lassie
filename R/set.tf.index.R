#' @keywords internal
set.tf.index <- function(S) {

    if (class(S) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to set.tf.index()")

    # This is all needlessly touchy if tf_index=1 is the default, due to confusion about refseq order

    if (is.null(S$tf_index) & is.null(S$tf_name)) {

	if (length(which(grepl("TF", S$original_seqnames))) == 1) {

	    S$tf_index = which(grepl("TF", S$original_seqnames))
	    S$tf_name = S$original_seqnames[which(grepl("TF", S$original_seqnames))]
	} else {

	    if (length(grepl("TF", rownames(S$aas_aln))) == 1) {
	        S$tf_index = which(rownames(S$aas_aln) == S$tf_name)
            } else {
	        stop("ERROR: TF undefined.  To fix this, please specify its line number or name, or rename it to contain 'TF'.")
	    }
	}
    }

    if (is.null(S$tf_index) & !is.null(S$tf_name)) {

	if (S$tf_name %in% rownames(S$aas_aln)) {
	    S$tf_index = which(rownames(S$aas_aln)==S$tf_name)
	} else if (S$tf_name %in% S$original_seqnames) {
	    S$tf_index = which(S$original_seqnames==S$tf_name)
	} else {
	    stop(paste0("ERROR: TF name '", S$tf_name, "'not found among sequence names.  To fix this, please change the name you provided."))
	}
    } else if (!is.null(S$tf_index) & is.null(S$tf_name)) {
	S$tf_name = rownames(S$aas_aln)[S$tf_index]
    }

    if (!is.null(S$tf_index) & !is.null(S$tf_name))
	if (S$tf_name != rownames(S$aas_aln)[S$tf_index] & 
	    S$tf_name != original_seqnames[S$tf_index])
	    stop("ERROR: TF name is inconsistent with TF index.  To fix this, please make them consistent or specify only one value.")

    if (!is.null(S$tf_index)) {

        ## rename columns to indicate ancestral aa
        colnames(S$aas_aln) <- sapply(1:ncol(S$aas_aln), function(i)
	    paste0(S$aas_aln[S$tf_index, i], i))

        ### go ahead and dotify matrix, compute tf loss, per timepoint, etc.
	S$is_tf <- dotify.matrix(S$aas_aln, S$aas_aln[S$tf_index, ]) 
	S$n_per_timepoint <- table(S$timepoint_per_sequence)
	S$tf_loss <- create.tf.loss.matrix(S$aas_aln, S$tf_index, S$n_per_timepoint)
	S$peak_tf_loss <- compute.peak.tf.loss(S$tf_loss)
    }

    S
}
