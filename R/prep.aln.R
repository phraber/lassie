#' Prepare input data for analysis after defining refseq, running read.alignment.file and set.tf if needed
#'
#' @keywords internal
prep.aln <- function(S) {

    if (class(S) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to prep.aln()")

#    if (!is.null(S$aas_file))
    if (is.null(S$aas_aln) & !is.null(S$aas_file))
	if (file.exists(S$aas_file))
	    S$aas_aln <- read.alignment.file(S$aas_file, S$aas_prefix, S$alignment_format)

    ### NB: refseq is used to standardize site numbering (e.g. HXB2), 
      # not for TF loss, which is specified by tf_index or tf_name
    if (!is.null(S$refseq_lut_file) & is.null(S$refseq_lut)) {
	S$refseq_lut = create.refseq.lut.from.file(S$refseq_lut_file)
    } else if (is.null(S$refseq_lut)) {
#    } else if (!is.null(S$refseq_name) & is.null(S$refseq_lut)) {
	S <- set.refseq(S)
    }

#    if (!is.null(S$refseq_name) & is.null(S$refseq_row))
#	S <- set.refseq(S)

    if (!is.null(S$aas_aln)) {

 	S$original_seqnames <- rownames(S$aas_aln)

	S$timepoint_per_sequence <- parse.timepoints(rownames(S$aas_aln),
	    uniquify=F, timepoints_parser=S$timepoints_parser)

#	if (!is.null(S$timepoint_per_sequence)) {
#	    timepoint_order <- order(as.numeric(gsub("^[A-Za-z]", "", 
#	                       unique(S$timepoint_per_sequence))))

#	    if (!is.null(timepoint_order)) {
	        
	        S$n_per_timepoint <- table(sort(S$timepoint_per_sequence))

	        ## order n_per_timepoint by numeric value/s --- NEED TO TEST
#	        S$n_per_timepont = n_per_timepoint[timepoint_order]
 #   	    names(S$n_per_timepoint) = names(n_per_timepoint)[timepoint_order]
#	    }
	#}

	S <- set.tf(S)
    }

    S
}
