#' Prepare input data for analysis after defining refseq, running read.alignment.file and set.tf if needed
#'
#' @keywords internal
prep.aln <- function(S) {

    if (class(S) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to prep.aln()")

### only read the alignment file if aas_aln is null
#    if (is.null(S$aas_aln) & !is.null(S$aas_file)) {
    if (is.null(S$aas_file) & is.null(S$aas_aln))
	NULL

    if (!is.null(S$aas_file))
        if (file.exists(S$aas_file))
            S$aas_aln <- read.alignment.file(S$aas_file, S$alignment_format)

    if (!is.null(S$aas_aln) & S$pngs2o)
        S$aas_aln <- pngs2o(S$aas_aln)

    ### NB: refseq is used to standardize site numbering (e.g. HXB2), 
      # not for TF loss, which is specified by tf_index or tf_name

    if (is.null(S$refseq_lut)) {

        if (!is.null(S$refseq_lut_file))
            S$refseq_lut = create.refseq.lut.from.file(S$refseq_lut_file)
	else
            S <- set.refseq(S)
    }

#    if (!is.null(S$refseq_name) & is.null(S$refseq_row))
#        S <- set.refseq(S)

    if (!is.null(S$aas_aln)) {

         S$original_seqnames <- rownames(S$aas_aln)

# TODO: validate parsing timepoints before running it
# but timepont_per_sequence n_per_timepoint will be undefined
        S$timepoint_per_sequence <- rep(NA, nrow(S$aas_aln))
        S$n_per_timepoint <- NULL

        S$timepoint_per_sequence <- parse.timepoints(rownames(S$aas_aln),
            uniquify=F, timepoints_parser=S$timepoints_parser)

        if (!is.null(S$timepoint_per_sequence))
            S$n_per_timepoint <- table(sort(S$timepoint_per_sequence))

        S <- set.tf(S)
    }

    S
}
