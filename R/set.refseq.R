#' @keywords internal
set.refseq <- function(S) {

    if (class(S) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to prep.aln()")

# 01142016
# CONSIDER INTERACTIVE USE - will changing alignments with different
# refseq_name get updated here?

    if (is.null(S$refseq_row) & !is.null(S$refseq_name) & 
        S$refseq_name %in% rownames(S$aas_aln))
        S$refseq_row <- get.refseq.row(S$aas_aln, S$refseq_name)

    ### create lut and excise refseq from alignment if it's in there
    if (!is.null(S$refseq_row)) {

        S <- create.refseq.lut(S, S$aas_aln[S$refseq_row, ])

        ### HERE WE EXCISE REFSEQ from ALIGNMENT
        if (!is.null(S$refseq_name) & 
            S$refseq_name == rownames(S$aas_aln)[S$refseq_row])
            S$aas_aln <- excise.refseq(S$refseq_row, S$aas_aln)

    } else if (!is.null(S$tf_index)) { 
        # & is.null(S$refseq_name)
        # this was never executed with HXB2 default
        S <- create.refseq.lut(S, S$aas_aln[S$tf_index, ])
        S$refseq_name <- rownames(S$aas_aln)[S$tf_index]
    }

    # more caution here seems needed so as not to delete rows by
    # mistake or otherwise corrupt alignment

        # hypothetical problem: if tf_index > refseq_row, adjust it or not??
# if (tf_index == refseq_row)
#    stop("ERROR: not sure what to do with tf_index after removing reference sequence from the alignment")
### assume it's fine to proceed - the default value of 1 will be in the correct place now.
### so HXB2 then TF will proceed normally

    S
}
