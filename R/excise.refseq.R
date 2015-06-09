#' @keywords internal
excise.refseq <- function(refseq_row, aas_aln) {

    # return alignment wihthout aas_aln[refseq_row, ] has one less row

    aas_aln[-refseq_row, ]
    ## need to update tf index if already defined
}
