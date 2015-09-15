#' @keywords internal
compute.hd.matrix <- function(seqs_a, seqs_b) {

    # distance matrix is not square & symmetric but rather a rows x b columns
    hds <- matrix(NA, nrow=length(seqs_a), ncol=length(seqs_b))
    rownames(hds) <- names(seqs_a)
    colnames(hds) <- names(seqs_b)

    for (a in 1:length(seqs_a))
        for (b in 1:length(seqs_b))
            hds[a, b] <- hamming.dist(seqs_a[a], seqs_b[b])

    hds
}
