#' @keywords internal
create.refseq.lut <- function(S, aa) {

    if (class(S) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to create.refseq.lut()")

    aln <- c(1:length(aa))

    l <- rep(NA, length(aa))
    r <- rep(NA, length(aa))

    L <- 0
    R <- 1

    # there must be an efficient alternative!
    for (i in 1:length(aa)) {

	if (aa[i] != "-") L = L + 1

	l[i] = L
	r[i] = R

	if (aa[i] != "-") R = R + 1
    }

    S$refseq_lut <- data.frame(aln, l, r, aa)

    return ( S )
}
