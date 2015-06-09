#' @keywords internal
hamming.dist = function(a, b) {

    if (nchar(a) != nchar(b)) 
	stop("ERROR: unequal sequence lengths in hamming.dist()")

# assert is.character(a) and is.character(b)

# need to consider NA?
    length(which(seqinr::s2c(a) != seqinr::s2c(b)))
}
