#' @keywords internal
exclude.vloop.sites <- function(S, site_list) {

    if (class(S) != "swarmtools")
        stop("ERROR: Unexpected status for arguments to exclude.vloop.sites()")

    # HXB2 sites for hypervariable loop regions; see Hraber et al. JVI 2014
    vloop.sites <- c(132:152, 185:190, 396:410, 460:465)

    S <- set.excluded.sites(S, 
        unique(c(S$refseq_lut$aln[which(S$refseq_lut$l %in% vloop.sites | 
                                        S$refseq_lut$r %in% vloop.sites )]),
               S$excluded_sites))

    return ( S )
}
