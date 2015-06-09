#' @keywords internal
update.variant.counts <- function(new_clone, WS) {

    if (length(new_clone) > 1)
	stop("ERROR in update.variant.counts(): multiple new_clones passed")
#	concatamer_id = concatamer_id[1]
#    } # stop here?  or iterate over arguments?

    for (s in 1:ncol(WS$dot_concatamer))
        WS$variant_counts[WS$dot_concatamer[new_clone, s], s] = 0

    WS
}
