#' @keywords internal
list.available.variants <- function(working_swarm, site) {

    variant_list <- names(sort(which(working_swarm$variant_counts[, site] > 0),
	decreasing=T))

    if (is.null(variant_list) | length(variant_list) == 0) return ( NULL )

    message(paste0("  column", 
	sprintf("%3s", which(colnames(working_swarm$dot_concatamer) == site)),
            ":", sprintf("%5s", site), "[", seqinr::c2s(variant_list), "]"))

    return ( variant_list )
}
