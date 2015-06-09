#' @keywords internal
make.swarm.logos <- function(SST) {

    if (class(SST) != "swarmset")
	stop("ERROR in make.swarm.logos(): Invalid swarmset object")

#   if (is.null(filename_prefix)) 
#      filename_prefix = paste0(ST$results_prefix, "-logos-", stratify, dotify)

    make.logoplot(SST$selected_sites, SST$working_swarm, SST$tf_index, 
	paste0(SST$results_prefix, "-TF"), dotify=F, aspect_ratio=1)

    make.logoplot(SST$selected_sites, SST$working_swarm, 
	which(SST$working_swarm$is_included), 
	paste0(SST$results_prefix, "-swarm"), dotify=F, 
	aspect_ratio=3)

    make.logoplot(SST$selected_sites, SST$working_swarm, 
	which(SST$working_swarm$is_included), 
	paste0(SST$results_prefix, "-dotswarm"), dotify=T, aspect_ratio=3)
}
