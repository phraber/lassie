#' @keywords internal
add.clone <- function(new_clone, WS, criterion=NULL, is_verbose) {

    if (length(new_clone) > 1) #{
	stop(paste("ERROR in add.clone()! multiple new_clones passed:",
		paste(new_clone, collapse=',')))

    if (is.null(new_clone))
	stop(paste("ERROR in add.clone(): new_clone index is null for criterion:", criterion))

    if (is.integer(new_clone) & 
	(new_clone < 1 | new_clone > nrow(WS$dot_concatamer)))
	    stop(paste("ERROR in add.clone(): new_clone index out of bounds:", 
		new_clone))

    if (is.character(new_clone) & 
	(!new_clone %in% rownames(WS$dot_concatamer)))
	    stop(paste("ERROR in add.clone(): new_clone name not among sequence names:", new_clone))

    # only if it hasn't been added already...
    if (!WS$is_included[new_clone]) {
    	    
    # need to print undotified form
        if (criterion=="TF") {

	    if.verbose.print(paste("   ", criterion, 
	    sprintf("%-12s", rownames(WS$aln_concatamer)[new_clone]), 
	        '\t', seqinr::c2s(WS$aln_concatamer[new_clone, ])), is_verbose)
        } else {

	    if.verbose.print(paste("   ", criterion, sprintf("%-12s", 
		new_clone),
		'\t', seqinr::c2s(WS$dot_concatamer[new_clone, ])), is_verbose)
        }

# TO DO: validate new_clone argument - is new_clone_index a name or an integer?

	WS <- update.variant.counts(new_clone, WS)
	WS$is_included[new_clone] <- T
    }

#    if (omit_singletons) #{
    if.verbose.print(paste0("Number of mutations to be represented is now ", 
	length(which(c(WS$variant_counts) > 0))), is_verbose)

    WS
}

