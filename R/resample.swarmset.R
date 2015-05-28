#' Randomly sample as many sequences as are in the given working swarmset, using the same inclusion/exclusion constraints, for a fair comparison.
#'
#' @param SST swarmset object
#' @param n_replicates number of replicates
#' @family swarmset methods
#' @seealso \code{\link{swarmset.default}}
#' @export
resample.swarmset <- function(SST, n_replicates=100) {

    is_included <- init.clone.list(SST$aln_allcolumns)

# user-excluded clones are marked as !is_viable

    swarm_size <- length(which(SST$working_swarm$is_included))

    included_seqs <- unique(SST$working_swarm$seq_allcolumns[
	    which(SST$is_also_included)])

    # for fair comparison, normalize to eliminate duplicate concatamers
    distinct_sequences <- unique(SST$working_swarm$seq_allcolumns[
	    which(SST$working_swarm$is_viable & 
		!SST$working_swarm$seq_allcolumns %in% included_seqs)])

    pool_for_resampling <- sort(sapply(1:length(distinct_sequences), 
	function(i) min(which(SST$working_swarm$is_viable & 
	    SST$working_swarm$seq_allcolumns == 
	        distinct_sequences[i]))))

    if (swarm_size - length(which(SST$is_also_included)) > 
	    length(pool_for_resampling))
	stop("resample.swarmset() ERROR: pool for resampling is too small")

    resampled_set <- sort(as.numeric(c(which(SST$is_also_included), 
	sample(pool_for_resampling, 
	    swarm_size - length(which(SST$is_also_included))))))

#    is_included[resampled_set] = T
# this gets set by add.clone()

#    if (length(which(SST$is_also_included)) > 0)
#	is_included[which(SST$is_also_included)] = T

# sh/could test here for errors
#    working_swarm <- add.clone(tf_index, working_swarm, "TF", is_verbose)
    # copied from above

    aa_alphabet <- sort(unique(c(SST$working_swarm$dot_concatamer)))

    # initialize counts
    variant_table <- tabulate.variants(SST$working_swarm$dot_concatamer,
	sequence_multiplicity=SST$working_swarm$sequence_multiplicity,
	omit_singletons=T, 
	min_counts=SST$min_counts,
	aa_alphabet=aa_alphabet, #SST$working_swarm$aa_alphabet, 
	is_verbose=F)

    random_swarm <- list ( is_included = is_included,
                     dot_concatamer = SST$working_swarm$dot_concatamer,
                     seq_concatamer = SST$working_swarm$seq_concatamer,
                     dotseq_concatamer = SST$working_swarm$dotseq_concatamer,
                     aln_concatamer = SST$working_swarm$aln_concatamer,
                     aln_allcolumns = SST$working_swarm$aln_allcolumns,
		     seq_allcolumns = SST$working_swarm$seq_allcolumns,
		     seq_times = SST$working_swarm$seq_times,
		     is_viable = SST$working_swarm$is_viable,
                     variant_counts = variant_table,
                     initial_variant_counts = variant_table,
		     initial_n_variants = 0)

#    force_includes <- which(SST$is_also_included)

#    if (length(force_includes) > 0)
#	for (i in 1:force_includes)
#	    random_swarm <- add.clone(force_includes[i], random_swarm, "a", F)

    for (i in 1:length(resampled_set))
	random_swarm <- add.clone(resampled_set[i], random_swarm, "b", F)

    random_swarm$initial_n_variants = 
        length(which(c(random_swarm$variant_counts) > 0))

# length(which(as.logical(c(eg.swarmset$working_swarm$initial_variant_counts))
# & !as.logical(c(eg.swarmset$working_swarm$variant_counts))))

# length(which(as.logical(c(eg.swarmset$working_swarm$initial_variant_counts)) 
# & !as.logical(c(foo$variant_counts)))

    return (random_swarm)
}
