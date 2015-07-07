#' @keywords internal
select.clones <- function(aln_allcolumns, tf_index, aln_concatamer,
    is_included, is_also_included, is_excluded, min_counts, 
    sequence_multiplicity, timepoints_parser) {

    dot_concatamer <- dotify.matrix(aln_concatamer, 
	aln_concatamer[tf_index, ], as_logical=FALSE)

    hd_to_tf_concatamer <- sapply(1:nrow(dot_concatamer), function(i) 
	length(which(dot_concatamer[i,] != ".")) )
    names(hd_to_tf_concatamer) <- rownames(aln_concatamer)

    seq_allcolumns <- sapply(1:nrow(aln_allcolumns), function(i) 
	seqinr::c2s(aln_allcolumns[i,]) )
    names(seq_allcolumns) <- rownames(aln_allcolumns)

    dot_allcolumns <- dotify.matrix(aln_allcolumns, 
	aln_allcolumns[tf_index, ], as_logical=FALSE)

    n_stops <- sapply(1:nrow(dot_allcolumns), function(i) 
	length(which(dot_allcolumns[i,]=="Z" | dot_allcolumns[i,]=="*" | 
	    dot_allcolumns[i,]=="#" | dot_allcolumns[i,]=="X")))

    seq_concatamer <- sapply(1:nrow(aln_concatamer), function(i) 
	seqinr::c2s(aln_concatamer[i,]) )
    names(seq_concatamer) <- rownames(aln_concatamer)

    dotseq_concatamer <- sapply(1:nrow(dot_concatamer), function(i) 
	seqinr::c2s(dot_concatamer[i,]) )
    names(dotseq_concatamer) <- rownames(dot_concatamer)

    hd_to_tf_allcolumns <- sapply(1:nrow(dot_allcolumns), function(i) 
	length(which(dot_allcolumns[i,] != ".")) )
    names(hd_to_tf_allcolumns) <- rownames(aln_allcolumns)

    is_viable = (n_stops == 0 & !is_excluded)

    keep_tf_dot_concatamer = dot_concatamer[tf_index, ]
    dot_concatamer[tf_index, ] = aln_concatamer[tf_index, ]

# consider whether a swarm should be an object
# benefit: constructor could contain all this and then select.clones 
# could be called separately
# drawback - not too different from swarmset object, is it?  are both needed?

    aa_alphabet <- sort(unique(c(dot_concatamer)))

    variant_table = tabulate.variants(dot_concatamer,
			 sequence_multiplicity=sequence_multiplicity,
			 omit_singletons=T, 
			 min_counts=min_counts,
			 aa_alphabet=aa_alphabet)

    working_swarm <- list ( is_included = init.clone.list(aln_allcolumns),
# start with all F and include is_included=T from userland below.
# this is intended for greater efficiency during iteration without
# needless calls to update.variant.counts()
                     dot_concatamer = dot_concatamer,
                     seq_concatamer = seq_concatamer,
                     dotseq_concatamer = dotseq_concatamer,
                     aln_concatamer = aln_concatamer,
                     aln_allcolumns = aln_allcolumns,
		     seq_allcolumns = seq_allcolumns,
		     seq_times = parse.timepoints(rownames(aln_allcolumns), 
			 uniquify=F, timepoints_parser),
		     is_viable = is_viable,
                     variant_counts = variant_table,
                     initial_variant_counts = variant_table,
		     initial_n_variants = 0)

    working_swarm$initial_n_variants = 
        length(which(c(working_swarm$initial_variant_counts) > 0))

    # force include TF sequence
    working_swarm <- add.clone(tf_index, working_swarm, "TF")

# TO DO: consider this in default behavior

# if you want it in, you have to specify its inclusion;
# but the dots in dotseq_concatamer will never get included.
# as it stands, the tf polymorphisms never get tabulated
# PROPOSED SOLUTION is to replace dot_concatamer[tf] with seq_concatamer[tf]

    all_timepoints <- sort(unique(working_swarm$seq_times))

    for (curr_t in all_timepoints) {

	t_rows <- which(working_swarm$seq_times == curr_t & 
	    working_swarm$is_viable & hd_to_tf_concatamer > 0)

	if (length(t_rows) == 0) next

	message(paste0("t=", curr_t, ", n=", 
		    length(t_rows), " viable clones"))
	if (length(t_rows) == 1) t_rows <- rep(t_rows, 2) # note kludge 
                                                   # so table() calls will work

        if (is.null(sequence_multiplicity)) {
            trows_multiplicity = NULL
        } else {
            trows_multiplicity = sequence_multiplicity[t_rows]
        }

	# first take the sequences that were specified a priori
#i <- names(which(is_included))
	i <- names(which(is_also_included[t_rows]))
	if (length(i) > 0)
	    for (j in i)
		working_swarm <- add.clone(j, working_swarm, "given")

        # count variants available for curr.t, i.e. on the "supply" side
	variant_supply <- 
	    tabulate.variants(working_swarm$dot_concatamer[t_rows, ], 
		sequence_multiplicity=trows_multiplicity,
		omit_singletons=F, 
		min_counts=min_counts,
		aa_alphabet=aa_alphabet)

### WHY DO SITE NAMES LOOK LIKE THIS:  column 28: G558[]

	sites_with_supply <- apply(variant_supply, 2, max)
	needed_sites <- which(sites_with_supply > 0)

	for (site in names(needed_sites)) {

            if (is.null(site) | site == "" | !site %in% names(needed_sites)) 
		next

            # take first the most abundant variant from supply for site
	    available_variants <- list.available.variants(working_swarm, site)

# sh/could this be more efficient where no available variants are present?
	    if (is.null(available_variants) | length(available_variants) == 0)
		next

	    for (site_variant in available_variants) {

                # do any clones, among viable forms, carry this variant?
		if (site_variant %in% aln_concatamer[t_rows, site]) {

                    # which clones carry this variant?
		    A0 <- names(which(aln_concatamer[t_rows, site] ==
			site_variant))
                
                    if (length(A0) == 1) {

                        # A0: Only one clone carries this mutation
		        working_swarm <- add.clone(A0, working_swarm, "A0")

                    } else {

                        A1 <- names(which(hd_to_tf_concatamer[A0] == 
                            min(hd_to_tf_concatamer[A0])))

                        # A1: One clone is closer to the T/F form among
                        #     concatamer sites than the others

			if (length(A1) == 1) {

			    working_swarm <- add.clone(A1, working_swarm, "A1")

			} else {

		            # B0: minimize env HD to TF
			    B0 <- names(which(hd_to_tf_allcolumns[A0] == 
				min(hd_to_tf_allcolumns[A0])))

			    if (length(B0) == 1) {

				working_swarm <- add.clone(B0, working_swarm, "B0")

			    } else {

				table_allcolumns <- 
				    table(working_swarm$seq_allcolumns[B0])

			        # B1: found multiple copies of one viable 
                                #     full-length clone type. It matters not 
			        #     which, so take the first.

			        if (length(table_allcolumns) == 1) {

				    working_swarm <- add.clone(B0[1], working_swarm, "B1")

				} else {

				    # C: One full-length Env is nearest TF, 
				    #     but multiple identical copies are 
				    # present

                                    # compute average hd to current clone set
				    seqs_a <- create_sequence_subset(B0, 
					working_swarm)
				    seqs_b <- create_sequence_subset(which(
					working_swarm$is_included), 
					working_swarm)

				    hds <- compute_hd_matrix(seqs_a, seqs_b)
				    mean_hds <- apply(hds, 1, mean)

				    table_min_mean_hds <- 
				        sort(table(
					    working_swarm$seq_allcolumns[which(
						mean_hds == min(mean_hds))]))

				    if (length(table_min_mean_hds) == 1 & 
					table_min_mean_hds[1]==1) {

				        # criterion C0:
				        #  one sequence uniquely minimizes
                                        #  distance to swarm
				        C0 <- rownames(hds)[which(mean_hds == 
					    min(mean_hds))]

				        working_swarm <- add.clone(C0, 
					    working_swarm, "C0")

				    } else if (length(min(which(mean_hds == 
						       min(mean_hds))) == 1)) {

				        # criterion C1: multiple identical 
                                        #               sequences minimize mean
                                        #               hd; just take the first
				        C1 <- rownames(hds)[min(which(mean_hds
						    == min(mean_hds)))]
					working_swarm <- add.clone(C1, 
					    working_swarm, "C1")

				    } else {

				        for (i in 1:length(table_allcolumns))
					    cat(paste(table_allcolumns[[i]], 
					    names(table_allcolumns)[i]), '\n')

                                        ### could include both/all
				        err_message=paste0("No clear choice! ",
					    "Failing at t=", curr_t,
					    " for site ", site, 
					    " variant ", site_variant)

					stop(err_message)

			            } # criterion C0/C1/fail
			        } # criterion B1
			    } # criterion B0
		        } # criterion B
		    } # criterion A
	        } # do any clones, among viable forms, carry this variant?
            } # for site.variant in available.variants
        } # for site in names_needed.sites
    } # for curr.t in all.timepoints

   # after having completed iteration, list sites that remain to be covered

    working_swarm$dot_concatamer[tf_index, ] = keep_tf_dot_concatamer
    return (working_swarm)
}
