#' @keywords internal
tabulate.variants <- function(seqs, sequence_multiplicity, 
    omit_singletons, min_counts, aa_alphabet, is_verbose) {


## TO DO: for resampling, consider case where resampled aa_alphabet
## has additional characters not found in initial aa_alphabet

    # seqs is a matrix of any nuamber of rows and n_sites columns
    # returns a vector or matrix of aa_alphabet counts per column
    # the number of rows is the alphabet size.
    # the option to zero-out single-count variants is for the global table
    if (is.vector(seqs))
	seqs <- t(as.matrix(seqs))

### OTHERWISE - WHAT?

    if (nrow(seqs) == 0)
	stop("ERROR in tabulate.variants(): no rows among sequences provided")

#    aa_counts <- apply(seqs[1:nrow(seqs), ], 2, table)
#    aa_counts <- apply(seqs, 2, table)

#    aas_sorted <- sapply(1:length(aa_counts), function(i) 
#    	rev(sort(aa_counts[[names(aa_counts)[i]]])))
#    aa_counts <- aas_sorted

    variant_counts <- matrix(0, ncol=ncol(seqs), nrow=length(aa_alphabet))
    rownames(variant_counts) <- aa_alphabet    # rows are amino acids
    colnames(variant_counts) <- colnames(seqs) # columns are selected sites

    if (is.null(sequence_multiplicity)) {

#    aa_counts <- apply(seqs, 2, table)

        for (site in 1:ncol(seqs)) { # each selected site

#	    cat(paste0('\n', site, ' '))

	    aa_counts <- table(seqs[, site])
	    
            for (aa in names(aa_counts))
                variant_counts[aa, site] <- aa_counts[[aa]]
	}

    } else {

        for (site in 1:ncol(seqs))
            for (aa in 1:nrow(seqs)) 
                variant_counts[seqs[aa, site], site] <- 
                    variant_counts[seqs[aa, site], site] + 
		        sequence_multiplicity[aa]

    }

    if (omit_singletons)
        for (aa in 1:nrow(variant_counts))
            variant_counts[aa, which(variant_counts[aa, ] < min_counts)] <- 0

    for (other_aa in c("X", "Z", "*", "#", ".")) # excluding '.' should result
                                      # in the TF being included automatically?
        if (other_aa %in% aa_alphabet)
            variant_counts[other_aa, which(variant_counts[other_aa, ] > 0)] <-0

    variant_counts
}
