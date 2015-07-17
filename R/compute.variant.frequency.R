#' @keywords internal
compute.variant.frequency <- function(S, site, site_ns, tps_mult, label_axes, 
    min_variant_count, conf_int, col_min, is_time_in_weeks, site_num, lut) {

    if (class(S) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to compute.variant.frequency()")

    if (is.null(S$sequence_multiplicity)) {

	site_counts <- table(S$timepoint_per_sequence, S$aas_aln[, site], 
	    exclude=c("Z", "X", "%", "*", "#"))
	site_totals <- apply(site_counts, 2, sum)

	# exclude rare variants on second pass
	site_counts <- table(S$timepoint_per_sequence, S$aas_aln[, site], 
	    exclude=c("Z", "X", "%", "*", "#", 
		colnames(site_counts)[which(site_totals < min_variant_count)]))
    } else {

        # expand each read by corresponding multiplicity 
	aas_mult <- rep(S$aas_aln[, site], S$sequence_multiplicity) 

	site_counts <- table(tps_mult, aas_mult, 
	    exclude=c("Z", "X", "%", "*", "#"))
	site_totals <- apply(site_counts, 2, sum)

	# exclude rare variants on second pass
	site_counts <- table(tps_mult, aas_mult, 
	    exclude=c("Z", "X", "%", "*", "#", 
		colnames(site_counts)[which(site_totals < min_variant_count)]))
    }

    if (ncol(site_counts) > 1) { # polymorphic?

#	site_freqs <- matrix(NA, nc=ncol(site_counts), nrow=nrow(site_counts))

#	for (i in 1:ncol(site_counts))
#	    site_freqs[, i] = 100*round(site_counts[, i] / site_ns, 4)

 	site_freqs <- 100 * sapply(1:ncol(site_counts), function(i)
 	    round(site_counts[, i] / site_ns, 4))

	colnames(site_freqs) <- colnames(site_counts)
	rownames(site_freqs) <- rownames(site_counts)

        # reorder columns
	when_up <- sapply(1:ncol(site_freqs), function(i) 
	    min(which(site_freqs[, i] > 0)))

	tf_area <- compute.tf.area(t(site_freqs))
	names(tf_area) <- colnames(site_freqs)

	col_order <- order(when_up, tf_area)

	if (length(col_order) > 1) {
	    site_freqs <- site_freqs[, col_order]
	    site_counts <- site_counts[, col_order]
	}

        # don't plot variants (columns) where max_freq never exceeds col_min
	if (!is.null(col_min)) {

	    if (is.matrix(site_freqs) & ncol(site_freqs) > 1) {
		max_freq <- apply(site_freqs, 2, max)
		site_freqs <- site_freqs[, which(max_freq >= col_min)]
	        site_counts <- site_counts[, which(max_freq >= col_min)]
	    }

	} else { 
	    col_min  = 0
	    max_freq = rep(1, ncol(site_counts))
	}

	if (length(which(max_freq >= col_min)) > 1) {

	    s <- S$selected_sites[which(S$selected_site$aln==site), ]

# old site name
    # aln/ tf_aa hxb2.l (^)
#    site_name = paste0(gsub("^[A-Z-]", "", rownames(s)), 
#	"/ ", gsub("[0-9]*$", "", rownames(s)), 
#	s$hxb2.l, ifelse(s$hxb2.l == s$hxb2.r, "", "^"))

	    site_name = paste0(rownames(s), " [", s$aln, "]")

#	s$hxb2.l, ifelse(s$hxb2.l == s$hxb2.r, "", "^"),
#	gsub("^[A-Z-]", "", rownames(s))

	    plot.variant.frequency(site_freqs, site_counts, site_ns, 
		site_name, label_axes, conf_int, S$tf_loss_cutoff,
		is_time_in_weeks=is_time_in_weeks, site_num=site_num, lut)

	}
    }
}
