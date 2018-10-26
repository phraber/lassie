#' @keywords internal

### This function is called by compute.variant.frequencies, which in
### turn is invoked by report.variant.frequencies, i.e. when making
### time-series plots of sites.  By calling compute.tf.area, it
### replicates some by features of the function select.sites()

compute.variant.frequency <- function(S, site, site_ns, tps_mult, 
                                      label_axes, min_variant_count, 
                                      conf_int, col_min, is_time_in_weeks,
                                      site_num, lut, annotate_env,
                                      Tmax, do_barplot, stack_barplot, 
                                      barplot_width, is_tf_hidden, ...) {

    if (class(S) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to compute.variant.frequency()")

    site_counts <- table(S$timepoint_per_sequence, S$aas_aln[, site], 
                         exclude=c("Z", "X", "%", "*", "#"))

    row.order <- order(as.numeric(gsub("[A-Z]", "", rownames(site_counts), ignore.case=T)))
    site_counts = site_counts[row.order, ]

    site_totals <- apply(site_counts, 2, sum)

    # exclude rare variants on second pass
    site_counts <- table(S$timepoint_per_sequence, S$aas_aln[, site], 
                         exclude=c("Z", "X", "%", "*", "#", 
                                   colnames(site_counts)[which(site_totals < min_variant_count)]))

    row.order <- order(as.numeric(gsub("[A-Z]", "", rownames(site_counts), ignore.case=T)))
    site_counts = site_counts[row.order, , drop = FALSE]

    site_ns <- site_ns[row.order] 

    site_freqs <- 100 * sapply(1:ncol(site_counts), function(i)
 	round(site_counts[, i] / site_ns, 4))

    colnames(site_freqs) <- colnames(site_counts)
    rownames(site_freqs) <- rownames(site_counts)

    # reorder columns
    when_up <- sapply(1:ncol(site_freqs), function(i) 
	min(which(site_freqs[, i] > 0)))  
    ### here the criterion is simply when the variant is first non-zero.
      # can we use compute.whenup instead?

    tf_area <- compute.tf.area(t(site_freqs))
    names(tf_area) <- colnames(site_freqs)

    col_order <- order(when_up, tf_area)

    if (length(col_order) > 1 & is.matrix(site_freqs) & is.matrix(site_counts)) {
	site_freqs <- site_freqs[, col_order]
	site_counts <- site_counts[, col_order]
    }

    # don't plot variants (columns) where max_freq never exceeds col_min
    if (!is.null(col_min)) {

	if (is.matrix(site_freqs) & ncol(site_freqs) > 1) {
	    if (is.matrix(site_freqs)) {
		if (ncol(site_freqs) > 1) {
		    max_freq <- apply(site_freqs, 2, max)
		    site_freqs <- site_freqs[, which(max_freq >= col_min)]
		    site_counts <- site_counts[, which(max_freq >= col_min)]
		}
	    }
	} else {
	    max_freq <- ifelse(is.matrix(site_freqs), apply(site_freqs, 2, max), max(site_freqs))
	}
    } else { 
	col_min  = 0
	max_freq = rep(1, ncol(site_counts))
    }

    if (length(which(max_freq >= col_min)) >= 1) {

	s <- S$selected_sites[which(S$selected_site$aln==site), ]

	site_name = paste0(rownames(s), " [", s$aln, "]")

	plot.variant.frequency(site_freqs, site_counts, site_ns, 
	    site_name, label_axes, conf_int, S$tf_loss_cutoff, S$tf_index, S$aas_aln[S$tf_index, site],
	    is_time_in_weeks=is_time_in_weeks, site_num=site_num, lut=lut, 
	    annotate_env=annotate_env, Tmax,
	    do_barplot, stack_barplot, barplot_width, is_tf_hidden, ...)

    } else {
        # blank place-holder
	plot(c(0,0), type='n', frame.plot=F, xlab='', ylab='', xaxt='n', yaxt='n')
    }
}
