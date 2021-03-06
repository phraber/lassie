#' @keywords internal
compute.variant.frequencies <- function(S, min_variant_count, conf_int, 
    col_min, is_time_in_weeks, color_lut_type, annotate_env, number_sites, 
    Tmax, do_barplot, stack_barplot, barplot_width, is_tf_hidden, ...) {

    #  iterate over selected_sites, calling compute.variant.frequency per site

    if (class(S) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to compute.variant.frequencies()")

    # if the package is not installed, simply ignore the argument and proceed
#    if (conf_int) 
#	conf_int = require(binom)

    # site_ns is the denominator for computing frequencies.
    # this does not equal the sum of each variant frequency, particularly
    # where reads (sequences) are excluded from summary/tabulation 
    # due to rare variants (<=mvc), stops, or incomplete codons.

    # tps is "timepoints"; 
    # facilitates tabulating read counts in compute.variant.frequency
    if (!is.null(S$sequence_multiplicity)) {

	stop("Sorry, this feature is broken - think about how to tabulate variants given sequence multiplicities")

	site_ns <- as.matrix(rep(0, length(unique(S$timepoint_per_sequence))), 
	    ncol=1)
	rownames(site_ns) <- unique(S$timepoint_per_sequence)

	for (i in 1:length(S$timepoint_per_sequence))
	    site_ns[S$timepoint_per_sequence[i]] <- site_ns[S$timepoint_per_sequence[i]] + 
	        S$sequence_multiplicity[i]

    } else {
	tps_mult <- NULL
	site_ns <- as.matrix(table(S$timepoint_per_sequence), ncol=1)
    }

    # slice through alignment, one column (selected site) at a time
    for (i in 1:length(S$selected_sites$aln)) {

        my.sitenum <- NULL
	if (number_sites) my.sitenum = i

	compute.variant.frequency(S, S$selected_sites$aln[i], 
	    site_ns, 
	    tps_mult, 
	    i==1, # only label axes for first selected site
	    min_variant_count=min_variant_count, 
	    conf_int=conf_int, 
	    col_min=col_min, 
	    is_time_in_weeks=is_time_in_weeks,
	    site_num=my.sitenum, 
	    lut=color_lut_type,
	    annotate_env=annotate_env, Tmax,
	    do_barplot, stack_barplot, barplot_width, is_tf_hidden, ...)
    }
}
