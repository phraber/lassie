#' @keywords internal
make.timepoint.logos <- function(ST, SST, filename_prefix=NULL,
	stratify=T, dotify=F, stack_width=18, aspect_ratio=3) {

### TO DO: eliminate ST requirement

    if (class(ST) != "swarmtools") 
	stop("ERROR in make.timepoint.logos(): Invalid swarmtools object")

    if (class(SST) != "swarmset")
	stop("ERROR in make.timepoint.logos(): Invalid swarmset object")

    if (is.null(filename_prefix)) 
	filename_prefix = paste0(ST$results_prefix, "-logos-", stratify, 
	    dotify)

    if (stratify) {

	Ts = sort(unique(SST$working_swarm$seq_times))
	outfiles <- rep(NA, length(Ts))

	for (i in 1:length(Ts)) {

	    timepoint = Ts[i]

	    do_dotify = ifelse(timepoint==Ts[1], F, dotify)
	    hide_xlabels = ifelse(timepoint==Ts[length(Ts)], F, T)

	    this_rows = which(SST$working_swarm$is_viable & 
		SST$working_swarm$seq_times==timepoint)

	    this_mult = rep(1, length(this_rows))
	    if (!is.null(ST$sequence_multiplicity))
		this_mult = ST$sequence_multiplicity[this_rows]

	    outfiles[i] <- make.logoplot(SST$selected_sites, 
		SST$working_swarm,
		this_rows,
		paste0(filename_prefix, '-', timepoint), 
		dotify=do_dotify, 
		stack_width=stack_width,
		aspect_ratio=aspect_ratio, # was 6
		hide_xlabels=hide_xlabels, y_label=timepoint, 
		sequence_multiplicity=this_mult)

	}

	return ( outfiles )

    } else {

	# pool all timepoints
	this_rows = which(SST$working_swarm$is_viable)

	this_mult = rep(1, length(this_rows))
	if (!is.null(ST$sequence_multiplicity))
	    this_mult = ST$sequence_multiplicity[this_rows]

	return (outfile <- make.logoplot(SST$selected_sites, SST$working_swarm,
	    this_rows,
	    paste0(filename_prefix, ifelse(do_dotify, '-DOT', '-ALL')),
	    stack_width=stack_width,
	    aspect_ratio=aspect_ratio, # was 6
	    dotify=do_dotify, 
	    hide_xlabels=F, y_label="proportion", 
	    sequence_multiplicity=this_mult) )
    }

}

