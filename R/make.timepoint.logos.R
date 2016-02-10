#' Render a logo plot of concatamer forms of clones in working_swarm
#'
#' @param x A swarmset object populated with working_swarm.
#' @param sort_stacks If true, reorder sites from left to right.
#' @param stacks_per_line If NULL, this is set to the number of selected sites; otherwise, it limits plot width.
#' @param name_prefix String used to identify the output files.
#' @param stratify If true, one logo plot is made per time-point sequenced.
#' @param dotify If true, one amino-acid state per site will be left blank to indicate frequency of the TF form.
#' @param stack_width Width of each stack, in pixels.
#' @param aspect_ratio Adjusts aspect ratio (width to height) of image.
#' @param show_sample_size If true, show number of sequences in the fineprint field.
#' @param logo_format Specifies the output image format (default is 'pdf' but can also be 'png', 'svg', or 'jpeg').
#'
#' @return A vector of names of the logo plot files generated, located in a directory removed at the end of the R session.  NB: You will need to copy this file (or these files) during run time or else lose them when the R session ends.
#'
#' @examples
#' \dontrun{
#' A <- lassie::swarmtools(aas_file=system.file("extdata", "CH505-gp160.fasta",
#'             package="lassie"), tf_loss_cutoff=80)
#' B <- lassie::swarmset(A)
#' logo.files <- make.timepoint.logos(B)
#' system(paste(paste("open", logo.files), collapse=';')
#' }
#'
#' @family swarmset methods
#' @export
make.timepoint.logos <- function(x, sort_stacks=F, stacks_per_line=NULL,
    name_prefix="logos", stratify=T, dotify=T, stack_width=18, aspect_ratio=3,
    show_sample_size=F,
    logo_format="pdf") {

    if (class(x) != "swarmset")
	stop("ERROR in make.timepoint.logos(): Invalid swarmset object")

    if (sort_stacks) {

        site.order <- order(x$selected_sites$aln)
        x$selected_sites = x$selected_sites[site.order, ]

        ## need to restring the concatamers
        x$aln_concatamer <- x$aln_concatamer[, site.order]
#        colnames(x$aln_concatamer) =  colnames(x$aln_concatamer)[site.order]

        x$working_swarm$seq_concatamer <- sapply(1:nrow(x$aln_concatamer), function(i)
          seqinr::c2s(x$aln_concatamer[i,]) )
        names(x$working_swarm$seq_concatamer) <- rownames(x$aln_concatamer)

        x$working_swarm$dot_concatamer <- dotify.matrix(x$aln_concatamer,
            x$aln_concatamer[x$tf_index, ], as_logical=F)
        names(x$working_swarm$dot_concatamer) <- rownames(x$aln_concatamer)

        x$working_swarm$dotseq_concatamer <- sapply(1:nrow(x$working_swarm$dot_concatamer), 
	    function(i) seqinr::c2s(x$working_swarm$dot_concatamer[i,]) )
        names(x$working_swarm$dotseq_concatamer) <- rownames(x$working_swarm$dot_concatamer)
    }

    if (stratify) {

	Ts = sort(unique(x$working_swarm$seq_times))
	outfiles <- rep(NA, length(Ts))

	for (i in 1:length(Ts)) {

	    timepoint = Ts[i]

	    do_dotify = ifelse(timepoint==Ts[1], F, dotify)
	    hide_xlabels = ifelse(timepoint==Ts[length(Ts)], F, T)

	    this_rows = which(x$working_swarm$is_viable & 
		x$working_swarm$seq_times==timepoint)

	    this_mult = rep(1, length(this_rows))

	    outfiles[i] <- make.logoplot(x$selected_sites, 
		x$working_swarm,
		this_rows,
		paste0(name_prefix, '-', timepoint, '-'), 
		dotify=do_dotify, 
		stack_width=stack_width,
		stacks_per_line=stacks_per_line,
		aspect_ratio=aspect_ratio, # was 6
		hide_xlabels=hide_xlabels, 
		y_label=timepoint,
		show_sample_size=show_sample_size,
		logo_format=logo_format)
	}

	return ( outfiles )

    } else {

	# pool all timepoints
	this_rows = which(x$working_swarm$is_viable)

	this_mult = rep(1, length(this_rows))

	return (outfile <- make.logoplot(x$selected_sites, x$working_swarm,
	    this_rows,
	    paste0(name_prefix, ifelse(dotify, '-DOT', '-ALL')),
	    dotify=dotify, 
	    hide_xlabels=F, 
	    y_label="proportion",
	    show_sample_size=show_sample_size,
	    stack_width=stack_width,
	    stacks_per_line=stacks_per_line,
	    aspect_ratio=aspect_ratio,
	    logo_format=logo_format) )
    }
}

