#' Render a logo plot of concatamer forms of clones in working_swarm
#'
#' @param x A swarmset object populated with working_swarm.
#' @param sort_stacks If true, reorder sites from left to right.
#' @param stacks_per_line If NULL, this is set to the number of selected sites; otherwise, it limits plot width.
#' @param dotify If true, one amino-acid state per site will be left blank to indicate frequency of the TF form.
#' @param ... can include "aspect_ratio" to adjust Aspect ratio (width to height) of image and "format" to specify the output image format (default is 'png' but can also be 'pdf', 'svg', or 'jpeg').
#'
#' @return An explicit path to the file generated, located in a directory removed at the end of the R session.  NB: You will need to copy this file during run time or else lose it when the R session ends.
#'
#' @family swarmset methods
#' @export
plot.swarmset <- function(x, sort_stacks=F, stacks_per_line=NULL, dotify=F, ...) {

    dots <- list(...)

    format = "png"
    if (!is.null(dots$format))
        format = dots$format

    aspect_ratio = 3
    if (!is.null(dots$aspect_ratio))
        aspect_ratio = dots$aspect_ratio

### TO DO: parse and match whatever is in ...
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
            x$aln_concatamer[x$tf_index, ], as_logical=FALSE)
        names(x$working_swarm$dot_concatamer) <- rownames(x$aln_concatamer)

        x$working_swarm$dotseq_concatamer <- sapply(1:nrow(x$working_swarm$dot_concatamer), 
	    function(i) seqinr::c2s(x$working_swarm$dot_concatamer[i,]) )
        names(x$working_swarm$dotseq_concatamer) <- rownames(x$working_swarm$dot_concatamer)
    }

    outfile <- make.logoplot(x$selected_sites,
	    x$working_swarm,
	    which(x$working_swarm$is_included),
	    paste0("swarmset-n", length(which(x$working_swarm$is_included))),
	    stacks_per_line = stacks_per_line,
	    dotify=dotify, aspect_ratio=aspect_ratio,
	    logo_format=format)

#    if (format=="png") {
#        require(png, quietly=T)
#	logo.png <- readPNG(outfile, native=T, info=T)
#	currdim <- dim(logo.png)
#	par(oma=c(0,0,0,0), mar=c(0,0,0,0))
#	plot(0, 0, type='n', xlab='', ylab='',
#	    xlim=c(1/2, currdim[2]+1/2), ylim=c(1/2, currdim[1]+1/2),
#	    xaxs='i', yaxs='i', xaxt='n', yaxt='n', frame.plot=F)
#	rasterImage(logo.png, 1, 1, currdim[2], currdim[1], interpolate=F)
#    } else {
#        cat(paste("opening", outfile, "\n"))
#        system(paste("open", outfile))
#    }

     outfile
}
