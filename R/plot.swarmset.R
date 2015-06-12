#' Render a logo plot of concatamer forms of clones in working_swarm
#'
#' @param x A swarmset object populated with working_swarm.
#' @param ... can include "aspect_ratio" to adjust Aspect ratio (width to height) of image and "format" to specify the output image format (default is 'png' but can also be 'pdf', 'svg', or 'jpeg').
#' 
#' @return An explicit path to the file generated, located in a directory removed at the end of the R session.  NB: You will need to copy this file during run time or else los it when the R session ends.
#' 
#' @family swarmset methods
#' @export
plot.swarmset <- function(x, ...) {

    dots <- list(...)

    format = "png"
    if (!is.null(dots$format))
        format = dots$format

    aspect_ratio = 3
    if (!is.null(dots$aspect_ratio))
        aspect_ratio = dots$aspect_ratio

### TO DO: parse and match whatever is in ...

    outfile <- make.logoplot(x$selected_sites, 
	    x$working_swarm, 
	    which(x$working_swarm$is_included), 
	    paste0(x$results_prefix, "-swarm"), 
	    dotify=F, aspect_ratio=aspect_ratio,
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
