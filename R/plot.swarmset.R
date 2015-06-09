#' Render a logo plot of concatamer forms of clones in working_swarm
#'
#' @param SST A swarmset object populated with working_swarm.
#' @param format Image format.
#' @param aspect_ratio Aspect ratio (width to height) of image.
#' @return An explicit path to the file generated, located in a directory removed at the end of the R session.
#' @family swarmset methods
#' @export
plot.swarmset <- function(SST, format="png", aspect_ratio=3) {

### TO DO: parse and match whatever is in ...?

    outfile <- make.logoplot(SST$selected_sites, 
	    SST$working_swarm, 
	    which(SST$working_swarm$is_included), 
	    paste0(SST$results_prefix, "-swarm"), 
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
