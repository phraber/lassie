#' Plot a cumulative distribution function of number of sites selected vs. TF loss cutoff.
#'
#' @param x Swarmtools object
#' @param ... plot options, parsed and passed to plot(): cex, xlab, ylab, pch, lwd, main, tcl, oma, mar, mgp, xlim, ylim.  Defaults are set but you can over-write them to customize plot appearance.
#'
#' @export
plot.swarmtools <- function(x, ...) {

    dots = list(...)

    ### one could do this far more elegantly by processing names of dots, no?

    xlab="Loss cutoff, % of sample"
    if (!is.null(dots$xlab))
        xlab=dots$xlab

    cex=1
    if (!is.null(dots$cex))
        cex=dots$cex

    ylab="Sites above cutoff, %"
    if (!is.null(dots$ylab))
        ylab=dots$ylab
    
    pch=21
    if (!is.null(dots$pch))
        pch=dots$pch

    lwd=1
    if (!is.null(dots$lwd))
        lwd=dots$lwd
    
    main=paste0("n sites = ", length(x$peak_tf_loss))
    if (!is.null(dots$main))
        main=dots$main

    tcl=-1/5
    if (!is.null(dots$tcl))
        tcl = dots$tcl

    oma=c(0,0,0,0) 
    if (!is.null(dots$oma))
	oma = dots$oma

    mar=c(6/4, 8/4, 1/4, 1/4)
    if (!is.null(dots$mar))
	mar = dots$mar

    mgp=c(7/4, 1/4, 1/5)
    if (!is.null(dots$mgp))
	mgp = dots$mgp

    ps=12
    if (!is.null(dots$ps))
	ps = dots$ps

    ylim=rev(c(-0.03, 1.01))
    if (!is.null(dots$ylim))
	ylim=dots$ylim

    if (!is.null(x$peak_tf_loss)) {

        cdf_tf_loss=ecdf(x$peak_tf_loss)

        xlim=c(-1, 1.08 * max(x$peak_tf_loss))

	if (!is.null(dots$xlim))
	    xlim=dots$xlim

	plot(cdf_tf_loss, col.01line="transparent", lwd=lwd, 
	    verticals=T, pch=NA,
	    xlim=xlim, ylim=ylim, 
	    frame.plot=F, yaxt='n', xaxt='n', 
	    main=main, 
	    pty='s',
	    oma=oma, 
	    mar=mar,
	    mgp=mgp,
	    ps=ps,
	    xlab=xlab, 
	    ylab=ylab,
	    cex.axis=cex*10/12, 
	    cex.lab=cex*9/12,
	    lend=1, ljoin=1, tcl=-1/5, 
	    xaxs='i', yaxs='i', las=1)

	points(0, length(which(x$peak_tf_loss==0)) / length(x$peak_tf_loss), 
	    pch=pch, bg=NA, lwd=lwd/2, cex=cex/2)

	text(2, 0.975*length(which(x$peak_tf_loss==0)) /length(x$peak_tf_loss),
	    paste0(length(which(x$peak_tf_loss > 0)), " sites vary"), 
	    adj=c(0,0/2), cex=cex*9/12)

	n_sites = length(x$peak_tf_loss)

### TO DO: CONSIDER TREATMENT FOR tf_loss_cutoff == 0 - use > tf_loss_cutoff?

# also consider reintroducing usage that allows tf_loss_cutoff to be a vector

	if (!is.null(x$tf_loss_cutoff)) {

	    n_selected = sapply(1:length(x$tf_loss_cutoff), function(i) 
		length(which(x$peak_tf_loss >= x$tf_loss_cutoff[i]))) # closed interval

	    p_selected = n_selected/n_sites

	    text(x$tf_loss_cutoff, 1-p_selected, adj=c(0, 0), cex=cex*8/12, 
		paste0(" ", n_selected), srt=45)

	    segments(x$tf_loss_cutoff, (usr <- par('usr'))[3], 
		x$tf_loss_cutoff, 1-p_selected, lty=2, lwd=lwd/2)

	    points(x$tf_loss_cutoff, 1-p_selected, pch=pch, bg=NA, lwd=lwd/2, cex=cex/2)
	}

	axis(2, at=0:4/4, labels=4:0*25, yaxt='s', las=1, cex.axis=cex*10/12, 
	    cex.lab=cex*12/12, tcl=tcl)

	axis(1, at=0:4*25, labels=0:4*25, line=1/16, padj=0, xaxt='s', las=1, 
	    cex.axis=cex*10/12, cex.lab=cex*12/12, mgp=c(5/4, 1/8, 1/10), tcl=tcl)

#	mtext(xlab, 1, line=2, cex=cex*11/12)
#	mtext(ylab, 2, line=2, cex=cex*11/12)
    }
}
