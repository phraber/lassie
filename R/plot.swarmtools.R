#' Plot a cumulative distribution function of number of sites selected vs. TF loss cutoff.
#'
#' @param S Swarmtools object
#' @export
plot.swarmtools <- function(S) {

    x_lab="Loss cutoff, % of sample"
    y_lab="Sites above cutoff, %"
    my_main=paste0("n sites = ", length(S$peak_tf_loss))
    y_lim=rev(c(-0.03, 1.01))

    if (!is.null(S$peak_tf_loss)) {

        cdf_tf_loss=ecdf(S$peak_tf_loss)
	x_lim=c(-1,1.08*max(S$peak_tf_loss))

	plot(cdf_tf_loss, col.01line="transparent", lwd=2/2, 
	    verticals=T, pch=NA,
	    xlim=x_lim, ylim=y_lim, xlab="", ylab="", 
	    frame.plot=F, yaxt='n', xaxt='n', 
	    main=my_main, 
	    pty='s',
	    oma=c(0,0,0,0), 
	    mar=c(8/4, 8/4, 1/4, 1/4),
	    mgp=c(6/4, 1/4, 1/5),
	    ps=12,
	    cex.axis=10/12, 
	    cex.lab=9/12,
	    lend=1, ljoin=1, tcl=-1/5, 
	    xaxs='i', yaxs='i', las=1)

	points(0, length(which(S$peak_tf_loss==0)) / length(S$peak_tf_loss), 
	    pch=21, bg=NA, lwd=1/2, cex=1/2)

	text(2, 0.975*length(which(S$peak_tf_loss==0)) /length(S$peak_tf_loss),
	    paste0(length(which(S$peak_tf_loss > 0)), " sites vary"), 
	    adj=c(0,0/2), cex=9/12)

	n_sites = length(S$peak_tf_loss)

### TO DO: CONSIDER TREATMENT FOR tf_loss_cutoff == 0 - use > tf_loss_cutoff?

	if (!is.null(S$tf_loss_cutoff)) {
	    n_selected = sapply(1:length(S$tf_loss_cutoff), function(i) 
		length(which(S$peak_tf_loss >= S$tf_loss_cutoff[i]))) # closed interval

	    p_selected = n_selected/n_sites

	    text(S$tf_loss_cutoff, 1-p_selected, adj=c(0, 0), cex=8/12, 
		paste0(" ", n_selected), srt=45)

	    segments(S$tf_loss_cutoff, (usr <- par('usr'))[3], 
		S$tf_loss_cutoff, 1-p_selected, lty=2, lwd=1/2)

	    points(S$tf_loss_cutoff, 1-p_selected, pch=21, bg=NA, lwd=1/2, cex=1/2)
	}
	axis(2, at=0:4/4, labels=4:0*25, yaxt='s', las=1, cex.axis=10/12, 
	    cex.lab=12/12, tcl=-1/5)

	axis(1, at=0:4*25, labels=0:4*25, line=1/16, padj=0, xaxt='s', las=1, 
	    cex.axis=10/12, cex.lab=12/12, mgp=c(5/4, 1/8, 1/10), tcl=-1/5)

	mtext(x_lab, 1, line=2, cex=11/12)
	mtext(y_lab, 2, line=2, cex=11/12)
    }
}
