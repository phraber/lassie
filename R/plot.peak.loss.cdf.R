#' @keywords internal
plot.peak.loss.cdf = function(S1, S2=NULL, plotfile_type='pdf', verbose=T, 
    my_palette=NULL) {

    if (class(S1) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to plot.peak.loss.cdf()!")

    if (!is.null(S2) & class(S2) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object for S2 to plot.peak.loss.cdf()!")

    if (is.null(S2)) {
	if (is.null(S1$aas_prefix)) {
	    if (!is.null(S1$ptid)) {
		if (!is.null(S1$region)) {
		    my_prefix = paste0(S1$ptid, "-", S1$region)
		} else {
		    my_prefix = S1$ptid
		}
	    }
	} else {
	    my_prefix = S1$aas_prefix
	}
    } else {
	if (!is.null(S1$ptid) & !is.null(S2$ptid) & S1$ptid == S2$ptid) {
	    my_prefix=S1$ptid
	} else if (!is.null(S1$region) & !is.null(S2$region) & S1$region == S2$region) {
	    my_prefix=S1$region
	}
    }

    plot_suffix = "-cumulativeTFloss."	

    if (length(S1$tf_loss_cutoff)==1) 
	plot_suffix = paste0("-cumulativeTFloss-", S1$tf_loss_cutoff, 
	    "percent.")

    out_file=paste0(my_prefix, plot_suffix, plotfile_type)

    if (plotfile_type == "pdf") {

	pdf(out_file, paper='special', width=3, height=3, 
	    useDingbats=F, pointsize=12)

    } else {

	postscript(out_file, width=3.25, height=3.25, pointsize=12,
	    bg='white', horizontal=F)

    }

    par(oma=c(0,0,0/4,0/2))
    par(mar=c(10/4,10/4,1/2,1/4))
    par(mgp=c(5/4, 1/4, 1/5))
    par(cex.axis=10/12)
    par(cex.lab=12/12)
    par(lend=1, ljoin=1)
    par(tcl=-1/5)
    par(xaxs='i', yaxs='i')
    par(las=1)

    if (!is.null(my_palette))
	palette(my_palette)

    x_lab="Peak TF loss cutoff, % of sample"
    y_lab=ifelse(is.null(S2), 
	paste0("Sites above cutoff, % of ", length(S1$peak_tf_loss)), 
	paste0("Sites above cutoff, % of total"))
    y_lim=rev(c(-0.03, 1.01))



    cdf_tf_loss=ecdf(S1$peak_tf_loss)
    x_lim=c(-1,1.04*max(S1$peak_tf_loss))

    plot(cdf_tf_loss, col.01line="transparent", lwd=2/2, verticals=T, pch=NA,
	xlim=x_lim, ylim=y_lim, xlab=x_lab, ylab=y_lab, main='', 
	frame.plot=F, yaxt='n', xaxt='n')

    points(0, length(which(S1$peak_tf_loss==0)) / length(S1$peak_tf_loss), 
	pch=21, bg=NA, lwd=1/2, cex=1/2)

    text(2, 0.975*length(which(S1$peak_tf_loss==0)) / length(S1$peak_tf_loss), 
	paste0(length(which(S1$peak_tf_loss > 0)), " sites vary"), 
	adj=c(0,0/2), cex=9/12)

    n_sites = length(S1$peak_tf_loss)
    n_selected = sapply(1:length(S1$tf_loss_cutoff), function(i) 
	length(which(S1$peak_tf_loss >= S1$tf_loss_cutoff[i])))#closed interval
    p_selected = n_selected/n_sites

    if (S1$tf_loss_cutoff[1] == 0) { # note kludge - test this?

	if (is.null(S2)) {

	    segments(S1$tf_loss_cutoff[-1], (usr <- par('usr'))[3], 
		S1$tf_loss_cutoff[-1], 1-p_selected[-1], col=6, lty=1, lwd=1/2)

	    text(S1$tf_loss_cutoff[-1], 1-p_selected[-1], adj=c(0, 0), 
		cex=8/12, paste0(" ", n_selected[-1]), srt=45)

	} else {
	    text(S1$tf_loss_cutoff[-1], 1-p_selected[-1], adj=c(1, 1), 
		cex=8/12, paste0(n_selected[-1], " "), srt=45)
	}
	points(S1$tf_loss_cutoff[-1], 1-p_selected[-1], pch=21, bg=NA, 
	    lwd=1/2, cex=1/2)
    } else {

	if (is.null(S2)) {

	    text(S1$tf_loss_cutoff, 1-p_selected, adj=c(0, 0), cex=8/12, 
		paste0(" ", n_selected), srt=45)

	    segments(S1$tf_loss_cutoff, (usr <- par('usr'))[3], 
		S1$tf_loss_cutoff, 1-p_selected, col=6, lty=1, lwd=1/2)

	} else {

	    text(S1$tf_loss_cutoff, 1-p_selected, adj=c(1, 1), cex=8/12, 
		paste0(n_selected, " "), srt=45)
	}

	points(S1$tf_loss_cutoff, 1-p_selected, pch=21, bg=NA, lwd=1/2, 
	    cex=1/2)
    }


    if (!is.null(S2)) {

	cdf_tf_loss=ecdf(S2$peak_tf_loss)
#	x_lim=c(-1,1.03*max(S2$peak_tf_loss))

	par(new=T)

	plot(cdf_tf_loss, col.01line="transparent", lwd=2/2, verticals=T, 
	    pch=NA, xlim=x_lim, ylim=y_lim, xlab='', ylab='', main='', 
	    frame.plot=F, yaxt='n', xaxt='n', col=2)#'red')

	points(0, length(which(S2$peak_tf_loss==0)) / length(S2$peak_tf_loss), 
	    pch=21, bg=NA, lwd=1/2, cex=1/2, col=2)

	text(2, 0.975*length(which(S2$peak_tf_loss==0)) / 
	    length(S2$peak_tf_loss), 
	    paste0(length(which(S2$peak_tf_loss > 0)), " sites vary"), 
	    adj=c(0,0/2), cex=9/12, col=2)

	n_sites = length(S2$peak_tf_loss)
	n_selected = sapply(1:length(S2$tf_loss_cutoff), function(i) 
	    length(which(S2$peak_tf_loss >= 
		S2$tf_loss_cutoff[i])))#closed interval

	p_selected = n_selected/n_sites

	if (S2$tf_loss_cutoff[1] == 0) { # note kludge

#		segments(S2$tf_loss_cutoff[-1], (usr <- par('usr'))[3], S2$tf_loss_cutoff[-1], 1-p_selected[-1], col=6, lty=1, lwd=1/2)
	    points(S2$tf_loss_cutoff[-1], 1-p_selected[-1], pch=21, bg=NA, 
		lwd=1/2, cex=1/2)

	    text(S2$tf_loss_cutoff[-1], 1-p_selected[-1], adj=c(0, 0), 
		cex=8/12, paste0(" ", n_selected[-1]), srt=45)

	} else {
#		segments(S2$tf_loss_cutoff, (usr <- par('usr'))[3], S2$tf_loss_cutoff, 1-p_selected, col=, lty=1, lwd=1/2)
	    points(S2$tf_loss_cutoff, 1-p_selected, pch=21, bg=NA, lwd=1/2, 
		cex=1/2, col=2)

	    text(S2$tf_loss_cutoff, 1-p_selected, adj=c(0, 0), cex=8/12, 
		paste0(" ", n_selected), srt=45, col=2)
	}
    }

#    cutoff_results = c(n_selected, length(which(S1$peak_tf_loss > 0)))
#    names(cutoff_results) = c(paste0(">=", S1$tf_loss_cutoff, "%"), "varying")

    axis(2, at=c(0:4)/4, labels=c(4:0)/0.04, yaxt='s')
    par(mgp=c(5/4, 1/8, 1/10))
    axis(1, at=100*c(0:4)/4, labels=c(0:4)/0.04, line=par('mgp')[2]/2, 
	padj=0, xaxt='s')

#    if (!is.null(my_prefix)) {

    if (is.null(S2)) {

	mtext(gsub("-", " ", my_prefix, fixed=T), 3, cex=12/12, adj=1/2, 
	    padj=1, line=0)

    } else {

	if (!is.null(S1$ptid) & !is.null(S2$ptid) & S1$ptid == S2$ptid) {

	    mtext(S1$ptid, 3, cex=12/12, adj=1/2, padj=1, line=0)
	    legend('topright', c(S1$region, S2$region), lwd=1, col=c(1,2), 
		bty='n')

	} else {

	    if (!is.null(S1$region) & !is.null(S2$region) & S1$region == 
		S2$region) {

		mtext(S1$region, 3, cex=12/12, adj=1/2, padj=1, line=0)

		legend('topright', c(S1$ptid, S2$ptid), lwd=1, col=c(1,2), 
		    bty='n')
	    }
	}
    }

    dev.off()

    if (verbose)
	system(paste("open", out_file))

#    cutoff_results
}
