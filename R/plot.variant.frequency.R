#' @keywords internal
plot.variant.frequency <- function(site_freqs, site_counts, n_sequenced, 
    site_name, label_axes, conf_int, tf_loss_cutoff, 
    is_time_in_weeks, site_num = NULL, lut) {

    x_values <- as.numeric(gsub("^[A-Za-z]*", "", rownames(site_freqs)))
    my_xlim <- c(0, max(x_values))
#    my_xlim <- c(-14, max(x_values) + 14)
    my_grey <- "grey67"#, seqinr::col2alpha("black", alpha=4/5)

    if (ncol(site_freqs) < 2)
	stop("WTF")

# each panel uses different color scheme, by column-ordering criterion
    my_colors = aa.col[tolower(colnames(site_freqs)), lut]

    plot(0, 0, type='n', xlab='', ylab='', xlim=my_xlim, ylim=c(-2,102), frame.plot=F)
    abline(h=c(0:5*20), lwd=1/2, col=my_grey, lty=1)
#    abline(h=c(0:4*25), lwd=1/2, col=my_grey, lty=1)

# TO DO: specify or set upper bound on number of years of follow-up

    if (is_time_in_weeks) {
	abline(v=c(0:4*52), lwd=1/2, col=my_grey, lty=1)
	if (label_axes)
	    mtext(paste0("Y",c(0:4)), 1, at=c(0:4)*52, line=0/4, cex=8/12, 
		col=my_grey)
    } else {
	abline(v=c(0:4*365.25), lwd=1/2, col=my_grey, lty=1)
	if (label_axes)
           mtext(paste0("Y",c(0:4)), 1, at=c(0:4)*365.25, line=0/4, cex=8/12, 
	       col=my_grey)
    }

    #TO DO: optionally add H line for tf_loss_cutoff
#    if (!is.null(tf_loss_cutoff))
#	abline(h=100-tf_loss_cutoff, lwd=1/2, col=my_grey, lty=3)

    if (label_axes) {

        mtext(c("20%", "60%", "100%"), 2, at=c(1,3,5)*20, line=0/4, cex=8/12, 
	    col=my_grey)

#        mtext(c("0%", "40%", "80%"), 2, at=c(0,2,4)*20, line=0/4, cex=8/12, col=my_grey)
#        mtext(c("0%", "50%", "100%"), 2, at=c(0,2,4)*25, line=0/4, cex=8/12, col=my_grey)
#        mtext(c("25%", "50%", "75%"), 2, at=c(1:3)*25, line=0/4, cex=8/12, col=my_grey)
#        mtext(c("25%", "75%"), 2, at=c(25, 75), line=0/4, cex=8/12, col=my_grey)
    }

    # iterate over variants found in this position
    # note that the first things drawn are covered by things drawn afterwards,
    # so iteration over variants is done per things to be drawn, rather than 
    # one big loop for all variant forms.  In other words, minimize obscuring
    # lines or points/dots for earlier variants (TF) with polygons from later
    # by first drawing all the CI polygons, 

# TO DO: enable option to specify CI level_s instead of default 95% CI
    if (conf_int) {

	for (variant in c(1:ncol(site_freqs))) {

	    my_bgcolor = seqinr::col2alpha(my_colors[variant], alpha=1/5)

	    cis = binom::binom.confint(site_counts[,variant], n_sequenced, method='e')

# TF (timepoint 1) is inferred by other means so assert its accuracy
	    cis$lower[1] = site_freqs[1, variant]/100
	    cis$upper[1] = site_freqs[1, variant]/100


# these literal values are provided if spline is undesirable
#	    lines(x_values, 100*cis$upper, col=my_colors[variant], lwd=1/2)
#	    lines(x_values, 100*cis$lower, col=my_colors[variant], lwd=1/2)

#	    lines(x_values, c(site_freqs[1, variant], 100*cis$upper[-1]), col=my_bgcolor, lwd=1/2)
#	    lines(x_values, c(site_freqs[1, variant], 100*cis$lower[-1]), col=my_bgcolor, lwd=1/2)

#	    if (1) {

	    # spline fit the CIs to reduce spiky effects
	    my_lambda = 1/4 # spline smoothing parameter

	    ub.lines = smooth.spline(c(site_freqs[1, variant], 100*cis$upper[-1]) ~ x_values, 
		spar=my_lambda, w=(c(100, n_sequenced[-1]))) # weigh each frequency by number of sequences
#		spar=my_lambda, w=sqrt(c(100, n_sequenced[-1]))) # weigh each value by sqrt of number of sequences

	    lb.lines = smooth.spline(c(site_freqs[1, variant], 100*cis$lower[-1]) ~ x_values,
		spar=my_lambda, w=(c(100, n_sequenced[-1])))
#		spar=my_lambda, w=sqrt(c(100, n_sequenced[-1])))

	    polygon(c(ub.lines$x, rev(lb.lines$x)),
		c(ub.lines$y, rev(lb.lines$y)),
		col=my_bgcolor, border=NA)
#	    }
	}
    }

    for (variant in 1:ncol(site_freqs))
        lines(x_values, 
	    site_freqs[, variant], 
	    col=my_colors[variant], 
	    lwd=1,#/2,
            lty=ifelse(variant==1 | site_freqs[1, variant]==100, 2, 1)) 
                                    # show TF form as dashed line

    # dots accent the measured observations
    for (variant in 1:ncol(site_freqs))
	points(x_values, site_freqs[, variant],
            col=my_colors[variant], pch=21, 
	    bg='white', 
	    cex=1/3, lwd=0)

    # legend below plot
    my_xpos = c(1:ncol(site_freqs)-1/2) * 
        (my_xlim[2]-my_xlim[1])/(ncol(site_freqs))

    mtext(colnames(site_freqs),
          side=1, outer=F, cex=8/12, col=my_colors, at=my_xpos,
          line=-1/8)#, family='Courier')

    mtext(site_name, side=3, line=0, adj=1/2, cex=8/12)#, family='Courier')

    if (!is.null(site_num))
        mtext(paste0(site_num, "."), side=3, line=0, adj=0/2, cex=8/12)#, family='Courier')
}
