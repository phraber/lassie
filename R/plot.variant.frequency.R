#' @keywords internal
plot.variant.frequency <- function(site.freqs, site.counts, n_sequenced, 
                                   site_name, label_axes, conf_int,
                                   tf_loss_cutoff, tf_index, tf_aa,
                                   is_time_in_weeks, 
                                   site_num, lut, annotate_env, max.time,
                                   do_barplot, stack_barplot, 
                                   barplot_width, is_tf_hidden, ...) {

    panel.grey <- "grey67"#, seqinr::col2alpha("black", alpha=4/5)

    if (is.vector(site.freqs) || (is.array(site.freqs) && 
                                  (length(dim(site.freqs)) == 1))) {
        site.freqs <- cbind(site.freqs)
        colnames(site.freqs) <- tf_aa
    }

    plot.freqs <- site.freqs
    rownames(plot.freqs) = rownames(site.freqs)
    colnames(plot.freqs) = colnames(site.freqs)

    if (is.vector(site.counts) || (is.array(site.counts) && 
                                  (length(dim(site.counts)) == 1))) {
        site.counts <- cbind(site.counts)
        colnames(site.freqs) <- tf_aa
    }

    plot.counts <- site.counts
    rownames(plot.counts) = rownames(site.counts)
    colnames(plot.counts) = colnames(site.counts)

    x.values <- as.numeric(gsub("[A-Z]", "", rownames(site.freqs), ignore.case=T))
    x.max = ifelse(is.null(max.time), max(x.values), max.time)
    x.min = ifelse(is_tf_hidden, min(x.values[-tf_index]), min(x.values))

    panel.xlim <- c(x.min, x.max)

    if (!is.null(max.time)) {
        if (any(x.values > max.time)) {
            plot.freqs <- plot.freqs[-which(x.values > max.time), ]
            plot.counts <- plot.counts[-which(x.values > max.time), ]
            n_sequenced <- n_sequenced[-which(x.values > max.time)]
        }
    }
    if (is.vector(plot.freqs) || (is.array(plot.freqs) && 
                                  (length(dim(plot.freqs)) == 1))) {
        plot.freqs <- cbind(plot.freqs)
        colnames(plot.freqs) <- colnames(site.freqs)[1]
    }
    if (is.vector(plot.counts) || (is.array(plot.counts) && 
                                   (length(dim(plot.counts)) == 1))) {
            plot.counts <- cbind(plot.counts)
            colnames(plot.counts) <- colnames(site.counts)[1]
    }
    if (is_tf_hidden) {
        plot.freqs <- plot.freqs[-tf_index, ]
        plot.counts <- plot.counts[-tf_index, ]
        n_sequenced <- n_sequenced[-tf_index]
    }
    if (is.vector(plot.freqs) || (is.array(plot.freqs) && 
                                  (length(dim(plot.freqs)) == 1))) {
        plot.freqs <- cbind(plot.freqs)
        colnames(plot.freqs) <- colnames(site.freqs)[1]
    }
    if (is.vector(plot.counts) || (is.array(plot.counts) && 
                                   (length(dim(plot.counts)) == 1))) {
        plot.counts <- cbind(plot.counts)
        colnames(plot.counts) <- colnames(site.counts)[1]
    }

    panel.colors = aa.col[tolower(colnames(plot.freqs)), lut]
    panel.ylim <- c(-2,102)

    if (do_barplot) {

        bar.xpos<- barplot(t(plot.freqs), 
                           axisnames=T, 
                           names.arg=rownames(plot.freqs), #label_axes, 
                           ylim=panel.ylim, # xlim=panel.xlim,
                           beside=!stack_barplot, 
                           yaxp=c(0, 100, 4), las=1,
                           width=barplot_width, 
                           col=panel.colors, 
                           border='white',
                           legend.text=T, mgp=c(1/4, 1/8, 0), 
                           args.legend=list(x=0, y=-12,#-7.5,
                                 col='transparent',
                                 border=NA,
                                 fill=NA,
                                 pch=1,
                                 bty='n', horiz=T, xpd=T, 
                                 text.col=panel.colors,
                                 xjust=0, yjust=1/2), ...)

        panel.xlim=range(c(bar.xpos))

    } else {
        plot(0, 0, type='n', xlab='', ylab='', xlim=panel.xlim, 
             ylim=panel.ylim, frame.plot=F, xaxp=c(panel.xlim, 3), ...)
    } # done with if(do_barplot)/else

    abline(h=c(0:4 * 25), lwd=1/2, col=panel.grey, lty=1)

    # update this because we may have edited the data matrix above
    x.values <- as.numeric(gsub("[A-Z]", "", rownames(plot.freqs), ignore.case=T))
    x.mult <- ifelse(is_time_in_weeks, 52, 365.25)
    floormax.time.inYears = floor(max(x.values) / x.mult)

    if (!do_barplot) {
        if (floormax.time.inYears > 0)
            abline(v=0:floormax.time.inYears * x.mult, lwd=1/2, 
                   col=panel.grey)

        if (label_axes & floormax.time.inYears > 0)
            mtext(paste0("Y", 0:floormax.time.inYears), 1, 
                  at=0:floormax.time.inYears * x.mult, line=0, 
                  cex=8/12, col=panel.grey)

        if (label_axes)
            mtext(c("25%", "75%"), 2, at=c(1, 3)*25, line=0, cex=8/12, 
              col=panel.grey)

# iterate over variants found in this position
# because the first things drawn are covered by things drawn afterwards,
# iteration over variants is done per things to be drawn, rather than 
# one big loop for all variant forms.  This is to minimize obscuring
# lines or points/dots for earlier variants (TF) with polygons from
# later by first drawing all the CI polygons, 

    if (conf_int) {

        tmp.n_sequenced = n_sequenced
        if (!is_tf_hidden) {
            tmp.n_sequenced[1] = 2*max(n_sequenced) # assumes that the lowest-order timepoint contains tf counts
            plot.counts[1, which(colnames(plot.counts) == tf_aa)] = tmp.n_sequenced[1] # this too is riddled with assumptions
        }

        for (variant in c(1:ncol(plot.freqs))) {

            panel.bgcolor = seqinr::col2alpha(panel.colors[variant], alpha=1/5)

            cis = binom::binom.confint(plot.counts[, variant], tmp.n_sequenced, 
                                       method='e')

            if (!is_tf_hidden & colnames(plot.freqs)[variant] == tf_aa) {
                # TF (timepoint 1) is inferred by other means so assert its accuracy
                cis$lower[1] = plot.freqs[1, variant] / 100
                cis$upper[1] = plot.freqs[1, variant] / 100
            }

            # spline fit the CIs to reduce spiky effects
            panel.lambda = 1/4 # spline smoothing parameter

            if (length(x.values) >= 4) {


                # weigh each frequency by number of sequences or sqrt of number of sequences

                ub.lines = smooth.spline(#c(plot.freqs[1, variant], 
                                           100*cis$upper ~ x.values, 
                                         spar=panel.lambda, 
                                         w=tmp.n_sequenced)

                lb.lines = smooth.spline(#c(plot.freqs[1, variant], 
                                           100*cis$lower ~ x.values,
                                         spar=panel.lambda, 
                                         w=tmp.n_sequenced)

            } else {
                # these literal values are provided if spline is undesirable
                 ub.lines <- list(x=x.values, y=100*cis$upper)
                 lb.lines <- list(x=x.values, y=100*cis$lower)
            }

            polygon(c(ub.lines$x, rev(lb.lines$x)),
                    c(ub.lines$y, rev(lb.lines$y)),
                    col=panel.bgcolor, border=NA)
        }
    }

    for (variant in 1:ncol(plot.freqs))
        lines(x.values, plot.freqs[, variant], 
              col=panel.colors[variant], lwd=1,
              lty=ifelse(colnames(plot.freqs)[variant] == tf_aa, 2, 1))
# show TF form as dashed line

    # dots accent the measured observations
    for (variant in 1:ncol(plot.freqs))
        points(x.values, plot.freqs[, variant],
               col=panel.colors[variant], pch=21, 
               bg='white', 
               cex=1/3, lwd=0)

    # legend below plot
    panel.xpos = c(1:ncol(plot.freqs)-1/2) * 
        (panel.xlim[2]-panel.xlim[1])/(ncol(plot.freqs))

    if (!is.null(colnames(plot.freqs)))
        mtext(colnames(plot.freqs),
              side=1, outer=F, cex=8/12, col=panel.colors, at=panel.xpos,
              line=1/2)

    # name site at top left of each panel
    mtext(ifelse(is.null(site_num), site_name, 
                 paste0(site_num, ". ", site_name)), 
          side=3, line=0, adj=0/2, cex=8/12)

    # add env/mAb functional annotation if specified
    if (annotate_env) {

        HXB2.position <- as.numeric(gsub("^[A-Z]", "", gsub("[a-z]$", "",
                                    unlist(strsplit(site_name, " "))[[1]])))

        my.features = NULL

        if (any(env.features[HXB2.position, ])) {

            my.features <- 
                colnames(env.features)[which(env.features[HXB2.position, ])]

            if (!is.null(my.features))
                mtext(paste(sort(ifelse(length(my.features) > 2,
			    sample(my.features, 2), my.features)), 
			collapse=","), 
                    font=2, side=3, line=1/4, adj=1, padj=0, cex=6/12, 
                      family='Courier')
        }

        my.features.L = NULL
        my.features.R = NULL

        if (any(env.features[HXB2.position-1, ]))
            my.features.L <- 
            colnames(env.features)[which(env.features[HXB2.position-1, ])]

        if (any(env.features[HXB2.position+1, ]))
            my.features.R <- 
            colnames(env.features)[which(env.features[HXB2.position+1, ])]

        my.features = unique(c(my.features.L, my.features.R))

        if (!is.null(my.features))
            mtext(paste(sort(ifelse(length(my.features) > 2,
			    sample(my.features, 2), my.features)), 
		    collapse=","), 
		font=1, side=3, line=0, adj=1, padj=1, cex=6/12, 
                family='Courier', col='grey35')

    }
  }
}
