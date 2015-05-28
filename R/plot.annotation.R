#' @keywords internal
plot.annotation <- function(tf_loss_cutoff=NULL, 
    refseq_lut=NULL, 
    ptid=NULL,
    region=NULL,
    show_marginal_annotation=FALSE,
    y_lim=NULL) {

    if (!is.null(region) & grepl("^gp", region)) {
	Lannotation <- c(132, 185, 276, 363, 396, 451, 459, 467)
	Rannotation <- c(152, 190, 283, 373, 410, 458, 466, 471)
	annotation <- c('V1', 'V2', 'Loop D', 'CD4 Loop', 'V4', 'b23', 'V5', 'b24')
    } else if (!is.null(region) & grepl("^V", region)) {
	Lannotation <- c(26, 50, 98)
	Rannotation <- c(35, 65, 110)
	annotation <- c('H1', 'H2', 'H3')
    } else {
	Lannotation <- NULL
	Rannotation <- NULL
	annotation  <- NULL
    }

    if (is.null(y_lim))
	y_lim = (usr <- par('usr'))[c(3,4)]

    if (!is.null(refseq_lut)) {

	for (x in 1:length(annotation)) {

	    x_1 = NULL
	    try (x_1 <- refseq_lut$aln[min(which(refseq_lut$l==Lannotation[x]))], silent=T)

	    x_2 = NULL
            try (x_2 <- refseq_lut$aln[max(which(refseq_lut$r==Rannotation[x]))], silent=T)

	    if (!is.null(x_1) & !is.null(x_2))
		rect(x_1, y_lim[1], x_2, y_lim[2], border=NA, col=5, lwd=1/2)
	}

        # gp120/gp41 boundary
	x_pos = NULL
	if (!is.null(region) & grepl("^gp", region))
	    try (x_pos <- mean(refseq_lut$aln[min(which(refseq_lut$l==511))], 
		           refseq_lut$aln[max(which(refseq_lut$r==512))]), silent=T)
	if (!is.null(x_pos) & !is.null(region) & grepl("^gp", region)) segments(x_pos, y_lim[1], x_pos, y_lim[2], col=5, lwd=1/2, lty=2)

        # signal peptide
	x_pos = NULL
	try (x_pos <- mean(refseq_lut$aln[which(refseq_lut$l==30 & refseq_lut$r==30)], 
		           refseq_lut$aln[which(refseq_lut$l==31 & refseq_lut$r==31)]), silent=T)

	if (!is.null(x_pos) & !is.null(region) & grepl("^gp", region)) segments(x_pos, y_lim[1], x_pos, y_lim[2], col=5, lwd=1/2, lty=2)
#	if (!is.null(x_pos)) abline(v=x_pos, col=5, lwd=1/2, lty=3)

        ### add margin text and legend if top plot
	if (show_marginal_annotation) {

	    for (X in 1:length(annotation)) {

		l.xpos=NULL
		try (l.xpos <- refseq_lut$aln[min(which(refseq_lut$l==Lannotation[X]))], silent=T)

		r.xpos=NULL
		try (r.xpos <- refseq_lut$aln[max(which(refseq_lut$r==Rannotation[X]))], silent=T)

		if (X==6) {

		    if (!is.null(r.xpos))
			mtext(expression(paste(beta,"23", sep='')), 
			    3, at=r.xpos, line=1/2, adj=1, padj=1, cex=8/12)

		} else if (X==8) {

		    if (!is.null(l.xpos))
			mtext(expression(paste(beta,"24", sep='')), 
			    3, at=l.xpos, line=1/2, adj=0, padj=1, cex=8/12)

		} else {

		    my_padj <- ifelse(X < 4, 1/2, ifelse(X==4, 1, 0))

		    if (!is.null(r.xpos) & !is.null(r.xpos))
			mtext(annotation[X], 3, at=(l.xpos + r.xpos)/2, 
			    line=1/2, adj=1/2, padj=my_padj, cex=8/12)
		}
	    }
        }
    }

    if (!is.null(tf_loss_cutoff))  {
	abline(h=tf_loss_cutoff, col=5, lwd=1/2, lty=3)
	abline(h=100, col=5, lwd=1/2) # draw last so it is on top
    }

    if (show_marginal_annotation){# & max(y_lim) < 2) {

	mtext("TF loss, % of n sequences", side=2, outer=T, line=6/4)

	if (!is.null(region)) {
	    mtext(paste(region, "site"), side=1, outer=T, line=6/4)
	} else {
	    mtext("Site", side=1, outer=T, line=6/4)
	}

	if (!is.null(ptid))
	    mtext(side=3, ptid, outer=T, line=6/4, at=1/2)

	if (!is.null(tf_loss_cutoff)) {
	    if (tf_loss_cutoff < 100) {
		legend('right', 
		    c(paste0("Site at peak TF loss and above ", 
			    tf_loss_cutoff, "% cutoff"), 
			paste0("Site above ", tf_loss_cutoff, 
			    "% cutoff, below peak"), 
			paste0("Site included but below ", tf_loss_cutoff, 
			    "% cutoff"), 
			paste0("Site never exceeds ", tf_loss_cutoff, 
			    "% cutoff")), 
		    col=c(4:1), lwd=3/2, bty='n', cex=11/12)
	    } else {
		legend('right', 
		    c(paste0("Site at peak TF loss and above ", 
			    tf_loss_cutoff, "% cutoff"), 
			paste0("Site included but below ", 
			    tf_loss_cutoff, "% cutoff"), 
			paste0("Site never exceeds ", 
			    tf_loss_cutoff, "% cutoff")), 
		    col=c(4,2,1), lwd=3/2, bty='n', cex=11/12)
	    }
	}
    }
}
