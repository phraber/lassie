#' @keywords internal
annotate.region <- function(tf_loss_cutoff=NULL, 
    refseq_lut=NULL, 
    ptid=NULL,
    region=NULL,
    show_marginal_annotation=F,
    y_lim=NULL) {

#x, notes=NULL, y_lim=NULL) {

    message("*** annotate.region ***\n")

    clr <- "#88888844"

    if (is.null(notes)) {
	notes <- list()
	notes$Lhs <- c(132, 185, 276, 396, 459, 511, 30)
	notes$Rhs <- c(152, 190, 283, 410, 466, 512, 31)
	notes$txt <- c('V1', 'V2', 'Loop D', 'V4', 'V5', '', '')
#	notes$Lhs <- c(132, 185, 276, 363, 396, 459, 511, 30)
#	notes$Rhs <- c(152, 190, 283, 373, 410, 466, 512, 31)
#	notes$txt <- c('V1', 'V2', 'Loop D', 'CD4 Loop', 'V4', 'V5', '', '')
    } else {
	if (is.null(notes$Lhs) | is.null(notes$Rhs))
	    return
	if (!is.numeric(notes$Lhs) | !is.numeric(notes$Rhs))
	    return

	if (!is.null(notes$txt))
	    if (!character(notes$col))
		return

	if (!is.null(notes$col))
	    clr <- notes$col
    }

#	} else if (!is.null(region) & grepl("^V", region)) {
#	    Lnotes <- c(26, 50, 98)
#	    Rnotes <- c(35, 65, 110)
#	     notes <- c('H1', 'H2', 'H3')
#	} else {
#	    Lnotes <- NULL
#	    Rnotes <- NULL
#	     notes <- NULL
#	}
#    }

    if (is.null(y_lim)) y_lim = (usr <- par('usr'))[c(3,4)]

    if (!is.null(refseq_lut)) {

 	for (x in 1:length(notes$txt)) {

 	    x_1 = NULL
	    x_1 <- refseq_lut$aln[min(which(refseq_lut$l==notes$Lhs[x]))]

 	    x_2 = NULL
	    x_2 <- refseq_lut$aln[max(which(refseq_lut$r==notes$Rhs[x]))]

 	    if (!is.null(x_1) & !is.null(x_2) & !is.na(x_1) & !is.na(x_2))
 		rect(x_1, y_lim[1], x_2, y_lim[2], border=NA, col=clr)
 	}

        ### annotate text at top of plot here so we don't draw boxes over text
 	if (!is.null(notes$txt)) {

 	    for (i in 1:length(notes$txt)) {

 		l.xpos <- NULL
		l.xpos = refseq_lut$aln[min(which(refseq_lut$l==notes$Lhs[i]))]

 		r.xpos <- NULL
		r.xpos = refseq_lut$aln[max(which(refseq_lut$r==notes$Rhs[i]))]

		usr <- par('usr')

                #  consider inverted y-axis values
		y.pos <- ifelse(usr[3] < usr[4],
		    usr[4] - (usr[4] - usr[3])*0.995,
		    usr[3] - (usr[3] - usr[4])*0.995)

		n_s <- ifelse(usr[3] < usr[4], 3, 1) # north or south?

 		if (!is.null(l.xpos) & !is.null(r.xpos) &
		      !is.na(l.xpos) & !is.na(r.xpos))
 		    text(mean(c(l.xpos, r.xpos)), y.pos, notes$txt[i], pos=n_s,
			cex=2/3)
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
