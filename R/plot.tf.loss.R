### LICENSING INFO GOES HERE

#' @keywords internal
plot.tf.loss = function(S, 
    my_palette=NULL,
    plotfile_type='pdf',
    outfile_prefix=NULL,
    verbose=T) {

    if (class(S) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to plot.tf.loss()!")

    if (is.null(outfile_prefix)) {

        if (is.null(S$aas_prefix)) {

	    if (!is.null(S$ptid)) {

	        if (!is.null(S$my_region)) {
		    my_prefix = paste0(S$ptid, "-", S$region, '-')
	        } else {
		    my_prefix = paste0(S$ptid, "-")
	        }
            }
        } else {

	    my_prefix = S$aas_prefix
        }
    } else {
	my_prefix = outfile_prefix
    }

    for (i in length(S$tf_loss_cutoff)) {

	out_file=paste0(my_prefix, '-', S$tf_loss_cutoff[i], '.',plotfile_type)

	if (plotfile_type == "pdf") {

	    pdf(out_file, paper='US', width=8.5, height=11, useDingbats=F, 
		pointsize=12)

	} else {

	    postscript(out_file, width=6.75, height=8, pointsize=12,
		bg='white', horizontal=F)
	}

	layout(matrix(c(1:length(colnames(S$tf_loss)[-1])), nc=1))

	if (!is.null(my_palette))
	    palette(my_palette)

	par(oma=c(8/4, 8/4, 8/4, 8/4))
	par(mar=c(0/8, 0, 0/8, 0))
	par(mgp=c(5/4, 1/4, 0/5))
	par(cex.axis=12/12)
	par(cex.lab=1)
	par(lend=1, ljoin=1)
	par(tcl=-1/5)
	par(xaxt='n')
	par(xaxs='i')
	par(yaxs='i')
	par(las=0)

	plot.tf.loss.matrix(S, which_tf_loss_cutoff=i)

	dev.off()

	if (verbose)
	    system(paste("open", out_file))
    }
}
