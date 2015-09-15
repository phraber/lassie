#' @keywords internal
plot.tf.loss.vector <- function(tf_loss_vector, 
    peak_tf_loss,
    tf_loss_cutoff=NULL,
    ptid=NULL,
    region=NULL,
    refseq_lut=NULL,
    show_marginal_annotation=F,
#    tf_loss_cutoff=NULL,
    my_timepoint=NULL, 
    x_lim=c(1, length(tf_loss_vector)), 
    y_lim=c(0,100),
    n_sequences=NULL) {

# my_xlim, my_ylim
# annotaton
# hxb2

    nonzero_tf_loss <- which(tf_loss_vector > 0.001)

    my_colors <- rep("transparent", length(tf_loss_vector))
    my_colors[nonzero_tf_loss] = 1

    if (!is.null(tf_loss_cutoff)) {

        # site eventually passes cutoff, but not now
	my_colors[which(tf_loss_vector < tf_loss_cutoff &
	    peak_tf_loss >= tf_loss_cutoff)] = 2

        # site passes cutoff, but not peak
	my_colors[which(tf_loss_vector < peak_tf_loss &
	    tf_loss_vector >= tf_loss_cutoff)] = 3

	my_colors[which(tf_loss_vector == peak_tf_loss &
	    tf_loss_vector >= tf_loss_cutoff)] = 4
    }

    plot(c(1:length(tf_loss_vector)), tf_loss_vector, xlab='', ylab='', 
	type='n', xlim=x_lim, ylim=y_lim, frame.plot=F, yaxt='n')

 	annotate.region(tf_loss_cutoff=tf_loss_cutoff, 
                            refseq_lut=refseq_lut, 
                            ptid=ptid, 
                            region=region, 
                            show_marginal_annotation=show_marginal_annotation,
                            y_lim=y_lim)

    fin=par('fin')

my_lwd = fin[1]*72/(3/2*length(tf_loss_vector))

    segments(nonzero_tf_loss, rep(0, length(nonzero_tf_loss)), 
	nonzero_tf_loss, tf_loss_vector[nonzero_tf_loss], 
	col=my_colors[nonzero_tf_loss], lwd=my_lwd)

    axis(2, 100*c(0:4)/4, c("", "25", "", "75", ""), line=1/10, las=1,yaxt='s')
    axis(4, 100*c(0:4)/4, c("0", "", "50", "", ""), line=1/10, las=1, yaxt='s')

    my.padj=ifelse(is.null(tf_loss_cutoff) & tf_loss_cutoff > 4/5, 1, 0)

    usr=par('usr')
## need to consider that usr[4] may be < usr[3]
    if (usr[3] > usr[4]) {
	tmp = usr[3]
	usr[3] = usr[4]
	usr[4] = tmp
	rm(tmp)
    }

    if (!is.null(tf_loss_cutoff)) {
	y_pos = usr[3] + (usr[4] - usr[3])*(tf_loss_cutoff/100)
    } else {
	y_pos = usr[3] + (usr[4] - usr[3])/100
    }

#    y_pos = usr[4] - (usr[4] - usr[3])/20
    x_pos = usr[1] + (usr[2] - usr[1])/50

    if (!is.null(my_timepoint) & !is.null(n_sequences))
	text(x_pos, y_pos,
	    paste0("t=", gsub("TF", "0", # TO DO: Clean up TF name!
 		gsub("^d(0)*", "", my_timepoint)), " dpi, ", # ugh
		"n=", n_sequences), adj=c(0,my.padj), cex=12/12)
}

