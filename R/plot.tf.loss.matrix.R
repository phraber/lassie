#' @keywords internal
plot.tf.loss.matrix <- function(S,
    which_tf_loss_cutoff=1,
    verbose=F) {

    if (class(S) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to plot.tf.loss.matrix()")

    if (!is.numeric(which_tf_loss_cutoff) | which_tf_loss_cutoff < 1 | 
	which_tf_loss_cutoff > length(S$tf_loss_cutoff))
            stop("ERROR in plot.tf.loss.matrix(): Please specify a valid index to tf_loss_cutoff")

    # iteratively call plot.tf.loss.vector per column (timepoint)

    x_offset <- nrow(S$tf_loss)/100 
        # padding around extreme values of x (env position)
    my_xlim <- c(1-x_offset, nrow(S$tf_loss)+x_offset)
    my_ylim <- 100*c(0/5, 20/20)

#    peak_tf_loss_vector = compute.peak.tf.loss(tf_loss)

    ## iterate over sample timepoints

    for (j in 2:ncol(S$tf_loss)) # skip earliest timepoint
        # NB: ^ this depends on dimensionality used for the layout()

	plot.tf.loss.vector(tf_loss_vector=S$tf_loss[,j], 
	    peak_tf_loss=S$peak_tf_loss,
	    tf_loss_cutoff=S$tf_loss_cutoff[which_tf_loss_cutoff],
	    ptid=S$ptid, 
	    region=S$region,
	    refseq_lut=S$refseq_lut,
	    show_marginal_annotation=(j==2 & !is.null(S$region) & 
		grepl("^gp", S$region)), # THIS (j==2) IS ALSO AN ASSUMPTION
#	    tf_loss_cutoff=tf_loss_cutoff, #=my_tf_loss_cutoff,
	    my_timepoint=colnames(S$tf_loss)[j],
	    x_lim=my_xlim, 
	    y_lim=my_ylim,
	    S$n_per_timepoint[[j]])

#    par(xaxt='s')

    # label x axis only on last panel:
# TO DO: wrap the refseq_lut refs in case it is null

    tick_interval = ifelse(my_xlim[2] >= 3000, 1000, 
	ifelse(my_xlim[2] >= 300, 100, 
	    ifelse(my_xlim[2] >= 200, 50,
		ifelse(my_xlim[2] >= 150, 30,
		    ifelse(my_xlim[2] >= 80, 20,
			ifelse(my_xlim[2] >= 40, 10, 
			    ifelse(my_xlim[2] >= 20, 5, 1)))))))

    x_locs <- which(c(my_xlim[1]:my_xlim[2]) %% tick_interval == 0)

    if (!is.null(S$refseq_lut))
	try (x_locs <- S$refseq_lut$aln[which(S$refseq_lut$l == 
	    S$refseq_lut$r & S$refseq_lut$l %% tick_interval == 0)], silent=T)

    x_labs <- x_locs
    if (!is.null(S$refseq_lut))
	try (x_labs <- S$refseq_lut$l[which(S$refseq_lut$l == 
	    S$refseq_lut$r & S$refseq_lut$l %% tick_interval == 0)], silent=T)

### to do: think about what to do when my_lim %in% x_locs
    axis(1, at=c(my_xlim[1], x_locs, my_xlim[2]), labels=c("", x_labs, ""), 
	line=1/8, xaxt='s')

}
