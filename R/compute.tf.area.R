#' @keywords internal
compute.tf.area <- function(tf_loss_matrix, invert=FALSE) {

    my_tpis = as.numeric(gsub("^[A-Z]", "", colnames(tf_loss_matrix), 
	ignore.case=T))

    deltaT = c(0, my_tpis[-1] - my_tpis[1:(-1+length(my_tpis))])

    tf_area = rep(NA, nrow(tf_loss_matrix))

    for (i in 1:nrow(tf_loss_matrix)) {
	if (invert) {
	    tf_area[i] = sum(tf_loss_matrix[i,] * deltaT, na.rm=T)
	} else {
	    tf_area[i] = sum((100-tf_loss_matrix[i,]) * deltaT, na.rm=T)
	}
    }

    tf_area
}
