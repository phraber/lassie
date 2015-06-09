#' Define the threshold value, above which sites are considered selected

#' @param S swarmtools object
#' @param c cutoff threshold value, from 0 to 100
#' @return swarmtools object with list of sites in selected_sites if possible
#' @export
set.tf.loss.cutoff <- function(S, c) {

    if (class(S) != "swarmtools") {
	warning("ERROR in set.tf.loss.cutoff()")
	return ( S )
    }

    ## may need to sanitize input
    if (!is.numeric(c)) {
	warning("ERROR in set.tf.loss.cutoff(): Please provide a numeric value.")
	return ( S )
    }

    if (length(c) != 1) {
	warning("ERROR in set.tf.loss.cutoff(): Please provide a single numeric value.")
	return ( S )
    }

    if (c < 0 | c > 100) {
	warning("ERROR in set.tf.loss.cutoff(): Please provide a numeric value from 0 to 100.")
	return ( S )
    }

    S$tf_loss_cutoff <- c

    S <- select.sites(S)

#    S$n_selected_sites <- ifelse(is.null(S$selected.sites), 0, nrow(S$selected_sites))

    return ( S )
}
