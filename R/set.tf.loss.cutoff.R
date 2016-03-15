#' Define the threshold value, above which sites are considered selected
#'
#' @param S swarmtools object
#' @param cutoff cutoff threshold value, from 0 to 100
#' @param fwu sites are sorted by when they first reach this value, from 0 to 100 and <= c.
#' @return swarmtools object with list of sites in selected_sites if possible
#' @export
set.tf.loss.cutoff <- function(S, cutoff, fwu=10) {

    if (class(S) != "swarmtools") {
	warning("ERROR in set.tf.loss.cutoff()")
	return ( S )
    }

    ## may need to sanitize input
    if (!is.numeric(cutoff)) {
	warning("ERROR in set.tf.loss.cutoff(): Please provide a numeric value.")
	return ( S )
    }

    if (length(cutoff) != 1) {
	warning("ERROR in set.tf.loss.cutoff(): Please provide a single numeric value.")
	return ( S )
    }

    if (cutoff < 0 | cutoff > 100) {
	warning("ERROR in set.tf.loss.cutoff(): Please provide a numeric value from 0 to 100.")
	return ( S )
    }

    S$tf_loss_cutoff <- cutoff

    if (is.null(S$frequency_when_up))
        S$frequency_when_up <- fwu
    else if (S$frequency_when_up != fwu)
        S$frequency_when_up <- fwu

    S <- select.sites(S)

#    S$n_selected_sites <- ifelse(is.null(S$selected.sites), 0, nrow(S$selected_sites))

    return ( S )
}
