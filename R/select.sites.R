#' @keywords internal
select.sites <- function(S) {
#tf_loss, peak_tf_loss, tf_loss_cutoff, 
#	     refseq_lut=NULL, included_sites, excluded_sites) {

    if (class(S) != "swarmtools") {
	warning("ERROR in select.sites()")
	return ( S )
    }

    if (length(S$tf_loss_cutoff) != 1 | is.null(S$tf_loss_cutoff))
	return ( S )

    if (is.null(S$tf_loss) | is.null(S$peak_tf_loss))
	return ( S )

    if (!is.null(S$included_sites) & !is.null(S$excluded_sites) & 
        length(which(S$included_sites %in% S$excluded_sites)) > 0)
            stop("select.sites ERROR: please reconsider including and excluding the same site/s")



    S$when_up <- compute.whenup(S$tf_loss, 10) # could make 10% min a variable
    ## and/or test for tf_cutoff below this 10 percent setting
    S$tf_area <- compute.tf.area(S$tf_loss)
    site_frame <- data.frame(S$peak_tf_loss, S$when_up, S$tf_area)
    colnames(site_frame) = c("peak_tf_loss", "when_up", "tf_area")

    new_site_frame <- cbind(S$refseq_lut[1:nrow(site_frame), ], site_frame)
    rownames(new_site_frame) <- rownames(S$tf_loss)

    if (!is.null(S$refseq_lut)) {

        site_epithets <- sapply(1:nrow(new_site_frame), function(s) {

            foo <- length(which(S$refseq_lut$aln <= s & 
                                S$refseq_lut$l != S$refseq_lut$r & 
                                S$refseq_lut$l == 
                                S$refseq_lut$l[which(S$refseq_lut$aln==s)]))

            ifelse(S$refseq_lut$l[s] == S$refseq_lut$r[s], "",
                   ifelse(foo <= 26, letters[foo],
                          paste0(letters[floor(foo/26)], 
                                 letters[foo-26*floor(foo/26)]))) 
        } ) # done with site_epithets

        site_names <- sapply(1:nrow(new_site_frame), function(s)
            paste0(gsub("[0-9]*$", "", rownames(new_site_frame)[s]), 
                   S$refseq_lut$l[which(S$refseq_lut$aln==s)], site_epithets[s]))

	rownames(new_site_frame) <- site_names
    }

    if (is.null(S$included_sites)) {

        if (is.null(S$excluded_sites)) {
            S$selected_sites <- subset(new_site_frame, 
                                    S$peak_tf_loss >= S$tf_loss_cutoff)
        } else {
            S$selected_sites <- subset(new_site_frame, 
                                   S$peak_tf_loss >= S$tf_loss_cutoff & 
                                   !c(1:nrow(S$tf_loss)) %in% S$excluded_sites)
        }
    } else {
        if (is.null(S$excluded_sites)) {
            S$selected_sites <- subset(new_site_frame, 
                                     S$peak_tf_loss >= S$tf_loss_cutoff | 
                                    c(1:nrow(S$tf_loss)) %in% S$included_sites)
        } else {
            S$selected_sites <- subset(new_site_frame, 
                                     (S$peak_tf_loss >= S$tf_loss_cutoff | 
                                    c(1:nrow(S$tf_loss)) %in% S$included_sites)
                                 & !c(1:nrow(S$tf_loss)) %in% S$excluded_sites)
        }
    }

#    site_order <- order(selected_sites$tf_area)
    site_order <- order(S$selected_sites$when_up, 
    	       S$selected_sites$tf_area, na.last=T)

    S$selected_sites <- S$selected_sites[site_order, ]

    return ( S )
}
