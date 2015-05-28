#' @keywords internal
set.included.sites <- function(S, site_list) {

    if (class(S) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to set.included.sites()")

    ### TO DO: Sanitize then parse site_list if it is.character()

    S$included_sites <- site_list

    if (is.null(S$aas_aln) | is.null(S$refseq_lut))
        S <- prep.aln(S)

### TO DO: ensure workflow will proceed if this is the last thing set by ui
#    if (!is.null(S$tf_loss_cutoff))
#        S <- select.sites(S)

    return ( S )
}
