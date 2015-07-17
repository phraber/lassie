#' Summarize and plot the frequency dynamics per site.
#'
#' The option \code{color_lut_type} is used to specify alternative
#' columns from the defined color scheme (see
#' \code{help(pixmap_colors)}).  For example, 'taylor' in the vignette
#' uses the colors described in Wittgensteinean fashion by W. Taylor,
#' Protein Engineering, Vol 10 , 743-746 (1997).
#'
#' @param S swarmtools object, with list of selected sites.
#' @param min_variant_count Disregard mutations that appear fewer than this 
#' number of times across the entire alignment.
#' @param conf_int If true, 95\% confidence intervals will be computed on 
#' frequencies from sample counts using the binomial distribution.  Requires 
#' package(binom).
#' @param col_min Only show variants that ever exceed this minimum value.
#' @param is_time_in_weeks If true, labels plot x-axis accordingly.  
#' Otherwise, time units are assumed to be days.  This influences the annual 
#' grey lines and labels that appear in the plots.
#' @param color_lut_type Optional string for color lookup table, currently only
#' implemented for amino acids as 'aa', 'charge' and 'taylor'.
#'
#' @return NULL if no plot made, 1 if plot was made
#' @examples
#' \dontrun{
#' A <- lassie::swarmtools(aas_file=system.file("extdata", "CH505-gp160.fasta",
#'             package="lassie"), tf_loss_cutoff=80)
#' outfile='variant-frequencies-min-10.pdf'
#' pdf(outfile, paper='USr', width=11, height=8.5, useDingbats=F, pointsize=10)
#' par(mar=c(2,2,0,0), oma=c(0,0,1,1), lend=1, ljoin=1, xaxt='n', yaxt='n')
#' layout(matrix(1:35, ncol=7, byrow=T))
#' report.variant.frequencies(A, conf_int=T, is_time_in_weeks=F, col_min=10)
#' dev.off()
#' system(paste("open", outfile))
#' }
#'
#' @export
report.variant.frequencies <- function(S, min_variant_count=2, conf_int=F, 
    col_min=10, is_time_in_weeks=T, color_lut_type='charge') {

    if (class(S) != "swarmtools")
        stop("ERROR: Please pass a swarmtools object to compute.variant.frequencies()")

    if (is.null(S$tf_loss_cutoff))
       return ( NULL )

    if (is.null(S$selected_sites))
       return ( NULL )

    if (!color_lut_type %in% colnames(aa.col))
	color_lut_type = 'charge'

    compute.variant.frequencies(S, min_variant_count,#=min_variant_count,
                                conf_int,#=conf_int, 
				col_min,#=col_min,
                                is_time_in_weeks,#=is_time_in_weeks, 
				color_lut_type)

    return ( 1 )
}
