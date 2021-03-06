#' Identify sites under putative immune selection.
#' 
#' Given a serially sampled protein sequence alignment, this will list sites where loss of transmitted-founder (TF) form exceeds a cutoff value within a given sample timepoint.
#'
#' The sample timepoints should be part of the sequence names and it should be possible to extract them by splitting sequence names with a particular separator and taking one of the resulting fields as the timepoint label.  TF loss is computed among sequences that have the same timepoint label.  By default, the timepoint label is in the first dot-delimited field.  However, you can specify alternatives using lassie::create.timepoint.parser() to specify how fields are separated and which field to use, then passing the returned function to swarmtools using the 'timepoints_parser' option.  
#' 
#' Timepoint labels can be in any units and are assumed to indicate time elapsed since infection.  
#' A mixture of different units is not advised.  
#' The values are used primarily when reordering sites and plotting frequency dynamics:
#' lassie::report.variant.frequencies()
#' After parsing the correct field, any leading character (e.g. "d123" or "56dpi") will be stripped and the remainder converted to a numeric value (123 and 56).
#'
#' You should provide at least the name of an alignment file or an alignment matrix (via seqinr::as.alignment.matrix()) 
#' already in memory, and specify a percentage value (0-100) for tf_loss_cutoff.  Without these, no sites will be returned.
#' This is not an error state, as the returned swarmtools object could be passed to lassie::set.alignment.file() and 
#' lassie::set.tf.loss.cutoff().
#'
#' @param aas_aln Alignment matrix.
#' @param aas_file Alignment file.
#' @param alignment_format Format of alignment file/s; must be one of these: \code{"fasta"}, \code{"clustal"}, \code{"phylip"}, \code{"msf"}, or \code{"mase"}.
#' @param tf_index TF index
#' @param tf_name TF name
#' @param timepoints_parser Timepoints parsing function generated by lassie::create.timepoint.parser()
#' @param refseq_lut_file Reference sequence LUT file
#' @param refseq_lut Reference sequence lookup table.  Optional, this could help to apply reference sequence numbering from a separate alignment that does not contain the reference sequence.
#' @param refseq_name Reference sequence name, used for numbering, e.g. HXB2.
#' @param pngs2o Switch to mark asparagines (N) in PNG motifs as O.
#' @param tf_loss_cutoff Threshold value (or vector of values) for including a site.
#' @param frequency_when_up Sites are sorted by when they first reach this value.
#' @param included_sites List of included sites.
#' @param excluded_sites List of excluded sites.
#' @param exclude_vloops Automagically add hypervariable loop sites to list of excluded sites.
#'
#' @return swarmtools object
#'
#' @examples
#' \dontrun{
#'   A <- lassie::swarmtools(aas_file=system.file("extdata", "CH505-gp160.fasta", 
#'             package="lassie"), tf_loss_cutoff=80)
#'   B <- lassie::swarmset(A)
#' }
#' @export

swarmtools <- function(
    aas_aln=NULL,
    aas_file=NULL, 
    alignment_format="fasta", 
    tf_index=1,
    tf_name=NULL,
    timepoints_parser=NULL,
    refseq_lut_file=NULL,
    refseq_lut=NULL,
    refseq_name="HXB2",
    pngs2o=T,
    tf_loss_cutoff=NULL, # include sites with tf_loss at or above (>=) cutoff
    frequency_when_up=10,
    exclude_vloops=F,
    included_sites=NULL,
    excluded_sites=NULL) {

    if (is.null(timepoints_parser))
	timepoints_parser <- create.timepoint.parser(".", 1)

    retval <- list(aas_aln=NULL,
	aas_file=NULL, # this gets set below and triggers prep.aln
	pngs2o=pngs2o,
	alignment_format=alignment_format,
	n_per_timepoint=NULL,
	timepoints_parser=timepoints_parser,
	original_seqnames=NULL,
	tf_index=tf_index,
	tf_name=tf_name,
	is_tf=NULL, #is_tf,
	tf_loss=NULL, #tf_loss,
	peak_tf_loss=NULL, #peak_tf_loss,
	refseq_lut=refseq_lut,
	refseq_lut_file=NULL, # set below: refseq_lut_file
	refseq_name=refseq_name,
	tf_loss_cutoff=NULL, # set below: tf_loss_cutoff
	frequency_when_up=frequency_when_up,
	selected_sites=NULL,
        included_sites=NULL,
        excluded_sites=NULL,
        exclude_vloops=exclude_vloops
    )

    class(retval) <- "swarmtools"

    ### TO DO: invoke accessor methods for defined attributes
    if (!is.null(excluded_sites))
	retval <- set.excluded.sites(retval, excluded_sites)

    if (!is.null(included_sites))
	retval <- set.included.sites(retval, included_sites)

    # TO DO: make refseq lut optional - where is it required?
    # could simply use TF sequence instead
    if (!is.null(refseq_lut_file) & is.null(refseq_lut))
        retval$refseq_lut_file = refseq_lut_file

    ### This attempts to parse timepoint labels from sequence names
    ### before refseq is excluded from the alignment
    if (!is.null(aas_file) | !is.null(aas_aln))
	retval <- set.alignment(retval, aas_aln, aas_file)

# set.alignment calls prep.aln
# prep.aln calls set.refseq
# set.refseq calls excise.refseq
# prep.aln calls set.tf

    if (!is.null(tf_loss_cutoff))
	retval <- set.tf.loss.cutoff(retval, tf_loss_cutoff, frequency_when_up)
# select.sites is called by set.tf.loss

    retval
}
