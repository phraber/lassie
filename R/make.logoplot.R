#' @keywords internal
make.logoplot <- function(selected_sites, working_swarm, included, 
    prefix=prefix, 
    dotify=T, hide_xlabels=F, 
    stack_width=18,
    stacks_per_line=NULL,
    aspect_ratio=1.5, 
    y_label=NULL,
    show_sample_size=F,
#    sequence_multiplicity=NULL,
    logo_format="pdf") {

    my_region <- ''

    if (!logo_format %in% c('png', 'eps', 'pdf', 'svg', 'jpeg'))
	stop('ERROR in make.logoplot(): Invalid logo_format value')

    # x-axis labels
    site_string <- paste(gsub("^[A-Z-]", "", rownames(selected_sites)), 
	collapse=",")

    if (is.null(stacks_per_line))
        stacks_per_line = nrow(selected_sites)

    if (hide_xlabels)
	site_string <- gsub(" ", "", seqinr::c2s(rep(",", -1+nrow(selected_sites))))

#last_field <- length(unlist(strsplit(prefix, "[/]")))
#tail_of_results_prefix <- unlist(strsplit(prefix, "[/]"))[[last_field]]

#    logo_prefix <- paste0(prefix, "percentTFloss-", 
#	nrow(selected_sites), "sites-", 
#	length(which(working_swarm$is_included)), "clones")

      out_file = tempfile(pattern=prefix, 
	  fileext=paste0('.', logo_format))

    fasta_file = tempfile(pattern=prefix, fileext='.fasta')

    if (dotify) {

        write.fasta.file(fasta_file, 
	    working_swarm$dotseq_concatamer[included])#,
#	    multiplicity=sequence_multiplicity)
    } else {

        write.fasta.file(fasta_file, 
	    working_swarm$seq_concatamer[included])#, 
#	    multiplicity=sequence_multiplicity)
    }

    ylab_string = ifelse(is.null(y_label), " --ylabel 'proportion'", 
	paste0(" --ylabel '", y_label, "'"))

    fine_print = ""

    if (hide_xlabels) {
        system(paste0("weblogo",
	    " --alphabet 'ACDEFGHIKLMNOPQRSTUVWYBJZX*-#.'",
	    " --stack-width ", stack_width,
	    " --aspect-ratio ", aspect_ratio,
	    " --composition 'none'",
	    " --units 'probability'",
	    " --errorbars NO --reverse-stacks NO",
            " --fineprint '", fine_print, "'",
	    " --format ", logo_format, 
	    " --stacks-per-line ", stacks_per_line,
	    ylab_string,
	    " --xlabel ''", 
	    " --annotate '", site_string, "'",
	    " -c charge < ", 
	    fasta_file, " > ", out_file))

    } else {

        system(paste0("weblogo",
	    " --alphabet 'ACDEFGHIKLMNOPQRSTUVWYBJZX*-#.'",
	    " --stack-width ", stack_width,
	    " --aspect-ratio ", aspect_ratio,
	    " --composition 'none'",
	    " --units 'probability'",
	    " --errorbars NO --reverse-stacks NO",
            " --fineprint '", fine_print, "'",
	    " --format ", logo_format, 
	    " --stacks-per-line ", stacks_per_line,
	    ylab_string,
#	    " --xlabel '", paste(my_region, "site'"),
# TO DO: Pass xlabel and ylabel values
	    " --xlabel ''",
	    " --annotate '", site_string, "'",
	    " -c charge < ", 
	    fasta_file, " > ", out_file))

    }

    return ( out_file )
}
