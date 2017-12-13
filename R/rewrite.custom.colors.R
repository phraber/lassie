#' @keywords internal
rewrite.custom.colors <- function(x, old_file) {

    if (class(x) != "swarmset")
	stop("ERROR in make.timepoint.logos(): Invalid swarmset object")

    if (is.null(old_file) | !file.exists(old_file))
       return (old_file)

    new_file <- tempfile()

    colors_input <- NULL
    try(colors_input <- read.csv(old_file, fill=T, header=F, stringsAsFactors=F))
    if (!is.null(colors_input))
       colors_in <- colors_input$V1

    for (i in 1:length(colors_in)) {

        if (grepl("^[0-9]+ [A-Za-z]+$", colors_in[i])) {
            this_line = unlist(strsplit(colors_in[i], " "))
            if (length(this_line) >= 2) {
                old_site = as.numeric(this_line[1])
                new_site = old_site

                if (any(x$selected_sites$l == old_site & x$selected_sites$r == old_site)) {
                    # map refseq site to concatamer position, via selected_sites
                    new_site = min(sort(unique(which(x$selected_sites$l == old_site &
                                                     x$selected_sites$r == old_site))))
                    colors_in[i] = paste(new_site, paste0(this_line[2:length(this_line)]))
                } else if (any(x$selected_sites$l == old_site | x$selected_sites$r == old_site)) {
                    new_site = min(sort(unique(which(x$selected_sites$l == old_site |
	                                                 x$selected_sites$r == old_site))))
                    colors_in[i] = paste(new_site, paste0(this_line[2:length(this_line)]))
                }
            }
        }
    }
    try(write.table(colors_in, file=new_file, quote=F, sep="", col.names=F, row.names=F))
    return (new_file)
}

#make.timepoint.logos <- function(x, sort_stacks=F,
#    stacks_per_line=NULL, name_prefix="logos", stratify=T, dotify=T,
#    stack_width=18, aspect_ratio=3, show_sample_size=F,
#    logo_format="pdf", colors_file=NULL, color_scheme='charge') { }

#make.logoplot <- function(selected_sites, working_swarm, included,
#    prefix=prefix, dotify=T, hide_xlabels=F, stack_width=18,
#    stacks_per_line=NULL, aspect_ratio=1.5, y_label=NULL,
#    show_sample_size=F, # sequence_multiplicity=NULL,
#    logo_format="pdf", color_option = color_option) { }