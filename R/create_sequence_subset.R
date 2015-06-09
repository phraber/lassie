#' @keywords internal
create_sequence_subset <- function(seq_ids, working_swarm) {

    seq_subset <- sapply(1:length(seq_ids), function(i) 
	seqinr::c2s(working_swarm$aln_allcolumns[seq_ids[i],]) )

    if (is.numeric(seq_ids)) {
	names(seq_subset) <- rownames(working_swarm$aln_allcolumns)[seq_ids]
    } else {
	names(seq_subset) <- seq_ids
    }

    seq_subset
}
