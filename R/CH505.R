#' Within-host longitudinal Env single-genome amplification alignment from CH505.
#'
#' An alignment of 384 + 2 full-length HIV-1 envelope glycoprotein (env) gp160 
#' amino-acid sequences sampled from 4 (w004) through 160 (w160) weeks 
#' post-infection.  
#'
#' For the record, 397 sequences from CH505 are available in this sample 
#' interval. Thirteen sequences have been excluded from this alignment, 
#' two with unique long insertions relative to the TF (w009.33.KM284795 and 
#' w136.B1.KC247403) and eleven with a shared 321-nt deletion, which most 
#' likely results in an inviable Env.  The two sequences with long insertions
#' explain why so many gapped columns in the alignment are unoccupied, i.e. 
#' the gaps are place-holders for the long insertions.  It is unclear whether 
#' the shared deletions result from sequencing or replication error.

#' @format
#' \describe{
#'   \item{sequence name}{Up to three dot-separated fields, indicating time 
#' post-infection, in weeks, the amplicon identifier, and the database 
#' accession number.  Two additional sequences are the HXB2 reference for 
#' numbering positions and the inferred transmitted-founder virus, which 
#' matches multiple Envs.}
#'   \item{sequence text}{Asparagines potential N-linked glycosylation sites 
#' are indicated as "O".  Gap characters ("-") are inserted to maintain aligned
#' sites.  "Z" indicates a stop codon.  "X" indicates an incomplete or 
#' ambiguous codon.}
#' }
#' @source \url{http://hiv.lanl.gov/components/sequence/HIV/search/d_search.com?spl_pub_id=16774}
#' @source \url{http://hiv.lanl.gov/components/sequence/HIV/search/d_search.com?spl_pub_id=18294}
#' @source \url{http://ncbi.nlm.nih.gov/pubmed/23552890}
#' @source \url{http://ncbi.nlm.nih.gov/pubmed/25065977}
"CH505"
