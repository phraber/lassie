#' lassie: Tools to select sequences that represent variation in a 
#' longitudinally sampled protein alignment.
#'
#' The lassie package provides methods to select sites and sequences
#' in a protein sequence alignment obtained from longitudinal sampling,
#' also known as LASSIE, an acronym for "longitudinal antigenic sites and 
#' sequences from intrahost evolution" or "longitudinal antigen swarm 
#' selection from intrahost evolution".
#'
#' @section Assumptions:
#'
#' Start with a sequence alignment sampled serially from an infected host, 
#' which characterizes a pathogen population evolving under immune pressure.
#' The population is related by a single common "transmitted-founder" (TF) 
#' ancestor.
#'
#' @section Step 1. Select sites:
#'
#' The first phase of analysis computes TF loss per aligned site for each 
#' time-point sampled, to select sites of interest where TF loss exceeds a 
#' threshold.  Sites that lose the ancestral TF amino acids are likely under 
#' positive selection and merit further investigation.  
#' @seealso \code{\link{swarmtools}}
#'
#' @section Step 2. Select sequences:
#'
#' The second phase selects a set of sequences with mutations among selected 
#' sites.  A swarm of sequences represents diversity in the population sampled
#' and can subsequently be used to study developing immune responses.  
#' @seealso \code{\link{swarmset}}
#'
#' @section Tutorial:
#'
#' Example data are included to identify sites on the HIV-1 envelope
#' glycoprotein, to characterize affinity maturation which leads to a
#' broadly neutralizing antibody response. To review the tutorial, run
#' \code{vignette(package="lassie")}
#'
#' @docType package
#' @name lassie
NULL
