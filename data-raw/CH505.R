alignment_file <- system.file("extdata", "CH505-gp160.fasta", package="lassie")
CH505 <- seqinr::read.fasta(alignment_file, seqtype="AA")
#devtools::use_data(CH505)
