## ---- fig.show='hold'----------------------------------------------------
    library(lassie)
    alignment_file <- system.file("extdata", "CH505-gp160.fasta", package="lassie")
    eg.swarmtools <- swarmtools(aas_file=alignment_file, tf_loss_cutoff=80)
    summary(eg.swarmtools)
    plot(eg.swarmtools)
    print(eg.swarmtools)

## ------------------------------------------------------------------------
    library(lassie)
    alignment_file <- system.file("extdata", "CH505-gp160.fasta", package="lassie")
    eg.swarmtools <- swarmtools(aas_file=alignment_file, tf_loss_cutoff=80)
    summary(eg.swarmtools)
    print(eg.swarmtools)
    eg.swarmset <- swarmset(eg.swarmtools)
    summary(eg.swarmset)
    print(eg.swarmset)
    plot(eg.swarmset)
