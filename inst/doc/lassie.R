## ---- fig.show='hold'----------------------------------------------------
    library(lassie)
    alignment_file <- system.file("extdata", "CH505-gp160.fasta", package="lassie")
    a <- swarmtools(aas_file=alignment_file, tf_loss_cutoff=0:5*20)
    summary(a)
    plot(a)

## ----echo=FALSE----------------------------------------------------------
    library(lassie)
    alignment_file <- system.file("extdata", "CH505-gp160.fasta", package="lassie")
    A <- swarmtools(aas_file=alignment_file, tf_loss_cutoff=80)
    print(A)

## ---- fig.show='hold'----------------------------------------------------
    library(lassie)
    alignment_file <- system.file("extdata", "CH505-gp160.fasta", package="lassie")
    a <- swarmtools(aas_file=alignment_file, tf_loss_cutoff=0:5*20)
    summary(a)
    A <- swarmtools(aas_file=alignment_file, tf_loss_cutoff=80)
    print(A)
    B <- swarmset(A)
    summary(B)
    print(B)
    b.plot <- plot(B)

