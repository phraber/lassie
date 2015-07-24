#' Env Feature Annotation
#' 
#' Used to identify sites in HIV-1 Env associated with neutralizating antibodies.

features_file <- system.file("extdata", "Env_features.xls", package="lassie")
raw.features <- gdata::read.xls(features_file, header=T)
features <- raw.features[11:866, c(9, 14:20, 25:26, 27:34, 37:39)]

# number rows per HXB2 position
rownames(features) <- raw.features[11:866, 7]

# name cols per nAb
colnames(features) <- gsub("^X", "", sapply(1:ncol(features), function(i)
	unlist(strsplit(colnames(features)[i], "[.]"))[[1]]))

env.features <- matrix(F, ncol=ncol(features), nrow=nrow(features))
rownames(env.features) = rownames(features)
colnames(env.features) = colnames(features)
for (i in 1:ncol(env.features))
    env.features[which(grepl("[A-Za-z0-9]", features[, i])), i] = T

rm(features_file, raw.features, features)
