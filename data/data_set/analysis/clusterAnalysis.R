# Check the "ideal" clustering of this feature set.

library(cluster)

# Hack: make sure this is run after classifyDonors.R
if (!exists("donorFeaturesMat")) {
  stop("Run this after classifyDonors.R. HACKY CODE!")
}

pam1 <- function(x,k) list(cluster = pam(x,k, cluster.only=TRUE))
clusterResults <- clusGap(donorFeaturesMat, pam1, K.max = 30, B = 100)
