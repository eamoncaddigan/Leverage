# Classify the donors as transactional vs. ideological

library(dplyr)
library(tidyr)
library(RPostgreSQL)

donorFeatures <- readRDS("donorFeatures.rds")

# Classify the donors -----------------------------------------------------

donorFeaturesMat <- as.matrix(donorFeatures[, 2:ncol(donorFeatures)])

# NAs are non-donations, so 0 is cool.
donorFeaturesMat[is.na(donorFeaturesMat)] <- 0

# Center the columns
donorFeaturesMat <- apply(donorFeaturesMat, 2, scale)

# Divide the data into 2 clusters
set.seed(17761856)
clusterResults <- kmeans(donorFeaturesMat, centers = 2)

# Find the cluster with all the high rollers
transactionalCluster <- clusterResults$cluster == which.min(table(clusterResults$cluster))

# Find out how much money each candidate got from transactional vs. ideological
# donors.
donorFeaturesClassified <- donorFeatures %>%
  mutate(DonorType = "ideological")
donorFeaturesClassified$DonorType[transactionalCluster] <- "transactional"

candidateSources <- donorFeaturesClassified %>% 
  gather("Candidate", "Amount", -Donor, -DonorType) %>%
  group_by(Candidate, DonorType) %>%
  summarize(Amount = sum(Amount, na.rm = TRUE))


# Put the amounts into the DB ---------------------------------------------

load("dblogin.RData")     # Brings in dbuser/dbpassword
con <- dbConnect(dbDriver("PostgreSQL"), dbname = "demhack2016",
                 host = "campaign-finance.phl.io", port = 5432,
                 user = dbuser, password = dbpassword)

if (!dbExistsTable(con, "candidate_sources")) {
  cat("Writing table 'candidate_sources'.")
  dbWriteTable(con, name = "candidate_sources", as.data.frame(candidateSources), row.names = FALSE)
} else {
  cat("Table 'candidate_sources' exists.")
}
