# Script used for visualization and analysis of microbial biodiversity data
# as part of a PhD by Maya Sollen-Norrlin at Canterbury Christ Church University
# Supervised by:
# Dr Naomi Rintoul-Hynes
# Dr Rodrigo Vega
# Dr Alec Forsyth

# Load libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(phyloseq)
library(readr)
library(BiocManager)
library(microbiome)
library(vegan)

# Set-up ----
# Read in the ASV table with number of reads per sample (produced with dada2)
ASVs <- read_csv("ps_countmat_2.csv")

# Read in the taxonomy table showing the taxonomy of each ASV
# column types need to be specified as otherwise it throws parsing errors
taxonomy <- read_csv("ps_taxamat.csv", col_types = list(
  ...1 = "c",
  kingdom = "c",
  phylum = "c",
  class ="c",
  order = "c",
  family = "c",
  genus = "c",
  species = "c"))

# check the data (the view() function doesn't show all columns)
head(ASVs)
head(taxonomy)

# Turn into matrices
ASV_mat <- as.matrix(ASVs)
taxa_mat <-as.matrix(taxonomy)

# Make a phyloseq object with the ASV matrix and the taxonomy matrix
physeq <- phyloseq(otu_table(ASV_mat, taxa_are_rows = TRUE), tax_table(taxa_mat))

# Read in metadata data (produced at the end of the data-prep.R script)
smp_data <- read.csv("combined-tidy-data.csv", na.strings = c(""))

# Remove some  extra text in the Sample.ID column so that the sample id's match those in the ASV table
smp_data$Sample.ID <- gsub("1-.*A00", "", smp_data$Sample.ID)
smp_data$Sample.ID <- gsub("1-.*A0", "", smp_data$Sample.ID)

# Arrange in ascending order by sample 
smp_data <- arrange(smp_data, Sample.ID)

# Filter out only those samples which we have sequencing data for
ASV_smps <- colnames(ASVs)
ASV_smps
smp_data2 <- smp_data %>%
  filter(Sample.ID %in% ASV_smps)

# Add empty rows for the repeats and controls to match with the phyloseq sample names
sample_names(physeq)
rpts <- sample_names(physeq)[159:191]
rpts
smp_data3 <- smp_data2 %>%
  add_row(Sample.ID = c(rpts))

# Create phyloseq type sample data out of the metadata
smp_data4 <- sample_data(data.frame(smp_data3, row.names = sample_names(physeq), stringsAsFactors = FALSE))

# Merge into a complete phyloseq oject (no tree yet)
ps_bac <- merge_phyloseq(physeq, smp_data4)


# Check the summary and save the phyloseq object
microbiome::summarize_phyloseq(ps_bac)
save(ps_bac, file = "phyloseq-bacteria.RData")

# Data exploration and visualisation ----
# Extract sample_depths (number of reads per sample)
sample_depths <- microbiome::readcount(ps_bac)
sample_depths
# Histogram of sample depths
hist(sample_depths, main = "Histogram of read depths")

# Extract row number from the otu_table to get the total number of ASVs in the  data
num_asvs_vec <- c(nrow(phyloseq::otu_table(ps_bac)))
names(num_asvs_vec)[1] <- "abundance"
num_asvs_vec
save(num_asvs_vec, file = "num-asvs-vec.RData")


# Minimum read depth
abundance_metadf <- phyloseq::sample_data(ps_bac)

#Check if the vector of sample_depths has the same order as our metadata rows
head(names(sample_depths))
head(row.names(abundance_metadf))
identical(names(sample_depths),row.names(abundance_metadf))

#Add sample depths to metadata data frame
abundance_metadf[,"depth"] <- sample_depths
#View top 6 rows of edited metadata dataframe
head(abundance_metadf)

# Boxplots of read depth by groupings
boxplot <- boxplot <- ggplot2::ggplot(abundance_metadf, aes(y=depth, x=Orchard)) +
  ggplot2::geom_boxplot()
boxplot

boxplot2  <- ggplot2::ggplot(abundance_metadf, aes(y=depth, x=intensity)) +
  ggplot2::geom_boxplot()
boxplot2

boxplot3  <- ggplot2::ggplot(abundance_metadf, aes(y=depth, x=Rootstock)) +
  ggplot2::geom_boxplot()
boxplot3

boxplot4  <- ggplot2::ggplot(abundance_metadf, aes(y=depth, x=Variety)) +
  ggplot2::geom_boxplot() +
  theme(axis.text.x = element_text(angle =110))
boxplot4


# Rarefaction curve
# Extract ASV table as transposed data frame
asv_abund_df <- as.data.frame(t(phyloseq::otu_table(ps_bac)))

# plot the rarefaction curve
vegan::rarecurve(
  x = asv_abund_df, step = 50,
  xlab = "Read depth",
  ylab = "ASVs"
)


# Subset and keep samples with more than 11k reads
ps_min11K <- phyloseq::subset_samples(ps_bac, sample_depths > 11000)

#Abundance sums of the 1st six ASVs
head(phyloseq::taxa_sums(ps_min11K))
length(phyloseq::taxa_sums(ps_min11K))

# Remove ASVs with no abundance
ps_min11K <- phyloseq::prune_taxa(
  phyloseq::taxa_sums(ps_min11K) > 0, ps_min11K
)

# Summarise subsetted phyloseq
microbiome::summarize_phyloseq(ps_min11K)
microbiome::readcount(ps_min11K)
length(phyloseq::taxa_sums(ps_min11K))
ps_min11K





# Taxa relative abundance ----
# Transform abundance table to a relative abundance (compositional) table
pseq_relabund <- microbiome::transform(ps_bac, "compositional")

#Summarise and check sample counts which should each amount to 1
microbiome::summarize_phyloseq(pseq_relabund)
microbiome::readcount(pseq_relabund)






# Diversity analysis ----
# Plot rarefaction curves displaying two different minimum read depths (11k and 6k) as a horizontal line
vegan::rarecurve(
  x = asv_abund_df, step = 50,
  xlab = "Read depth", ylab = "ASVs", lwd=1, label = F,
  sample = min(microbiome::readcount(ps_min11K))
)

vegan::rarecurve(
  x = asv_abund_df, step = 50,
  xlab = "Read depth", ylab = "ASVs", lwd=1, label = F,
  sample = min(6000)
)

# 11k seems like a good minimum read depth to rarefy at
# Rarefaction slopes
rarefaction_slopes <- vegan::rareslope(
  x = asv_abund_df, sample = min(microbiome::readcount(ps_min11K))
)
# View slopes from lowest to highest value
sort(rarefaction_slopes)

# Summary of slopes
summary(rarefaction_slopes)

# Histogram of slopes
hist(rarefaction_slopes)


# Rarefy to minimum depth
pseq_rarefy <- phyloseq::rarefy_even_depth(
  ps_min11K, sample.size = min(microbiome::readcount(ps_min11K)),
  rngseed = 1000
)


# Summarise and check sample counts which should each amount to 12832 (min depth)
microbiome::summarize_phyloseq(pseq_rarefy)
microbiome::readcount(pseq_rarefy)
# ASV counts
# Add relative abundance ASV count
num_asvs_vec["rarefied"] <- nrow(phyloseq::otu_table(pseq_rarefy))
num_asvs_vec

# Phyloseq save
save(pseq_rarefy, file ="phyloseq-bacteria-rarefied.RData")
# ASV count save
save(num_asvs_vec, file="num_asvs_vec.v2.RData")

















