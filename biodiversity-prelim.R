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



ps <- prune_samples(sample_names(physeq) != c("123R":"Pos2"))