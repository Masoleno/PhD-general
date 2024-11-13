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
library(BiocManager)
# Use the below code to install BiocManager and phyloseq if needed:
# if (!require("BiocManager", quietly = TRUE))
 # install.packages("BiocManager")
# BiocManager::install("phyloseq")
# Use this to install microViz if needed:
# install.packages(
 # "microViz",
#  repos = c(davidbarnett = "https://david-barnett.r-universe.dev", getOption("repos")))
library(microViz)
library(phyloseq)
library(readr)
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
str(smp_data)
# Arrange in ascending order by sample
## First convert to numeric
smp_data$Sample.ID <- as.numeric(smp_data$Sample.ID)
str(smp_data)
smp_data <- arrange(smp_data, Sample.ID)
head(smp_data)

## Convert back to character so that they can be matched with the repeats 
smp_data$Sample.ID <- as.character(smp_data$Sample.ID)
str(smp_data)

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

# Merge into a complete phyloseq object (no tree yet)
ps_bac <- merge_phyloseq(physeq, smp_data4)

# Check the summary of the phyloseq object
microbiome::summarize_phyloseq(ps_bac)


# Make a new phyloseq object without repeats and controls
ps_bac %>%
  sample_data() %>%
  as.data.frame() %>%
  head()
to_remove <- sample_names(ps_bac)[159:191]
pseq_noRPT <- prune_samples(!sample_names(ps_bac) %in% to_remove, ps_bac)
sample_names(pseq_noRPT)

# Extract sample_depths (number of reads per sample)
sample_depths <- microbiome::readcount(pseq_noRPT)
sample_depths

# Histogram of sample depths
hist(sample_depths, main = "Histogram of read depths")

# Extract row number from the otu_table to get the total number of ASVs in the  data
num_asvs_vec <- c(nrow(phyloseq::otu_table(pseq_noRPT)))
names(num_asvs_vec)[1] <- "abundance"
num_asvs_vec
save(num_asvs_vec, file = "num-asvs-vec.RData")

# Minimum read depth
abundance_metadf <- phyloseq::sample_data(pseq_noRPT)
abundance_metadf

# Check if the vector of sample_depths has the same order as our metadata rows
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

boxplot3  <- ggplot2::ggplot(abundance_metadf, aes(y=depth, x=rootstock_group)) +
  ggplot2::geom_boxplot()
boxplot3

boxplot4  <- ggplot2::ggplot(abundance_metadf, aes(y=depth, x=variety_group)) +
  ggplot2::geom_boxplot() +
  theme(axis.text.x = element_text(angle =110))
boxplot4


# Rarefaction curve
# Extract ASV table as transposed data frame
asv_abund_df <- as.data.frame(t(phyloseq::otu_table(pseq_noRPT)))

# plot the rarefaction curve
vegan::rarecurve(
  x = asv_abund_df, step = 50,
  xlab = "Read depth",
  ylab = "ASVs"
)

# Subset and keep samples with more than 6k reads
ps_min6K <- phyloseq::subset_samples(pseq_noRPT, sample_depths > 6000)

#Abundance sums of the 1st six ASVs
head(phyloseq::taxa_sums(ps_min6K))
length(phyloseq::taxa_sums(ps_min6K))

# Remove ASVs with no abundance
ps_min6K <- phyloseq::prune_taxa(
  phyloseq::taxa_sums(ps_min6K) > 0, ps_min6K
)


# Summarise subsetted phyloseq
microbiome::summarize_phyloseq(ps_min6K)
microbiome::readcount(ps_min6K)
length(phyloseq::taxa_sums(ps_min6K))
ps_min6K


# Plot rarefaction curves displaying two different minimum read depths (6k and min read depth of the whole physeq) as a horizontal line
vegan::rarecurve(
  x = asv_abund_df, step = 50,
  xlab = "Read depth", ylab = "ASVs", label = F,
  sample = min(microbiome::readcount(ps_min6K))
)

vegan::rarecurve(
  x = asv_abund_df, step = 50,
  xlab = "Read depth", ylab = "ASVs", label = F,
  sample = min(microbiome::readcount(pseq_noRPT))
)


# 6k seems like a good minimum read depth to rarefy at
# Rarefaction slopes
rarefaction_slopes <- vegan::rareslope(
  x = asv_abund_df, sample = min(microbiome::readcount(ps_min6K)))
  
# View slopes from lowest to highest value
sort(rarefaction_slopes)

# Summary of slopes
summary(rarefaction_slopes)

# Histogram of slopes
hist(rarefaction_slopes)

# Rarefy to minimum depth
pseq_rarefy <- phyloseq::rarefy_even_depth(
  ps_min6K, sample.size = min(microbiome::readcount(ps_min6K)),
  rngseed = 1000
)

# Summarise and check sample counts which should each amount to 9332 (min depth)
microbiome::summarize_phyloseq(pseq_rarefy)
microbiome::readcount(pseq_rarefy)
# ASV counts
# Add relative abundance ASV count
num_asvs_vec["rarefied"] <- nrow(phyloseq::otu_table(pseq_rarefy))
num_asvs_vec

# Phyloseq save
save(pseq_rarefy, file ="phyloseq-bacteria-rarefied.RData")
save(pseq_noRPT, file = "phyloseq-bacteria-noRPTs.RData")
# ASV count save
save(num_asvs_vec, file="num_asvs_vec.v2.RData")

# Load the phyloseq project saved in set-up step if picking up in a later session
load("phyloseq-bacteria-rarefied.RData")
tail(sample_data(pseq_rarefy), 15)

# Data exploration and visualization ----
# Load the phyloseq object saved in set-up step if picking up in a later session
load("phyloseq-bacteria-rarefied.RData")
load("C:/Users/UKGC/OneDrive - Canterbury Christ Church University/PhD/R-general/num_asvs_vec.v2.RData")
num_asvs_vec

# Taxa relative abundance ----
# Transform abundance table to a relative abundance (compositional) table
pseq_relabund <- microbiome::transform(pseq_rarefy, "compositional")

#Summarise and check sample counts which should each amount to 1
microbiome::summarize_phyloseq(pseq_relabund)
microbiome::readcount(pseq_relabund)


# Diversity analysis ----

# Alpha diversity plots ----
alpha_plot <- phyloseq::plot_richness(physeq = pseq_rarefy, 
                                      measures = c("Observed","Chao1","Shannon"))
alpha_plot + theme(axis.text.x = element_text(angle =110))

phyloseq::plot_richness(physeq = pseq_rarefy, 
                        x = "Orchard",
                        measures = "Shannon") +
  ggplot2::geom_boxplot(aes(fill = sample_data(pseq_rarefy)$intensity), alpha = 0.5) + 
  theme_bw() + labs(y = "Shannon's Alpha Diversity", fill = "Management Intensity") +
  theme(strip.text.x = element_blank(),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle =110, face = "bold", size = 12), 
        axis.title = element_text(face = "bold", size = 15), axis.title.y = element_text(margin = margin(r = 15))) +
  scale_fill_manual(labels = c("High", "Low"), values = c("#55C667FF", "#FDE725FF"))


phyloseq::plot_richness(physeq = pseq_rarefy, 
                        x = "Orchard",
                        measures = "Observed") +
  ggplot2::geom_boxplot()

phyloseq::plot_richness(physeq = pseq_rarefy, 
                        x = "variety_group",
                        measures = c("Observed","Shannon")) +
  ggplot2::geom_boxplot()

phyloseq::plot_richness(physeq = pseq_rarefy, 
                        x = "rootstock_group",
                        measures = "Shannon") +
  ggplot2::geom_boxplot()

phyloseq::plot_richness(physeq = pseq_rarefy, 
                        x = "intensity",
                        measures = c("Shannon", "Simpson")) +
  ggplot2::geom_boxplot() + theme_bw() + labs(x = "Management Intensity", y = "Shannon's Alpha Diversity") +
  theme(strip.text.x = element_blank(),
        strip.background = element_blank()) + scale_x_discrete(labels = c("high" = "High", "low" = "Low"))

phyloseq::plot_richness(physeq = pseq_rarefy, 
                        x = "fruit_type",
                        measures = c("Simpson","Shannon")) +
  ggplot2::geom_boxplot()

phyloseq::plot_richness(physeq = pseq_rarefy, 
                        x = "orchard_type",
                        measures = c("Observed","Simpson","Shannon")) +
  ggplot2::geom_boxplot()


subset_samples(pseq_rarefy, !orchard_age == "NA") %>%
    phyloseq::plot_richness(
                        x = "orchard_age",
                        measures = c("Shannon")) +
  ggplot2::geom_boxplot()

subset_samples(pseq_rarefy, !compost == "NA") %>%
  phyloseq::plot_richness(
    x = "compost",
    measures = c("Shannon")) +
  ggplot2::geom_boxplot()


# Produce data frame of all alpha diversity values
alpha_df <- phyloseq::estimate_richness(physeq = pseq_rarefy)
head(alpha_df)

#Paired wilcoxon test
#Observed
pairwise.wilcox.test(alpha_df$Observed, phyloseq::sample_data(pseq_rarefy)$Orchard)

head(sample_data(pseq_rarefy))
pairwise.wilcox.test(alpha_df$Shannon, phyloseq::sample_data(pseq_rarefy)$orchard_age)

sample_data(pseq_rarefy)$pH <- as.numeric(sample_data(pseq_rarefy)$pH)
sample_data(pseq_rarefy)$Conductivity <- as.numeric(sample_data(pseq_rarefy)$Conductivity)
sample_data(pseq_rarefy)$NO3 <- as.numeric(sample_data(pseq_rarefy)$NO3)
sample_data(pseq_rarefy)$NO2 <- as.numeric(sample_data(pseq_rarefy)$NO2)
sample_data(pseq_rarefy)$NH4 <- as.numeric(sample_data(pseq_rarefy)$NH4)
sample_data(pseq_rarefy)$TON <- as.numeric(sample_data(pseq_rarefy)$TON)
sample_data(pseq_rarefy)$OM <- as.numeric(sample_data(pseq_rarefy)$OM)

str(sample_data(pseq_rarefy))

# Ordinations ----
dist.mat <- phyloseq::distance(pseq_rarefy, "bray")
ord.nmds.bray <- phyloseq::ordinate(pseq_rarefy, method = "NMDS", distance = "bray")
ord.mds.bray <- phyloseq::ordinate(pseq_rarefy, method = "MDS", distance = "bray")
ord.cca <- ordinate(pseq_rarefy, "CCA")

#Plot ordination
mds.bray <- phyloseq::plot_ordination(pseq_rarefy, ord.mds.bray,
                                            color = "compost")
mds.bray

pseq_rarefy_filtered <- pseq_rarefy %>%
  subset_samples(compost != "NA" & pesticides !="NA")

ord.mds2  <- ordinate(pseq_rarefy_filtered, method = "MDS", distance = "bray")

phyloseq::plot_ordination(pseq_rarefy_filtered, ord.mds2, color = "compost", shape = "pesticides")


p.cca <- plot_ordination(pseq_rarefy, ord.cca,
                         type = "samples", color = "fruit_type")

p.cca + geom_point()


biplot_pcoa(pseq_rarefy, color = "Orchard", shape = "instensity")


is.na(sample_data(pseq_rarefy))
str(sample_data(pseq_rarefy))




pseq_rarefy %>%
  tax_transform("clr") %>%
  ord_calc(
    constraints = c("pH", "OM"),
    scale_cc = FALSE
  ) %>%
  ord_plot(color = "intensity", shape = "fruit_type", size = 2) +
  scale_colour_brewer(palette = "Dark2")













