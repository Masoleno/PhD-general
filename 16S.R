# Script used for visualization and analysis of microbial biodiversity data
# as part of a PhD by Maya Sollen-Norrlin at Canterbury Christ Church University
# Supervised by:
# Dr Naomi Rintoul-Hynes
# Dr Rodrigo Vega
# Dr Alec Forsyth

# Load libraries
library(phyloseq)
library(tidyverse)
library(ggplot2)
library(ggpubr)
# Use the below code to install BiocManager and phyloseq if needed:
# if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
# library(BiocManager)
# BiocManager::install("phyloseq")
# Use this to install microViz if needed:
# install.packages(
  #  "microViz",
 #  repos = c(davidbarnett = "https://david-barnett.r-universe.dev", getOption("repos")))
library(microViz)
library(readr)
library(microbiome)
library(vegan)

# Set-up ----
## Make a function to create phyloseq object from csv files ----
make_pseq <- function(asvfile, taxfile, metafile){
  ASVs <- read_csv(asvfile)
  taxonomy <- read_csv(taxfile, col_types = cols(.default = "c"))
  taxonomy$...1 <- gsub("ASV_", "", taxonomy$...1)
  
  taxonomy <- arrange(taxonomy, as.numeric(...1))
  taxonomy$...1 <- NULL
  
  # Turn into matrices
  ASV_mat <- as.matrix(ASVs)
  taxa_mat <-as.matrix(taxonomy)
  
  
  # Make a phyloseq object with the ASV matrix and the taxonomy matrix
  OTU = otu_table(ASV_mat, taxa_are_rows = TRUE)
  TAX = tax_table(taxa_mat)
  
  physeq <- phyloseq(OTU, TAX)
  
  # Read in metadata file
  smp_data <- read.csv(metafile, na.strings = c(""))
  
  # Remove some  extra text in the Sample.ID column so that the sample id's match those in the ASV table
  smp_data$Sample.ID <- gsub("1-.*A00", "", smp_data$Sample.ID)
  smp_data$Sample.ID <- gsub("1-.*A0", "", smp_data$Sample.ID)
  
  # Arrange in ascending order by sample
  ## First convert to numeric
  smp_data$Sample.ID <- as.numeric(smp_data$Sample.ID)
  smp_data <- arrange(smp_data, Sample.ID)
  
  ## Convert back to character so that they can be matched with the repeats 
  smp_data$Sample.ID <- as.character(smp_data$Sample.ID)
  
  # Filter out only those samples which we have sequencing data for
  ASV_smps <- colnames(ASVs)
  
  smp_data2 <- smp_data %>%
    filter(Sample.ID %in% ASV_smps)
  head(smp_data2)
  
  # Add empty rows for the repeats and controls to match with the phyloseq sample names
  sample_names(physeq)
  rpts <- sample_names(physeq)[159:191]
  rpts
  smp_data3 <- smp_data2 %>%
    add_row(Sample.ID = c(rpts))
  
  # Create phyloseq type sample data out of the metadata
  smp_data4 <- sample_data(data.frame(smp_data3, row.names = sample_names(physeq), stringsAsFactors = FALSE))
  
  # Merge into a complete phyloseq object (no tree yet)
  physeq2 <- merge_phyloseq(physeq, smp_data4)
  
  
}

## Use function ----
ps_bac <- make_pseq("ps_countmat_16S.csv", "ps_taxamat_silva.csv", "combined-tidy-data.csv")

# Check the summary of the phyloseq object
microbiome::summarize_phyloseq(ps_bac)
ps_bac
View(tax_table(ps_bac))

# Make a new phyloseq object without repeats
sample_names(ps_bac)
ps_bac %>%
  sample_data() %>%
  as.data.frame() %>%
  head()
to_remove <- sample_names(ps_bac)[159:173]
pseq_noRPT <- prune_samples(!sample_names(ps_bac) %in% 
                              to_remove, ps_bac)
sample_names(pseq_noRPT)

# Remove taxa that occur in the negative controls from the samples
neg_controls <- sample_names(pseq_noRPT)[159:174]
neg_controls
pseq_controls <- subset_samples(pseq_noRPT, Sample.ID %in%
                   neg_controls)
pseq_controls
pseq_controls <- prune_taxa(taxa_sums(pseq_controls) > 0,
                            pseq_controls)
pseq_controls

badASV <- taxa_names(pseq_controls)
allASV <- taxa_names(pseq_noRPT)
allASV <- allASV[!(allASV %in% badASV)]
pseq_bac <-  prune_taxa(allASV, pseq_noRPT)
pseq_bac
pseq_noRPT

# Remove the controls from the sample data so that it's not interfering with analyses
sample_names(pseq_bac)
to_remove <- sample_names(pseq_bac)[159:176]
pseq_bac <- prune_samples(!sample_names(pseq_bac) %in% 
                              to_remove, pseq_bac)
sample_names(pseq_bac)
View(tax_table(pseq_bac))

# Extract sample_depths (number of reads per sample)
sample_depths <- microbiome::readcount(pseq_bac)
sample_depths

# Histogram of sample depths
hist(sample_depths, main = "Histogram of read depths")

# Extract row number from the otu_table to get the total number of ASVs in the  data
num_asvs_vec <- c(nrow(phyloseq::otu_table(pseq_bac)))
names(num_asvs_vec)[1] <- "abundance"
num_asvs_vec
save(num_asvs_vec, file = "num-asvs-vec.RData")

# Minimum read depth
abundance_metadf <- phyloseq::sample_data(pseq_bac)
abundance_metadf

# Check if the vector of sample_depths has the same order as our metadata rows
head(names(sample_depths))
head(row.names(abundance_metadf))
identical(names(sample_depths),row.names(abundance_metadf))

#Add sample depths to metadata data frame
abundance_metadf[,"depth"] <- sample_depths

#View top 6 rows of edited metadata dataframe
head(abundance_metadf)
class(abundance_metadf)

# add sample depths to original metadata 
smp_data2$depth <- sample_depths

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
asv_abund_df <- as.data.frame(t(phyloseq::otu_table(pseq_bac)))
min(microbiome::readcount(pseq_bac))

# plot the rarefaction curve
vegan::rarecurve(
  x = asv_abund_df, step = 50,
  xlab = "Read depth",
  ylab = "ASVs"
)


# Rarefaction slopes
rarefaction_slopes <- vegan::rareslope(
  x = asv_abund_df, sample = min(microbiome::readcount(pseq_bac)))
  
# View slopes from lowest to highest value
sort(rarefaction_slopes)

# Summary of slopes
summary(rarefaction_slopes)

# Histogram of slopes
hist(rarefaction_slopes)

# Rarefy to minimum depth
pseq_rarefy <- phyloseq::rarefy_even_depth(
  pseq_bac, sample.size = min(microbiome::readcount(pseq_bac)),
  rngseed = 1000
)

# Summarise and check sample counts which should each amount to 8487 (min depth)
microbiome::summarize_phyloseq(pseq_rarefy)
microbiome::readcount(pseq_rarefy)
pseq_bac
pseq_rarefy

#Abundance sums of the 1st six ASVs
head(phyloseq::taxa_sums(pseq_bac))

#View number of ASVs in our data
length(phyloseq::taxa_sums(pseq_bac))

#Remove ASVs with no abundance
pseq_bac <- phyloseq::prune_taxa(
  phyloseq::taxa_sums(pseq_bac) > 0, pseq_bac
)

#Summarise subsetted phyloseq
microbiome::summarize_phyloseq(pseq_bac)
microbiome::readcount(pseq_bac)
length(phyloseq::taxa_sums(pseq_bac))
length(phyloseq::taxa_sums(pseq_rarefy))
# ASV counts
# Add relative abundance ASV count
num_asvs_vec["rarefied"] <- nrow(phyloseq::otu_table(pseq_rarefy))
num_asvs_vec


num_asvs_vec["no. removed taxa"] <- num_asvs_vec[1] - num_asvs_vec[2]
num_asvs_vec

## Phyloseq save ----
save(pseq_rarefy, file ="phyloseq-bacteria-rarefied.RData")
save(pseq_bac, file = "phyloseq-bacteria-noRPTs.RData")
# ASV count save
save(num_asvs_vec, file="num_asvs_vec.v2.RData")


# Data exploration and visualization ----
# Load the phyloseq object saved in set-up step if picking up in a later session
load("phyloseq-bacteria-rarefied.RData")
load("phyloseq-bacteria-noRPTs.RData")
load("num_asvs_vec.v2.RData")
num_asvs_vec

taxa_names(pseq_bac)
taxa_names(pseq_rarefy)
phyloseq::tax_table(pseq_rarefy)[1:5, 1:5]
colnames(tax_table(pseq_rarefy))
phyloseq::tax_table(pseq_rarefy)[1:30, 1:5]
otu_table(pseq_bac)

colnames(tax_table(pseq_bac))
pseq_bac
tail(tax_table(pseq_bac), 20)
head(otu_table(pseq_rarefy), 20)
taxa_names(pseq_bac)

# filter low-occurring taxa (if wanted),
# remove taxa not seen more than 3 times in at least 20% of the samples;
pseq_bac_ft <- filter_taxa(pseq_bac, function(x) sum(x > 3) > (0.2*length(x)), TRUE)
summarize_phyloseq(pseq_bac_ft)
# Diversity analysis ----
## Alpha diversity plots ----
alpha_plot <- phyloseq::plot_richness(physeq = pseq_bac_ft, 
                                      measures = c("Observed","Chao1","Shannon"))
alpha_plot 

phyloseq::plot_richness(physeq = pseq_bac_ft, 
                        x = "Orchard",
                        measures = "Observed") +
  ggplot2::geom_boxplot(aes(fill = sample_data(pseq_bac)$intensity), alpha = 0.5) + 
  theme_bw() + labs(y = "Shannon's Alpha Diversity", fill = "Management Intensity") +
  theme(strip.text.x = element_blank(),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle =110, face = "bold", size = 12), 
        axis.title = element_text(face = "bold", size = 15), axis.title.y = element_text(margin = margin(r = 15))) +
  scale_fill_manual(labels = c("High", "Low"), values = c("#55C667FF", "#FDE725FF"))


phyloseq::plot_richness(physeq = pseq_bac_ft, 
                        x = "Orchard",
                        measures = "Shannon") +
  ggplot2::geom_boxplot()

phyloseq::plot_richness(physeq = pseq_bac_ft, 
                        x = "variety_group",
                        measures = "Shannon") +
  ggplot2::geom_boxplot()

phyloseq::plot_richness(physeq = pseq_bac_ft, 
                        x = "rootstock_group",
                        measures = "Shannon") +
  ggplot2::geom_boxplot()

phyloseq::plot_richness(physeq = pseq_bac_ft, 
                        x = "intensity",
                        measures = "Shannon") +
  ggplot2::geom_boxplot() + theme_bw() + labs(x = "Management Intensity", y = "Shannon's Diversity") +
  theme(strip.text.x = element_blank(),
        strip.background = element_blank()) + scale_x_discrete(labels = c("high" = "High", "low" = "Low"))

phyloseq::plot_richness(physeq = pseq_bac_ft, 
                        x = "fruit_type",
                        measures = "Shannon") +
  ggplot2::geom_boxplot()

phyloseq::plot_richness(physeq = pseq_bac_ft, 
                        x = "orchard_type",
                        measures = "Shannon") +
  ggplot2::geom_boxplot()

subset_samples(pseq_bac_ft, orchard_type == "Charity") %>%
  phyloseq::plot_richness(
    x = "Orchard",
    measures = c("Shannon")) +
  ggplot2::geom_boxplot()

subset_samples(pseq_bac_ft, !orchard_age == "NA") %>%
    phyloseq::plot_richness(
                        x = "orchard_age",
                        measures = c("Shannon")) +
  ggplot2::geom_boxplot()

subset_samples(pseq_bac_ft, !compost == "NA") %>%
  phyloseq::plot_richness(
    x = "compost",
    measures = c("Shannon")) +
  ggplot2::geom_boxplot()


## Alpha stats ----
# Produce data frame of all alpha diversity values
alpha_df <- phyloseq::estimate_richness(physeq = pseq_bac_ft)
head(alpha_df)
row.names(alpha_df) <- smp_data2$Sample.ID
row.names(alpha_df)

# Add Shannon and Observed to metadata data frame and save
smp_data2$Shannon <- alpha_df$Shannon
smp_data2$Observed <- alpha_df$Observed
write.csv(smp_data2, "tidy-data-16S-diversity.csv")
# Save the richness df as csv file
write.csv(alpha_df, "16S-diversity-metrics.csv")

# Paired wilcoxon 
pairwise.wilcox.test(alpha_df$Shannon, phyloseq::sample_data(pseq_rarefy)$Orchard)
pairwise.wilcox.test(alpha_df$Shannon, phyloseq::sample_data(pseq_rarefy)$intensity)
pairwise.wilcox.test(alpha_df$Shannon, phyloseq::sample_data(pseq_rarefy)$fruit_type)
pairwise.wilcox.test(alpha_df$Shannon, phyloseq::sample_data(pseq_rarefy)$orchard_type)

# Rel. abundance plots ----
# See taxa at differennt ranks
get_taxa_unique(pseq_bac, taxonomic.rank = rank_names(pseq_bac)[3])

### Phylum ----
phylum_pseq <- pseq_bac %>%
  aggregate_taxa(level = "phylum") %>%
  transform("compositional")

taxa_names(phylum_pseq)
tail(taxa_names(pseq_bac))
head(taxa_names(pseq_bac))
head(phyloseq::otu_table(phylum_pseq))
head(phyloseq::tax_table(phylum_pseq))

phylum_pseq <- tax_glom(phylum_pseq, "phylum", NArm = TRUE, bad_empty = "Unknown")
head(phyloseq::tax_table(phylum_pseq))

view(phyloseq::tax_table(phylum_pseq))
paste0("Number of phyla: ", nrow(phyloseq::otu_table(phylum_pseq)))

microbiome::summarize_phyloseq(phylum_pseq)
microbiome::readcount(phylum_pseq)


head(phyloseq::tax_table(phylum_pseq))



bar.rel.abund <- plot_composition(phylum_pseq,
                                  average_by = "Orchard") +
  labs(x = "Site", y = "Relative Abundance", fill = "Phyla") 

bar.rel.abund


bar.rel.abund2 <- plot_composition(phylum_pseq,
                                   average_by = "intensity") +
  labs(x = "Management Intensity", y = "Relative Abundance", fill = "Phyla")

bar.rel.abund2


bar.rel.abund3 <- plot_composition(phylum_pseq,
                                   average_by = "orchard_type") +
  labs(x = "Orchard Type", y = "Relative Abundance", fill = "Phyla")

bar.rel.abund3

bar.rel.abund4 <- plot_composition(phylum_pseq,
                                   average_by = "fruit_type") +
  labs(x = "Fruit Type", y = "Relative Abundance", fill = "Phyla")

bar.rel.abund4


### Family ----
#Transform abundance table to a relative abundance (compositional) table
pseq_relabund <- microbiome::transform(pseq_bac, "compositional")

#Summarise and check sample counts which should each amount to 1
microbiome::summarize_phyloseq(pseq_relabund)
microbiome::readcount(pseq_relabund)

#Family phyloseq
family_pseq <- microbiome::aggregate_taxa(pseq_relabund, "family", verbose = FALSE)
#Head of family relative abundance table
head(phyloseq::otu_table(family_pseq))
tail(phyloseq::otu_table(family_pseq))
#Number of families
paste0("Number of families: ", nrow(phyloseq::otu_table(family_pseq)))
#Summarise
microbiome::summarize_phyloseq(family_pseq)
microbiome::readcount(family_pseq)

pseq_rarefy <- subset_taxa(pseq_rarefy, family !="Unknown")

fam <- tax_glom(pseq_bac_ft, taxrank = "family")
fam <- subset_taxa(fam, family != "Unknown")
plot_heatmap(fam, sample.label = "Orchard")

#Family heatmap
family_heatmap <- microbiome::plot_composition(family_pseq, plot.type = "heatmap") +
  xlab("Family") + ylab("Sample") +
  ggtitle("Family relative abundance heatmap") +
  #Flip the x and y axes
  coord_flip()

family_heatmap

### Genus ----

genus_pseq <- pseq_bac %>%
  aggregate_taxa(level = "genus") %>%
  transform("compositional")

head(phyloseq::otu_table(genus_pseq))
head(phyloseq::tax_table(genus_pseq))


genus_pseq <- tax_glom(genus_pseq, "genus", NArm = TRUE, bad_empty = "Unknown")
head(phyloseq::tax_table(genus_pseq))


view(phyloseq::tax_table(genus_pseq))
paste0("Number of genera: ", nrow(phyloseq::otu_table(genus_pseq)))

plot_composition(genus_pseq, average_by = "Orchard") +
  labs(x = "Site", y = "Relative Abundance", fill = "Genus") 


plot_composition(genus_pseq, average_by = "intensity") +
  labs(x = "Management Intensity", y = "Relative Abundance", fill = "Genus")

### Species ----
get_taxa_unique(pseq_bac, taxonomic.rank = rank_names(pseq_bac)[7])
species_pseq <- pseq_bac %>%
  aggregate_taxa(level = "species") %>%
  transform("compositional")

head(phyloseq::otu_table(species_pseq))
head(phyloseq::tax_table(species_pseq))

taxa_names(species_pseq) <- str_remove(
  taxa_names(species_pseq) , "uncultured_")

taxa_names(species_pseq)

phyloseq::otu_table(species_pseq)

#### Function for filtering out dubious taxa ----
pop_taxa = function(physeq, badTaxa){
  allTaxa = taxa_names(physeq)
  allTaxa <- allTaxa[!(allTaxa %in% badTaxa)]
  return(prune_taxa(allTaxa, physeq))
}
# Define unwanted taxa
badTaxa = c("Unknown","unidentified", "bacterium", "archaeon", "soil_archaeon", "prokaryote",
            "organism", "eukaryote", "compost_bacterium", "soil_bacterium")

# Apply function on species phyloseq object
sp2 = pop_taxa(species_pseq, badTaxa)
# Check that taxa have been removed
taxa_names(sp2)

# Agglomerate 
species_pseq2 <- tax_glom(sp2, "species")
taxa_names(species_pseq2)
paste0("Number of species: ", nrow(phyloseq::otu_table(species_pseq2)))

#### Plots ----
plot_composition(species_pseq2, average_by = "intensity") +
  labs(x = "Management Intensity", y = "Relative Abundance", fill = "Species")

plot_composition(species_pseq, average_by = "fruit_type") +
  labs(x = "Fruit Type", y = "Relative Abundance", fill = "Species")


# Ordinations ----
dist.mat <- phyloseq::distance(pseq_bac, "bray")
ord.nmds.bray <- phyloseq::ordinate(pseq_rarefy, method = "NMDS", distance = "bray")
ord.mds.bray <- phyloseq::ordinate(pseq_rarefy, method = "MDS", distance = "bray")
ord.cca <- ordinate(pseq_rarefy, "CCA")
ord.pcoa <- ordinate(pseq_bac, method = "PCoA", distance = "bray")

#Plot ordination
pcoa_plot = plot_ordination(pseq_bac, ord.pcoa, color="intensity",
                            shape = "orchard_type") +
  geom_point(size = 3)  
pcoa_plot

pcoa_plot2 = plot_ordination(pseq_rarefy, ord.pcoa, 
                             color="intensity") +
  geom_point(size = 3) + ggplot2::stat_ellipse()
pcoa_plot2

pcoa_plot3 = plot_ordination(pseq_rarefy, ord.pcoa, 
                             color="orchard_type") +
  geom_point(size = 3) + ggplot2::stat_ellipse()
pcoa_plot3

pcoa_plot4 = plot_ordination(pseq_rarefy, ord.pcoa, 
                             color="fruit_type") +
  geom_point(size = 3) + ggplot2::stat_ellipse()
pcoa_plot4

plot_ordination(pseq_rarefy, ord.pcoa, 
                color="variety_group") +
  geom_point(size = 3) + ggplot2::stat_ellipse()

plot_ordination(pseq_rarefy, ord.pcoa, 
                color="Orchard") +
  geom_point(size = 3) + ggplot2::stat_ellipse()


pseq_rarefy_no_charity <- subset_samples(pseq_rarefy, !orchard_type == "Charity")
str(sample_data(pseq_rarefy_no_charity))
pseq_rarefy_no_charity
ord.pcoa.commercial <- ordinate(pseq_rarefy_no_charity, 
                                method = "PCoA", distance = "bray")

pcoa_plot5 = plot_ordination(pseq_rarefy_no_charity, ord.pcoa.commercial, 
                             color="fruit_type") +
  geom_point(size = 3) + ggplot2::stat_ellipse()
pcoa_plot5

pseq_rarefy_no_cider <- subset_samples(pseq_rarefy, 
                                       !fruit_type == "Cider")

pseq_rarefy_no_dessert <- subset_samples(pseq_rarefy, 
                                       !fruit_type == "Dessert")

pseq_rarefy_no_dessert
view(sample_data(pseq_rarefy_no_dessert))

ord.pcoa.cider <- ordinate(pseq_rarefy_no_dessert, 
                                method = "PCoA", distance = "bray")

plot_ordination(pseq_rarefy_no_dessert, ord.pcoa.cider, 
                color="Orchard") +
  geom_point(size = 3) + ggplot2::stat_ellipse()

pcoa_plot6 = plot_ordination(pseq_rarefy_no_cider, ord.pcoa.dessert, 
                             color="intensity") +
  geom_point(size = 3) + ggplot2::stat_ellipse()
pcoa_plot6

plot_ordination(pseq_rarefy_no_cider, ord.pcoa.dessert, 
                color="Orchard") +
  geom_point(size = 3) + ggplot2::stat_ellipse()


pseq_rarefy_filtered <- pseq_rarefy %>%
  subset_samples(compost != "NA" & pesticides !="NA")

ord.nmds2  <- ordinate(pseq_rarefy_filtered, method = "NMDS", distance = "bray")

phyloseq::plot_ordination(pseq_rarefy_filtered, ord.nmds2, color = "compost", shape = "pesticides")











