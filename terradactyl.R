#install.packages("devtools")
# Install 'Rtools'
#devtools::install_github('Landscape-Data-Commons/terradactyl')
#install.packages("tidyverse")

setwd("C:/Users/kayla/OneDrive/Documents/PhDDataAnalysis/SageSTEP_terradactyl_v2")
library("terradactyl")
library("tidyverse")

# Transform LPI from wide to long format.
# **Only needed the first time running this script or if original lpi csv is altered.**

# Read in wide LPI csv file
lpi_wide <- read.csv("orig_inputs/LPI.csv")
# Transform to tall LPI
lpi_tall <- pivot_longer(lpi_wide, cols = 5:10, names_to = "layer",
                          values_to = "code", values_drop_na = TRUE)

# Add and populate LineKey & PointLoc columns
lpi_tall$LineKey = lpi_tall$PrimaryKey
lpi_tall$PointLoc = lpi_tall$PointNbr

# Write tall LPI csv file
write.csv(lpi_tall, file = 'int_inputs/LPI_Tall.csv', row.names = FALSE)


### Cover Indicators ###
#Read in tall LPI csv file
lpi_tall <- read.csv("int_inputs/LPI_Tall.csv")

#Calculate species percent cover
cover_species <- pct_cover(lpi_tall = lpi_tall,
          tall = F,
          hit = "any",
          by_line=FALSE,
          code
          )

# write species cover csv file
write.csv(cover_species, file = 'final_outputs/cover_by_species.csv', row.names = F)

# To pull out cover by single species code
cover_kopr80 <-
  pct_cover(lpi_tall = lpi_tall,
            tall = T,
            hit = "any",
            by_line = F,
            code) %>%
  # subset to only cover of exposed soil
  dplyr::filter(indicator == "KOPR80")

# To pull out cover by group of species combined
# define a grouping variable for IAG vs weed vs seeded vs other plant vs none percent cover
lpi_tall$Type <- "Other"
lpi_tall[lpi_tall$code %in% c("BRTE", "VUBR"),
         "Type"] <- "IAG"
lpi_tall[lpi_tall$code %in% c("SATR12", "SIAL2", "BASC5", "CENTA","CETE5", "CHJU", 
                              "DESO2", "LASE", "LEPE2", "POBU", "TAOF", "TRDU"),
         "Type"] <- "Weed"
lpi_tall[lpi_tall$code %in% c("ACMI2", "AGROP", "KOPR80", "POSE", "LILE3"),
         "Type"] <- "Seeded"
lpi_tall[lpi_tall$code %in% c("N"),
         "Type"] <- "gap"

#Create new dataframe without the non-plant codes
library(dplyr)
lpi_plants_only <- filter(lpi_tall, !(code %in% c("S","L","W","R","DF","DG","DS")))

#condense by grouping variable to create new dataframe
lpi_groups <- lpi_plants_only %>% group_by(PrimaryKey, LineKey, Line, PointLoc, PointNbr,
                                    Region, Type) %>% summarize(count=n())
#remove 'count' column
lpi_groups <- subset(lpi_groups, select = -c(count))
#Rename 'Type' column to 'code'
names(lpi_groups)[names(lpi_groups) == 'Type'] <- 'code'
#add 'layer' column back in
lpi_groups$layer <- NA
# write csv file for ease later
write.csv(lpi_groups, file = "int_inputs/lpi_weed_groups.csv", row.names = FALSE)

#read in lpi_weed_groups csv file
lpi_weed_groups <- read.csv("int_inputs/lpi_weed_groups.csv")

# calculate cover by weed, seeded, and other species
cover_weed_groups <- pct_cover(lpi_tall = lpi_weed_groups,
                           tall = F,
                           hit = "any",
                           by_line=FALSE,
                           code
                           )

# write weed/seed/other cover csv
write.csv(cover_weed_groups, file = "final_outputs/WeedSeedOther_cover.csv", row.names = F)

#Calculate bare soil percent cover
cover_baresoil <- pct_cover_bare_soil(lpi_tall = lpi_tall, tall = F, by_line = F) 

# write baresoil csv
write.csv(cover_baresoil, file = "final_outputs/baresoil_cover.csv", row.names = F)

# calculate percent cover between plants                 
# L: Herbaceous litter. WL: Woody litter. S: Bare soil. R: Rock.
# see the Monitoring Manual (Herrick et al 2017) for code definitions
cover_between_plants <- 
  pct_cover_between_plant(lpi_tall = lpi_tall, 
                          tall = F,
                          by_line = F)

## Height indicators ###
# Read in wide height csv
ht<-read.csv("orig_inputs/all_heights_wide.csv")

ht_tall <- pivot_longer(ht, cols = 5:7, names_to = "species",
                         values_to = "Height", values_drop_na = TRUE)

# Write tall LPI csv file
write.csv(ht_tall, file = 'int_inputs/ht_Tall.csv', row.names = FALSE)

#read tall height csv
ht_tall <- read.csv('int_inputs/ht_tall.csv')

# calculate mean height for each plant functional group
ht_pfg_mean <- 
  mean_height(ht_tall,
              method = "mean",
              omit_zero = F,
              by_line = F,
              tall = F,
              species)

# write mean height csv
write.csv(ht_pfg_mean, file = "final_outputs/all_ht_pfg_mean.csv", row.names = F)

# calculate mean of maximum heights at each point
ht_meanmax <-
  mean_height(ht_tall,
              method = "max",
              omit_zero = F,
              by_line = F,
              tall = T)

# write mean max height csv
write.csv(ht_meanmax, file = "final_outputs/all_ht_meanmax.csv", row.names = F)

### Gap indicators ###
# Read in gap csv
gap<- read.csv("orig_inputs/gap.csv")

# calculate gap cover indicators
gap_out <- 
  gap_cover(gap_tall = gap,
            tall = F,
            breaks = c(0),
            type = "canopy",
            by_line = F)

# write gap csv files
write.csv(gap_out["percent"], file = "final_outputs/gap_percent.csv", row.names = F)
write.csv(gap_out["n"], file = "final_outputs/gap_n.csv", row.names = F)
write.csv(gap_out["length"], file = "final_outputs/gap_length.csv", row.names = F)