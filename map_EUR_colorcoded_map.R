#### July 2025
#### The data for countries were obtained as follows:

#### "1MG" from the following website:
#### https://dashboard.onemilliongenomes.eu
#### Federica's manual curation 
#### Helen's manual curation
#### Also checked numbers from Walter Wolfsberger (Ukraine), where missing from other sources 
#### https://globgen.uzhnu.edu.ua/world-geo-data/
#### We did not include any information from personal communication, so all numbers here are citable/public domain only.					
#### As of July 2025.

####
#### Important Notes:					
#I queried ChatGPT (mostly via GPT-4o) on 21/07/25					
#I supplied chatGPT with all the studies identifie in the abpve sources, and asked it to look more closely when it didn't find something. But note, very often the public information is sparse.					
# Some of the studies inputted by Federica (e.g. The French Constances cohort) had no publicly confirmed genomes, so ChatGPT concluded to put zero for those.
# chatGPT notes that:
#Numbers are approximate and reflect the best available data from public sources and literature.					
#Some countries have no identified WGS cohorts beyond small pilot projects or none publicly reported.					
#The “International Genome Project” datasets include WGS samples linked to specific countries but are counted only once per country above.					
#Exome and targeted sequencing studies are excluded.					
#Genome of Europe projected samples are excluded.					
					
#Additional studies involving multiple countries, for which we cannot separate sample numbers by country:					
#Study Name	Countries Involved	Focus Area	Sample Size	WGS Data Availability	Notes
#Solve-RD	15 European countries	Rare diseases	6,447	Yes	Pan-European reanalysis of rare disease data
#ERN EURO-NMD	Belgium, Canada, Finland, France, Germany, Italy, Spain, UK	Neuromuscular disorders	2,125	Yes	Reanalysis of neuromuscular disorder data
#Kuusamo & Crete Isolates	Finland, Greece	Population isolates	Not specified	Yes	WGS data from isolated populations
#ERN GENTURIS	Germany, Netherlands, Portugal, Spain	Genetic tumor risk syndromes	390	Yes	Reanalysis of genetic tumor risk data

#### Population nos are from wikipedia, accessed 29/02/2024

#### The following will be done:
## Take the top entry for each country (max no. samples)
## Bin the proportions of citizens
## Plot on a map of Europe
options(scipen=999)
library(eurostat)
library(knitr)
library(sf)
library(giscoR)
library(ggplot2)
library(cowplot)
library(ggthemes)
library(data.table)
library(Hmisc)
library(dplyr)
library(stringr)

data(efta_countries)
kable(efta_countries)


SHP_0 <- get_eurostat_geospatial(resolution = 10, 
                                 nuts_level = 0, 
                                 year = 2016)

SHP_0 %>% 
  ggplot() +
  geom_sf()

### just need to get the geo code. Adding to Excel sheet.
### just need to get the geo code. Adding to Excel sheet.
setwd("~/Documents/Erasmus")
mydata <- fread("./countries_eurostat_input_chatGPT.txt")
mydata[, samples := str_replace_all(`Approximate WGS Samples`, "~", "")]
mydata[, samples := str_replace_all(`samples`, ",", "")]
mydata[, samples := str_replace_all(`samples`, "\\+", "")]
mydata[, samples := as.numeric(samples)]
mydata[, Total := sum(samples), by = c("geo", "Country")] 

populations <- fread("./countries_populations.txt")
populations <- unique(populations)
mydata_popn <- populations[mydata, on = "geo"]

mydata_short1 <- unique(mydata_popn[, .(Popn, Total, geo)])
mydata_short1[, Ratio := Total/Popn] 
mydata_short <- mydata_short1[, .(geo, Ratio)]

### Connect with spatial data
### But we want to keep other countries!
EU27 <- eu_countries %>% 
  select(geo = code, name)
print(EU27, n = 27)

#to_add <- data.table("geo" = c("BE", "BG", "LU", "AT", "RO", 
#                               "SI", "SK"), 
#                     "Ratio" = c("NA", "NA", "NA", "NA", "NA", 
#                                 "NA", "NA"))
#to_add <- data.table("geo" = c("BE", "LU", "AT", 
#                               "SI"), 
#                     "Ratio" = c("NA", "NA", "NA", "NA"))

#all_countries_info <- rbind(mydata_short, to_add)
all_countries_info <- copy(mydata_short)
all_countries_info[, Ratio_SF := signif(as.numeric(Ratio), digits = 2)]
all_countries_info[, percentage := Ratio_SF * 100]

SHP_29 <- SHP_0 %>% 
  select(geo = NUTS_ID, geometry) %>% 
  inner_join(all_countries_info, by = "geo") %>% 
  arrange(geo) %>% 
  st_as_sf()

SHP_29 %>%
  ggplot() + 
  geom_sf()

### Crop to our area of interest:
SHP_29 %>%
  ggplot(aes(fill = percentage)) +  # should be percentage
  scale_fill_viridis_c("% of popn") +
  geom_sf() + 
  scale_x_continuous(limits = c(-22, 37)) + 
  scale_y_continuous(limits = c(35, 65)) + 
  theme_void()
##################

### Binning strategy

##################

### We need to look at how to represent the numbers, because they are distributed so weirdly.
# Take inspiration from this and look at rounded numbers
# First bin is < 0.0001 (0.01%)
# Second bin is < 0.0005 (0.05%)
# Third bin is < 0.001 (0.1%)
# Fourth is < 0.005 (0.5%)
# Fifth is <0.01 (1%)
# Sixth is <0.5 (50%)

Bin1 <- 0.0001
Bin2 <- 0.0005
Bin3 <- 0.001
Bin4 <- 0.005
Bin5 <- 0.01
Bin6 <- 0.5

## Make a label for this
all_countries_info[Ratio < Bin1 & Ratio >= 0, bin := Bin1]
all_countries_info[Ratio < Bin2 & Ratio >= Bin1, bin := Bin2]
all_countries_info[Ratio < Bin3 & Ratio >= Bin2, bin := Bin3]
all_countries_info[Ratio < Bin4 & Ratio >= Bin3, bin := Bin4]
all_countries_info[Ratio < Bin5 & Ratio >= Bin4, bin := Bin5]
all_countries_info[Ratio < Bin6 & Ratio >= Bin5, bin := Bin6]

all_countries_info[bin == Bin1, name := "< 0.01%"]
all_countries_info[bin == Bin2, name := "< 0.05%"]
all_countries_info[bin == Bin3, name := "< 0.1%"]
all_countries_info[bin == Bin4, name := "< 0.5%"]
all_countries_info[bin == Bin5, name := "< 1%"]
all_countries_info[bin == Bin6, name := "< 50%"]


SHP_29 <- SHP_0 %>% 
  select(geo = NUTS_ID, geometry) %>% 
  inner_join(all_countries_info, by = "geo") %>% 
  arrange(geo) %>% 
  st_as_sf()


#######################################
finalplot <- SHP_29 %>%
  ggplot(aes(fill = name)) + 
  geom_sf() + 
  scale_fill_viridis_d("% of popn") +
  scale_x_continuous(limits = c(-22, 38)) + 
  scale_y_continuous(limits = c(35, 66)) + 
  labs(
    title = "Percentage of population with reported WGS data across Europe",
    caption = "Source data: manual curation by Federica Santonastaso and Helen Ray-Jones,\n with data-mining assistance from ChatGPT v4o (July 2025)"
  ) +
  
  theme_void()  + 
  theme(
    legend.position = c(0.93, 0.5)
  )
print(finalplot)

######################################
## This one looks a lot better

pdf(file = "./EUR_colorcoded_HRJ_FS_manualBins_chatGPT.pdf")
print(finalplot)
dev.off()

######################################

#### NOTE: the below was when I also plotted the ancestry.com panel data
#### The idea is to show how certain ancestries are under-represented in this panel.
#### The numbers can be found in Google Doc.
#### I think I had to add "NA" for the following countries:
#to_add <- data.table("geo" = c("BE", "LU", "AT", 
#                               "SI"), 
#                     "Ratio" = c("NA", "NA", "NA", "NA"))
####
finalplot <- SHP_29 %>%
  ggplot(aes(fill = percentage)) + 
  geom_sf() + 
  scale_fill_viridis_c("% of popn") +
  scale_x_continuous(limits = c(-22, 38)) + 
  scale_y_continuous(limits = c(35, 66)) + 
  labs(
    title = "Ancestry.com panel, array data",
    caption = "Source data: Ancestry.com"
  ) +
  
  theme_void()  + 
  theme(
    legend.position = c(0.93, 0.5)
  )
print(finalplot)

jpeg(file = "./EUR_colorcoded_HRJ_Ancestry_panel.jpg", res=300)
print(finalplot)
dev.off()
######################################
#######################################
finalplot <- SHP_29 %>%
  ggplot(aes(fill = name)) + 
  geom_sf() + 
  scale_fill_viridis_d("% of popn") +
  scale_x_continuous(limits = c(-22, 38)) + 
  scale_y_continuous(limits = c(35, 66)) + 
  labs(
    title = "Ancestry.com panel, array data",
    caption = "Source data: Ancestry.com"
  ) +
  
  theme_void()  + 
  theme(
    legend.position = c(0.93, 0.5)
  )
print(finalplot)
#######################################

pdf(file = "./EUR_colorcoded_HRJ_Ancestry_panel.pdf")
print(finalplot)
dev.off()

