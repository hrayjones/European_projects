#### The data for countries were obtained as follows:

#### Source "1MG" is from the following website:
#### https://dashboard.onemilliongenomes.eu

#### Souce "Fede" is from Nicole Soranzo's group (manual curation)

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

data(efta_countries)
kable(efta_countries)


SHP_0 <- get_eurostat_geospatial(resolution = 10, 
                                 nuts_level = 0, 
                                 year = 2016)

SHP_0 %>% 
  ggplot() +
  geom_sf()

### just need to get the geo code. Adding to Excel sheet.
setwd("~/Documents/Erasmus")
mydata <- fread("./countries_eurostat_input.txt")
mydata[, top := max(Total), by = "Country"] # take the top of either 1_MG or Fede
mydata[, top := as.numeric(top)]
mydata_short1 <- unique(mydata[, .(top, Popn, geo)])
mydata_short1[, Ratio := top/Popn]
mydata_short <- mydata_short1[, .(geo, Ratio)]

### Connect with spatial data
### But we want to keep other countries!
EU27 <- eu_countries %>% 
  select(geo = code, name)
print(EU27, n = 27)
to_add <- data.table("geo" = c("BE", "BG", "LU", "AT", "RO", 
                               "SI", "SK"), 
                     "Ratio" = c("NA", "NA", "NA", "NA", "NA", 
                                 "NA", "NA"))
all_countries_info <- rbind(mydata_short, to_add)
all_countries_info[, Ratio_SF := signif(as.numeric(Ratio), digits = 2)]

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
  ggplot(aes(fill = Ratio)) + 
  geom_sf() + 
  scale_x_continuous(limits = c(-22, 37)) + 
  scale_y_continuous(limits = c(35, 65)) + 
  theme_void()

### We need to look at how to represent the numbers, because they are distributed so weirdly.
### Try looking at quantiles.
myquantiles <- quantile(all_countries_info$Ratio_SF, probs = seq(0, 1, 0.2), 
                        na.rm = T)
all_countries_info[, Ratio_bin := cut2(Ratio_SF, cuts = as.numeric(myquantiles))]
str(all_countries_info)

SHP_29 <- SHP_0 %>% 
  select(geo = NUTS_ID, geometry) %>% 
  inner_join(all_countries_info, by = "geo") %>% 
  arrange(geo) %>% 
  st_as_sf()
SHP_29 %>%
  ggplot(aes(fill = Ratio_bin)) + 
  geom_sf() + 
  scale_x_continuous(limits = c(-22, 37)) + 
  scale_y_continuous(limits = c(35, 65)) + 
  theme_void()

# Take inspiration from this and look at rounded numbers
# First bin is < 0.001 (0.1%)
# Second is < 0.002 (0.2%)
# Third is < 0.006 (0.6%)
# Fourth is <0.02 (20%)
# Fifth is <0.75 (75%)

Bin1 <- 0.001
Bin2 <- 0.002
Bin3 <- 0.006
Bin4 <- 0.02
Bin5 <- 0.75

## Make a label for this
all_countries_info[Ratio < Bin1 & Ratio >= 0, bin := Bin1]
all_countries_info[Ratio < Bin2 & Ratio >= Bin1, bin := Bin2]
all_countries_info[Ratio < Bin3 & Ratio >= Bin2, bin := Bin3]
all_countries_info[Ratio < Bin4 & Ratio >= Bin3, bin := Bin4]
all_countries_info[Ratio < Bin5 & Ratio >= Bin4, bin := Bin5]

all_countries_info[bin == Bin1, name := "< 0.1%"]
all_countries_info[bin == Bin2, name := "< 0.2%"]
all_countries_info[bin == Bin3, name := "< 0.6%"]
all_countries_info[bin == Bin4, name := "< 2%"]
all_countries_info[bin == Bin5, name := "< 75%"]


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
    title = "Percentage of population with genetic data (WGS or array)",
    caption = "Source data: 1+MG + manual curation by Federica Santonastaso"
  ) +
  
  theme_void()  + 
  theme(
    legend.position = c(0.93, 0.5)
  )
print(finalplot)
#######################################

pdf(file = "./EUR_colorcoded_HRJ_FS.pdf")
print(finalplot)
dev.off()
