# Load libraries
# if you do not have these libraries installed, please uncomment and run the following
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("readr")
# install.packages("scales")
# install.packages("sf")
# install.packages("viridis")
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(sf)
library(viridis)

# Acquire and clean data
# get Toronto census tracts shapefile
if (!file.exists("raw_data/lct_000a16a_e.shp")) {
  dir.create("raw_data", showWarnings = F)
  temp <- tempfile()
  download.file("http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lct_000a16a_e.zip", temp)
  unzip(temp, exdir = "raw_data", overwrite = T)
  unlink(temp)
}
canada_census_tract_location <- "raw_data/lct_000a16a_e.shp"
canada_data <- st_read(canada_census_tract_location)

# filtering all Canadian census tracts for just Torontonian tracts
canada_toronto <- filter(canada_data, CMANAME=="Toronto")

# modifying the data type to enable joining later
canada_toronto <- mutate(canada_toronto, CTNAME2 = as.numeric(as.character(CTNAME)))

# Census Tracts serviced by the TPL
if (!file.exists("raw_data/Branch_Census_Tracts.csv")) {
  dir.create("raw_data", showWarnings = F)
  download.file("https://opendata.tpl.ca/data/Branch_Information/Branch_Census_Tracts.csv", "raw_data/Branch_Census_Tracts.csv")
}

tpl_census_tracts <- read_csv("raw_data/Branch_Census_Tracts.csv", skip=1)
#filter out branch that has no specific census tract
tpl_census_tracts <- tpl_census_tracts[1:572, 1:3]
tpl_census_tracts$`Census Tract` <- as.numeric(as.character(tpl_census_tracts$`Census Tract`))

# join TPL-serviced census tracts to total Torontonian ones
tpl_join <- left_join(canada_toronto, tpl_census_tracts, by=c("CTNAME2"="Census Tract"))

# Filter out census tracts not served by TPL branches
tpl_join <- tpl_join %>% filter(!is.na(ID))

# Add multilingual circulation of TPL holdings
# First, download and import the appropriate csv
if (!file.exists("raw_data/Circulation_by_Language_Group.csv")) {
  dir.create("raw_data", showWarnings = F)
  download.file("https://opendata.tpl.ca/data/Circulation/Circulation_by_Language_Group.csv", "raw_data/Circulation_by_Language_Group.csv")
  tpl_circulation <- read_csv("raw_data/Circulation_by_Language_Group.csv", skip=3)
}

# Selecting just the most recent data, from 2017
tpl_circulation <- tpl_circulation[1:15]

# Selecting just the totals for each language, ignoring literacy level
tpl_circulation <- select(tpl_circulation, ID, "Branch Name", Tier, "Total ENG", "Total FRE", "Total MLC", "All Languages")
# Note: all languages other than English and French are condensed into "Multilingual"

# Then, clean the data further by keeping only neighbourhood library branches (NL),
# District Library branches (DL), and Reference and Research branches (RR)
tpl_circulation <- filter(tpl_circulation, Tier %in% c("NL", "DL", "RR"))

# Trimming to remove RR branches that are non-circulating collections
tpl_circulation <- filter(tpl_circulation, ID!= "MC")
tpl_circulation <- filter(tpl_circulation, ID!= "OC")

# Trimming to remove RR "branches" that multiple or ambiguous geographic areas,
# e.g. bookmobiles, interlibrary loan service, and phone services
tpl_circulation <- filter(tpl_circulation, ID!= "AL")
tpl_circulation <- filter(tpl_circulation, ID!= "IL")

# Convert totals to numeric data
tpl_circulation$`Total ENG` <- as.numeric(tpl_circulation$`Total ENG`)
tpl_circulation$`Total FRE` <- as.numeric(tpl_circulation$`Total FRE`)
tpl_circulation$`Total MLC` <- as.numeric(tpl_circulation$`Total MLC`)
tpl_circulation$`All Languages` <- as.numeric(tpl_circulation$`All Languages`)

# Create a column for percent of multilingual circulation
tpl_circulation <- mutate(tpl_circulation, percent_MLC = round((`Total MLC` / `All Languages` * 100), digits = 1))
tpl_circulation <- arrange(tpl_circulation, desc(tpl_circulation$percent_MLC))

# Join multilingual circulation data to Toronto census tracts
tpl_join_pmlc <- left_join(tpl_join, tpl_circulation, by="ID")
tpl_join_pmlc <- mutate(tpl_join_pmlc, percent_MLC_c = percent_MLC/100)

# Mapping
ggplot(canada_toronto) + 
  geom_sf(data=tpl_join_pmlc, aes(fill = percent_MLC_c), size=.1) + 
  scale_fill_viridis(label=percent, option="cividis", direction = -1, name="Multilingual\nCirculation") + 
  labs(title = "2017 Toronto Public Library circulation of multilingual\n(non-English, non-French) materials, by census tract", subtitle = "by Nicholas Field (@nk_field)", caption = "Sources: opendata.tpl.ca/ and open.canada.ca/") + 
  theme(plot.subtitle = element_text(hjust=0.5)) + 
  theme_void() + 
  coord_sf(datum = NA)

ggsave("output_data/tpl_multilingual_map.png", width=20, height=16, units="cm")

# That's it! Thanks for running my script. If you have problems, suggestions, or
# feedback, feel free to reach out to me at:
# Twitter: @nk_field
# Email: nicholaskenjifield@gmail.com