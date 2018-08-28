# Step 0: Load libraries
# if you do not have these libraries installed, please uncomment and run the following
# install.packages("dplyr")
# install.packages("forcats")
# install.packages("ggplot2")
# install.packages("readr")
# install.packages("scales")
# install.packages("sf")
# install.packages("tidyr)
# install.packages("viridis")
library(dplyr)
library(forcats)
library(ggplot2)
library(readr)
library(scales)
library(sf)
library(tidyr)
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

# save this cleaned data frame
write_csv(tpl_census_tracts, "output_data/tpl_census_tracts_cleaned.csv")

# join TPL-serviced census tracts to total Torontonian ones
tpl_join <- left_join(canada_toronto, tpl_census_tracts, by=c("CTNAME2"="Census Tract"))

# Filter out census tracts not served by TPL branches
tpl_join <- tpl_join %>% filter(!is.na(ID))

# Add multilingual circulation of TPL holdings
# First, download and import the appropriate csv
if (!file.exists("raw_data/Circulation_by_Language_Group.csv")) {
  dir.create("raw_data", showWarnings = F)
  download.file("https://opendata.tpl.ca/data/Circulation/Circulation_by_Language_Group.csv", "raw_data/Circulation_by_Language_Group.csv")
}

tpl_circulation <- read_csv("raw_data/Circulation_by_Language_Group.csv", skip=3)

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

# Save this modified data frame
write_csv(tpl_circulation, "output_data/tpl_circulation_mlc.csv")

# Join multilingual circulation data to Toronto census tracts
tpl_circulation[29,2] <- "Dufferin St Clair"
tpl_circulation[25,2] <- "Parliament"
tpl_circulation[12,2] <- "Lillian H Smith"
tpl_circulation[47,2] <- "North York Central"
tpl_circulation[67,2] <- "Mimico"
tpl_circulation[43,2] <- "Maria A Shchuka"
tpl_circulation[64,2] <- "Oakwood Village"
tpl_circulation[92,2] <- "Annette"
tpl_circulation[73,2] <- "St Clair Silverthorn"
tpl_circulation[90,2] <- "Swansea"
tpl_join_pmlc <- left_join(tpl_join, tpl_circulation, by=c("ID", "Branch Name"))
tpl_join_pmlc <- mutate(tpl_join_pmlc, percent_MLC_c = percent_MLC/100)
write_csv(tpl_join_pmlc, "output_data/tpl_join_pmlc.csv")

# Multilingual census tract information
# Warning! This is a huge file (~ 130 MB zipped, ~1.3 GB unzipped). Downloading and processing may take some time.
# If you want to avoid downloading this file, skip ahead to SKIP_ALL_THAT below

# Census metropolitan areas, tracted census agglomerations and census tracts
# zip: https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/download-telecharger/comp/GetFile.cfm?Lang=E&TYPE=CSV&GEONO=043

if (!file.exists("raw_data/98-401-X2016043_English_CSV_data.csv")) {
  dir.create("raw_data", showWarnings = F)
  temp <- tempfile()
  download.file("https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/prof/details/download-telecharger/comp/GetFile.cfm?Lang=E&TYPE=CSV&GEONO=043", temp)
  unzip(temp, exdir = "raw_data", overwrite = T)
  unlink(temp)
}

canada <- read_csv("raw_data/98-401-X2016043_English_CSV_data.csv")

# just the Toronto data, please!
# specifying rows for Toronto census tracts 1:802.2, which are the tracts serviced by the TPL
toronto <- canada[4646798:7136472,]
# removing other irrelevant information
toronto <- toronto %>% filter(GEO_NAME!="Toronto")
toronto <- toronto %>% select(-DATA_QUALITY_FLAG, -GNR, -GNR_LF, -`Notes: Profile of Census Tracts (2247)`, -ALT_GEO_CODE, -GEO_LEVEL, -CENSUS_YEAR, -`GEO_CODE (POR)`, -`Dim: Sex (3): Member ID: [2]: Male`, -`Dim: Sex (3): Member ID: [3]: Female`)

# Please note that census tract 6 has no population data, though it has a geographic region associated with it
# Likewise tract 205.00 has a small population (149) but no other data associated with it

# Or, SKIP_ALL_THAT and uncomment and run this instead
# toronto <- read_csv("output_data/toronto2016ct.csv")

# Selecting data regarding total population and use of languages other than English and French
#   1 == total population of a given census tract
# 104 == pop with no knowledge of either English or French
# 117 == pop with mother tongue which is neither English or French
# 386 == pop with language most spoken at home being neither English or French
# 654 == pop with other languages spoken at home (e.g. second or third most spoken at home) which are neither Eng or Fre
toronto_lang_pop <- toronto %>% filter(`Member ID: Profile of Census Tracts (2247)` == 1 | `Member ID: Profile of Census Tracts (2247)` == 104 | `Member ID: Profile of Census Tracts (2247)` == 117 | `Member ID: Profile of Census Tracts (2247)` == 386 | `Member ID: Profile of Census Tracts (2247)` == 654)
toronto_lang_pop <- select(toronto_lang_pop, -`DIM: Profile of Census Tracts (2247)`)
names(toronto_lang_pop)[3]<-"population"

# tidying data
toronto_lang_pop = spread(toronto_lang_pop, `Member ID: Profile of Census Tracts (2247)`, population)
names(toronto_lang_pop) <- c("CT_GEO", "total_pop", "know_no_ef", "mother_not_ef", "home_not_ef", "home_other_not_ef")
toronto_lang_pop <- mutate(toronto_lang_pop, CT_GEO2 = as.numeric(as.character(CT_GEO)))
toronto_lang_pop <- select(toronto_lang_pop, -CT_GEO)

# join to data above
tpl_join_pmlc_ct <- left_join(tpl_join_pmlc, toronto_lang_pop, by=c("CTNAME2"="CT_GEO2"))

# create percentages
tpl_join_pmlc_ct <- mutate(tpl_join_pmlc_ct, percent_know_no_ef = round((know_no_ef / total_pop), digits = 3))
tpl_join_pmlc_ct <- mutate(tpl_join_pmlc_ct, percent_mother_not_ef = round((mother_not_ef / total_pop), digits = 3))
tpl_join_pmlc_ct <- mutate(tpl_join_pmlc_ct, percent_home_not_ef = round((home_not_ef / total_pop), digits = 3))
tpl_join_pmlc_ct <- mutate(tpl_join_pmlc_ct, percent_home_other_not_ef = round((home_other_not_ef / total_pop), digits = 3))
tpl_join_pmlc_ct <- mutate(tpl_join_pmlc_ct, percent_home_total_not_ef = percent_home_not_ef + percent_home_other_not_ef)
tpl_join_pmlc_ct <- mutate(tpl_join_pmlc_ct, gap_know_no_ef = (percent_MLC_c - percent_know_no_ef))

# Mapping anyone in Toronto whose first language is not English or French
ggplot(canada_toronto) + 
  geom_sf(data=tpl_join_pmlc_ct, aes(fill = percent_mother_not_ef), size=.1) + 
  scale_fill_viridis(na.value="transparent", label=percent, option="cividis", direction = -1, name="Percent") + 
  labs(title = "Whose first language is not English or French?", 
       subtitle = "Plotted according to Toronto census tracts", 
       caption = "Sources: opendata.tpl.ca/, open.canada.ca/, and www12.statcan.gc.ca/\nLibrary circulation data from 2017, census data from 2016\nby Nicholas Field (@nk_field)") + 
  theme(plot.title = element_text(hjust=0), plot.subtitle = element_text(hjust=0.5)) + 
  theme_void() + 
  coord_sf(datum = NA)
ggsave("output_data/first_language_map.png", width=20, height=16, units="cm")

# That's it! Thanks for running my script. If you have problems, suggestions, or
# feedback, feel free to reach out to me at:
# Twitter: @nk_field
# Email: nicholaskenjifield@gmail.com