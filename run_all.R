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

# Step 1: Acquire and clean data
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

# Step 2: Graphing
# Make plot, from highest percent multilingual circulation to lowest,
# and with coord_flip, for tidiness
# and identifying branch type by colour,
# with colours that should be accessible for colourblind readers
ggplot(data = tpl_circulation, aes(x=fct_reorder(`Branch Name`, percent_MLC), y = percent_MLC, fill=Tier)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  theme_minimal() + 
  labs(x="Library branch", y="Percent of annual circulation in non-English, non-French languages", title="Multilingual Circulation in Toronto Public Library branches in 2017", subtitle="by Nicholas Field @nk_field", caption="Source: https://opendata.tpl.ca/") + 
  scale_fill_manual(values=c("#7fb3d5", "#d4e6f1", "#2980b9"), name="Library\nBranch Tier", breaks=c("NL", "DL", "RR"), labels=c("Neighbourhood library", "District library", "Research and\nReference library")) + 
  theme(plot.title = element_text(hjust=0.6), plot.caption = element_text(hjust=0.5), plot.subtitle = element_text(hjust=0.5)) + 
  scale_y_continuous(sec.axis = dup_axis())

# Save with resizing for legibility
ggsave("output_data/tpl_multilingual_graph.png", width=20, height=30, units="cm")

# Step 3: Mapping
ggplot(canada_toronto) + 
  geom_sf(data=tpl_join_pmlc, aes(fill = percent_MLC_c), size=.1) + 
  scale_fill_viridis(na.value="transparent", label=percent, option="cividis", direction = -1, name="Multilingual\nCirculation") + 
  labs(title = "2017 Toronto Public Library circulation of multilingual\n(non-English, non-French) materials, by census tract", subtitle = "by Nicholas Field (@nk_field)", caption = "Sources: opendata.tpl.ca/ and open.canada.ca/") + 
  theme(plot.subtitle = element_text(hjust=0.5)) + 
  theme_void() + 
  coord_sf(datum = NA)

ggsave("output_data/tpl_multilingual_map.png", width=20, height=16, units="cm")

# Step 4: Multilingual census tract information
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

# saving output
write_csv(toronto, "output_data/toronto2016ct.csv")

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

# saving output
write_csv(toronto_lang_pop, "output_data/toronto2016lang_pop_ct.csv")

# join to data above
tpl_join_pmlc_ct <- left_join(tpl_join_pmlc, toronto_lang_pop, by=c("CTNAME2"="CT_GEO2"))

# create percentages
tpl_join_pmlc_ct <- mutate(tpl_join_pmlc_ct, percent_know_no_ef = round((know_no_ef / total_pop), digits = 3))
tpl_join_pmlc_ct <- mutate(tpl_join_pmlc_ct, percent_mother_not_ef = round((mother_not_ef / total_pop), digits = 3))
tpl_join_pmlc_ct <- mutate(tpl_join_pmlc_ct, percent_home_not_ef = round((home_not_ef / total_pop), digits = 3))
tpl_join_pmlc_ct <- mutate(tpl_join_pmlc_ct, percent_home_other_not_ef = round((home_other_not_ef / total_pop), digits = 3))
tpl_join_pmlc_ct <- mutate(tpl_join_pmlc_ct, percent_home_total_not_ef = percent_home_not_ef + percent_home_other_not_ef)
tpl_join_pmlc_ct <- mutate(tpl_join_pmlc_ct, gap_know_no_ef = (percent_MLC_c - percent_know_no_ef))

# Step 5: Mapping Toronto's population that do not speak English or French
ggplot(canada_toronto) + 
  geom_sf(data=tpl_join_pmlc_ct, aes(fill = percent_know_no_ef), size=.1) + 
  scale_fill_viridis(na.value="transparent", label=percent, option="cividis", direction = -1, name="Percent of\nPopulation") + 
  labs(title = "What percent of each branch's catchment population\ndoes not speak English or French?", 
       subtitle = "by Nicholas Field (@nk_field)", 
       caption = "Sources: opendata.tpl.ca/, open.canada.ca/, and www12.statcan.gc.ca/\nLibrary circulation data from 2017, census data from 2016") + 
  theme(plot.title = element_text(hjust=0), plot.subtitle = element_text(hjust=0.5)) + 
  theme_void() + 
  coord_sf(datum = NA)
ggsave("output_data/no_ef_map.png", width=20, height=16, units="cm")

# Step 6: Graphing the gap between populations that don't speak English or French and the libraries that serve them
tpl_join_pmlc_ct2 <- filter(tpl_join_pmlc_ct, total_pop!= 0)
tpl_join_pmlc_ct2 <- filter(tpl_join_pmlc_ct2, CTNAME2 != 205.00)

just_avg_no_ef <- tpl_join_pmlc_ct2 %>% group_by(`Branch Name`) %>% summarise(avg_pc_know_no_ef = round(mean(percent_know_no_ef), digits = 4) * 100, avg_gap = round(mean(gap_know_no_ef), digits = 4) * 100)
avg_join <- left_join(tpl_circulation, just_avg_no_ef, by="Branch Name")
avg_join <- filter(avg_join, `Branch Name` != "Toronto Reference Library")
avg_join <- arrange(avg_join, avg_gap)
ggplot(avg_join) +
  geom_segment(
    aes(x = avg_pc_know_no_ef,
        y = fct_reorder(`Branch Name`, avg_gap),
        xend = percent_MLC,
        yend = fct_reorder(`Branch Name`, avg_gap))) +
  geom_point(aes(x=percent_MLC, y=`Branch Name`), color="light blue") +
  geom_point(aes(x=avg_pc_know_no_ef, y=`Branch Name`), color="dark blue") +
  labs(x="Percent", 
       y="Library branch", 
       title="Difference between non-English, non-French\nToronto Public Library circulation and\nlocal population that knows neither language", 
       subtitle="Dark blue = Average branch's population that\ndoes not know English or French,\nLight blue = circulation of that branch's\nnon-English, non-French material", 
       caption="Sources: opendata.tpl.ca/, open.canada.ca/, and www12.statcan.gc.ca/\nLibrary circulation data from 2017, census data from 2016.\nby Nicholas Field @nk_field") + 
  theme_minimal() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks=seq(0,30,5), limits=c(0,30), sec.axis = dup_axis()) +
  scale_fill_continuous(na.value="transparent")

ggsave("output_data/tpl_gap_graph.png", width=20, height=30, units="cm")

# Step 7: Mapping the gap
# What is the difference between our first and second maps?
ggplot(canada_toronto) + 
  geom_sf(data=tpl_join_pmlc_ct, aes(fill = gap_know_no_ef), size=.1) + 
  scale_fill_viridis(na.value="transparent", label=percent, option="cividis", direction = -1, name="Margin") + 
  labs(title = "If you don't speak English or French,\nhow well does your library serve you?", 
       subtitle = "Difference between non-English, non-French library circulation\nand local population that does not speak English or French.\nNote: Areas below 0% see less multilingual use\nthan their population would suggest.", 
       caption = "Sources: opendata.tpl.ca/, open.canada.ca/, and www12.statcan.gc.ca/\nLibrary circulation data from 2017, census data from 2016\nby Nicholas Field (@nk_field)") + 
  theme(plot.title = element_text(hjust=0), plot.subtitle = element_text(hjust=0.5)) + 
  theme_void() + 
  coord_sf(datum = NA)
ggsave("output_data/tpl_gap_map.png", width=20, height=16, units="cm")

# Step 8: Mapping anyone in Toronto whose first language is not English or French
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