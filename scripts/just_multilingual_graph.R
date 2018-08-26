# Load libraries
# if you do not have these libraries installed, please uncomment and run the following
# install.packages("dplyr")
# install.packages("forcats")
# install.packages("ggplot2")
# install.packages("readr")
# install.packages("scales")
# install.packages("sf")
# install.packages("viridis")
library(dplyr)
library(forcats)
library(ggplot2)
library(readr)
library(scales)

# Acquire and clean data
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

# Graphing
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

# That's it! Thanks for running my script. If you have problems, suggestions, or
# feedback, feel free to reach out to me at:
# Twitter: @nk_field
# Email: nicholaskenjifield@gmail.com