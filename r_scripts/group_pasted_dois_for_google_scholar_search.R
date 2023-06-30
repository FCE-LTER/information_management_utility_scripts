# Date: 2023-06-30
# Author: Gabriel Kamener
# Author email:gkamener@fiu.edu
# Organization:
# Florida Coastal Everglades LTER Program
# (Lead Principal Investigator: John Kominoski, jkominos@fiu.edu)
# Institute of Environment
# Florida International University
# 11200 SW 8th Street, OE 148
# Miami, FL 33199
# Website: https://fcelter.fiu.edu
# GitHub site: https://github.com/FCE-LTER

# This script fetches the most recent DOI for each FCE package in the
# Environmental Data Initiative's public repository and then groups and
# pastes them together as strings of five DOIs that can be input into
# Google Scholar search while meeting it's maximum character limit of 256
# characters.


# Load libraries 
library(tidyverse)
library(EDIutils)

# Create string containing today's date
todays_date <- gsub("-", "_", Sys.Date())

# Search for FCE data packages on EDI's production portal
query_prod <- search_data_packages(query = 'q=scope:(knb-lter-fce)&fl=doi,title,packageid,enddate',
                                   env = "production")

# Create list of resolvable DOIs
dois <- query_prod %>%
  select(doi) %>%
  mutate(doi = str_sub(doi, 5, -1))

# Create function to group DOIs into groups of 5
assign_row_groups <-function(x){
 x$row_group =  rep(1:ceiling(nrow(x)/5), each = 5)[1:nrow(x)]
 x
}

# Assign row groups
groups_assigned <- assign_row_groups(dois)

# Create output
output_grouped_doi_strings <- groups_assigned %>%
  # Create row_num var in order to supply something for header names
  # when pivoting to wide format
  mutate(row_num = row_number()) %>%
  # Group, pivot, and unite
  group_by(row_group) %>%
  pivot_wider(names_from = "row_num",
              values_from = "doi"
              ) %>%
  ungroup() %>%
  select(-row_group) %>%
  unite(., col = "united_doi",  1:ncol(.), na.rm=TRUE, sep = " OR ")

# Write to file
write_csv(output_grouped_doi_strings, paste("data/final_output/fce_dataset_doi_strings_for_scholar_search_",
                                            todays_date,
                                            ".csv",
                                            sep = "")
          )
