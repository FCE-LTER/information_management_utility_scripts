# Date: 2025-09-11
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

###### Introduction ###### 

# This script converts dates in text strings from DD-MMM-YY or DD-MMM-YYYY
# into ISO 8601 format: YYYY-MM-DD, prints a formatted output to the console,
# and writes the output to a .txt file

library(tidyverse)

###### Load example string as a character vector ###### 
text_with_raw_dates <- "Submission notes:
At the below sites and dates (listed to the nearest month), one third of the total plants in a plot were not measured:

SRS1b: 01-Jan-01, 01-Mar-01, 01-May-02, 01-Nov-02, 01-Jul-03, 01-Nov-03, 01-Jan-04, 01-Jan-05;

SRS1d: 01-Mar-07, 01-Nov-07, 01-Nov-08, 01-Jul-12, 01-Jan-17, 01-May-20, 01-Mar-24, 01-Nov-24, 01-Jan-25

SRS2: 01-Jan-01, 01-May-01, 01-Jul-01, 01-Sep-01, 01-Jan-02, 01-Mar-02, 01-May-02, 01-Jan-03, 01-Jan-04, 01-May-07, 01-Jan-12, 01-Sep-13, 01-Jan-14, 01-Jan-17, 01-Mar-17, 01-Sep-17, 01-Nov-17, 01-Jun-18, 01-Sep-18, 01-Nov-17, 01-Jan-18, 01-Jan-19, 01-Mar-19, 01-Sep-19, 01-Nov-19, 01-Jan-20, 01-Mar-20, 01-May-20, 01-Jul-20, 01-Sep-20, 01-Nov-20, 01-Jan-21, 01-Mar-21, 01-May-21, 01-Jul-21, 01-Sep-21, 01-Nov-21, 01-Jan-22, 01-Mar-22, 01-May-22, 01-Jul-22, 01-Sep-22, 01-Nov-22, 01-Jan-23,, 01-Mar-23, 01-May-23, 01-Jul-23, 01-Sep-23, 01-Nov-23, 01-Jan-24, 01-May-24, 01-Jul-24, 01-Sep-24, 01-Nov-25, 01-Jan-25.

SRS3: 01-Nov-00, 01-Mar-01, 01-May-01, 01-Jul-01, 01-Sep-01, 01-Mar-02, 01-Mar-07, 01-Jul-08, 01-Sep-09, 01-Nov-09, 01-Jan-10, 01-Sep-13, 01-Nov-13, 01-Jan-14, 01-Mar-14, 01-May-14, 01-Jul-14, 01-Sep-14, 01-Nov-14, 01-Jan-15, 01-Mar-15, 01-May-15, 01-Jul-15, 01-Sep-15, 01-Nov-15, 01-Jan-16, 01-Mar-16, 01-May-16, 01-Jul-16, 01-Sep-16, 01-Nov-16, 01-Jan-17, 01-Mar-17, 01-May-17, 01-Jun-17, 01-Sep-17, 01-Nov-17, 01-Jan-18, 01-Mar-18, 01-May-18, 01-Jun-18, , 01-Sep-18, 01-Nov-17, 01-Jan-19, 01-Mar-19, 01-May-19, 01-Jul-19, 01-Sep-19, 01-Nov-19, 01-Jan-20, 01-Mar-20, 01-May-20, 01-Jul-20, 01-Sep-20, 01-Nov-20, 01-Jan-21, 01-Mar-21, 01-May-21, 01-Jul-21, 01-Sep-21, 01-Nov-2021, 01-Jan-22, 01-Mar-22, 01-May-22, 01-Jul-22, 01-Sep-22, 01-Nov-22, 01-Jan-23, 01-Mar-23, 01-May-23, 01-Jul-23, 01-Sep-23, 01-Nov-23, 01-Jan-24, 01-Mar-24, 01-May-24, 01-Jul-24, 01-Sep-24, 01-Nov-24, 01-Jan-25.

Data are not available for Jul 2002 at site SRS1b at plot 1, so for Sawgrass_AG_Biomass (g/m2), Sawgrass_AG_Biomass_STD, Sawgrass_Culm_Density, and Sawgrass_Culm_Density_ STD only measurements from 2 plots were used (SRS1b-2 & SRS1b-3) during the Jul 2002 sampling event.

On 22-Aug-2011 a wildfire burned the vegetation at SRS1d plots, measurements of new plants started on 27-Sep-2011; from September 2011 until January 2015 three non-burned plots were stablished and measured at the site, the non-burned data (Sawgrass_AG_Biomass, Sawgrass_AG_Biomass_STD, Sawgrass_Culm_densidy & Sawgrass_Culm_Density_STD) is reported as SRS1d non-burned sites.

In September 2017 Hurricane Irma damaged all the plots at SRS2 and SRS3, measurements of new plants started in November 2017."


###### Convert dates ###### 

# Take a character vector of raw dates (text_with_raw_dates) and replace all matches
# of the patterns DD-MMM-YYY (e.g.,"12-Jan-2023") or DD-MMM-YY (e.g., "12-Jan-23")
# with a reformatted version.

# regex pattern:
#   (\\d{2})    -> two digits for the day
#   -           -> dash
#   (\\w{3})    -> three letters for the month abbreviation (e.g., Jan, Feb, Mar)
#   -           -> dash
#   (\\d{2,4})  -> two OR four digits for the year (handles both YY and YYYY)

# replacement function: applied to each regex match


date_text_formatted <- str_replace_all(text_with_raw_dates,
                             "(\\d{2})-(\\w{3})-(\\d{2,4})",
                             function(x) {
                               # Extract the year portion from the match
                               # (everything after the last dash).
                               # sub(".*-(\\d+)$", "\\1", x) isolates the year part.
                               # nchar(...) checks if it's 2 digits or 4 digits.
                               # -> If 2 digits, use "%d-%b-%y"
                               # -> If 4 digits, use "%d-%b-%Y"
                               dmy <- as.Date(x,
                                              format = ifelse(nchar(sub(".*-(\\d+)$", "\\1", x)) == 2,
                                                              "%d-%b-%y", "%d-%b-%Y"))
                               # Convert parsed Date object into ISO 8601 format: YYYY-MM-DD
                               format(dmy, "%Y-%m-%d")
                             })


###### Review output ###### 

# Concatenate and Print text with all dates formatted for visual review
cat(date_text_formatted)

# Extract all date values from date_text_formatted into a data frame and name the day
# of the week in a new column.This is helpful in checking if dates reported to the exact
# day are accurate (e.g. Saturdays and Sundays are less likely for most sampling)
all_dates_df <- str_extract_all(date_text_formatted, "\\d{4}-\\d{2}-\\d{2}") %>%
  unlist() %>%
  as.Date() %>%
  as_tibble() %>%
  rename(date = value) %>%
  mutate(day_of_week = weekdays(date))


# Filter the dates from all_dates_df so that only days greater than 1
# are included for review. This can help identify cases that should perhaps be
# listed as the beginning of the month (common for some datasets where data are
# sampled monthly), rather than an exact day of sampling.
filtered_dates_df <- all_dates_df %>%
  filter(lubridate::day(date) > 1)

# The only dates from the example that appear in filtered_dates_df are associated
# with the date of the mentioned wildfire and the date when new plants were
# measured after the fire.



######  Write output to file ###### 
writeLines(date_text_formatted,
           con = paste0("date_conversion_output_",
                        gsub("-", "_", Sys.Date()), ".txt"))
