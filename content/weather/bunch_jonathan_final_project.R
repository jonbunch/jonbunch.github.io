# Assignment: Final Project
# Name: Bunch, Jonathan
# Date: 2010-08-06

library(ggplot2)
library(readxl)
library(plyr)
library(stats)
library(gvlma)

##
## Occurrence Data
##

# Import data and create subsets based on values of relevant variables.
occurrences <- read.delim("/Users/jonathanbunch/Documents/dsc520/final-project/data/drymarchon_occ/occurrence.txt")
occ_sub1 <- subset(occurrences, select = c(basisOfRecord, year, stateProvince, 
                                           specificEpithet, issue))
occ_sub2 <- subset(occ_sub1, stateProvince == "Florida" & specificEpithet == "couperi" 
                   & year >= 1970 & basisOfRecord == "HUMAN_OBSERVATION")
# Plot to see the distribution of sightings.
ggplot(data = occ_sub2, aes(x = year)) + geom_histogram(bins = 50)

# Create a new data table that represents the number of sightings per year.
occ_count <- as.data.frame(table(occ_sub2[, "year"]))
# This method converted the year to a factor. Convert it back to a number.
occ_count$Var1 <- as.numeric(as.character(occ_count$Var1))
# The year 2015 had no occurrences, so it was not included in our count data frame.
# We need to add it to the data frame with a zero value for frequency.
occ_count <- rbind(occ_count, c(2015, 0))
# The order() function will move our newly added row to the correct chronological position.
occ_count <- occ_count[order(occ_count$Var1), ]
# Plot to see how our "sightings per year" data frame looks.
ggplot(data = occ_count, aes(x = Var1, y = Freq)) + geom_col()

##
## Permit Data
##

# Import data and create subsets based on values of relevant variables.
permits <- read_xlsx("/Users/jonathanbunch/Documents/dsc520/final-project/data/fl_permits.xlsx")
permits$year <- as.integer(permits$year)
permits_sub1 <- subset(permits, select = -c(`1_unit_buildings`, `2_unit_buildings`,
                                            `3_and_4_unit_buildings`, `5_plus_unit_buildings`,
                                            total_buildings))
permits_sub2 <- subset(permits_sub1, year >= 1970)
permits_sub2$year <- as.integer(permits_sub2$year)
permits_sub3 <- subset(permits_sub2, select = c(total_construction_valuation, `1_unit_valuation`))

##
## Data by Five Year Blocks
##

# This function returns the sum of each five row block for a given variable.
blocks_func <- function(x) {
  return(c(sum(x[1:5]), sum(x[6:10]), sum(x[11:15]), sum(x[16:20]), sum(x[21:25]), 
           sum(x[26:30]), sum(x[31:35]), sum(x[36:40]), sum(x[41:45]), sum(x[46:50])))
}
# Block labels.
block_labels <- c("1970-1974", "1975-1979", "1980-1984", "1985-1989", "1990-1994",
                 "1995-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2019")
# New data frame with sums of values for each 5 year period.
blocks_df <- data.frame(years = block_labels, occurrences = blocks_func(occ_count$Freq),
                        total_const_valuation = blocks_func(permits_sub3$total_construction_valuation),
                        one_unit_valuation = blocks_func(permits_sub3$`1_unit_valuation`))

ggplot(data = blocks_df, aes(y = occurrences)) + geom_point(aes(x = total_const_valuation)) + 
  geom_smooth(aes(x = total_const_valuation), method = "lm", se=FALSE, color="black") +
  geom_point(aes(x = one_unit_valuation), color = "blue") +
  geom_smooth(aes(x = one_unit_valuation), method = "lm", se=FALSE, color="blue")

##
## Modeling
##

cor(blocks_df[,-1])

tot_val_lm <- lm(occurrences ~ total_const_valuation, data = blocks_df)
summary(tot_val_lm)
one_unit_val_lm <- lm(occurrences ~ one_unit_valuation, data = blocks_df)
summary(one_unit_val_lm)
multi_lm <- lm(occurrences ~ total_const_valuation + one_unit_valuation, data = blocks_df)
summary(multi_lm)

gvmodel_tot <- gvlma(tot_val_lm)
summary(gvmodel_tot)
gvmodel_one <- gvlma(one_unit_val_lm)
summary(gvmodel_one)
gvmodel_multi <- gvlma(multi_lm)
summary(gvmodel_multi)
gvmodel_tot_del <- deletion.gvlma(gvmodel_tot)
summary(gvmodel_tot_del)
