# LOCF.R function

#create function to replace NAs with the last observation (last observation carried forward or LOCF)
na.locf2 <- function(x) zoo::na.locf(x, na.rm = FALSE)

# apply the LOCF function to each column in your dataframe (called dat below), grouping by country  so that if the first obs is NA, 
# the function doesn't replace with observation from different country
# if data can be grouped by another column e.g. food item (eg beef, milk) or fuel type (e.g. diesel, gas), write the name of that column after "country" below

dat <- transform(dat, column1 = ave(column1, country, item, FUN = na.locf2))

#repeat or loop for each column as necessary