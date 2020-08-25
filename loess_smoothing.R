#READ IN DATA
setwd("~/Google Drive File Stream/My Drive/Food & Farming/Metrics for Sustainable Intensification/Raw Data") #replace with folder location
df <- read.csv("Metrics for Tableau - for blog.csv")  #replace with file

# #replace all instances of x, y, and grouping_col with appropriate column names

# create loess fitted values, grouped by one column
my.loess <- df %>% 
  group_by(grouping_col) %>%
  arrange(grouping_col, x) %>% 
  mutate(loess = predict(loess(y ~ x)))  

#plot points
plot(df$x, df$y)  #original values
plot(my.loess$x, my.loess$loess)  #loess smoothed values

#plot lines
ggplot(df, aes(x,y, color = grouping_col))+  #original values
  geom_line()

ggplot(my.loess, aes(x, loess, color = grouping_col))+  #loess smoothed values
  geom_line()


# save / write out data
write.csv(x = my.loess, file = "smoothed_loess.csv") #specify output file name
