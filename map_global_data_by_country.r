
install.packages("rworldmap")
library(rworldmap)

#export map - theres no package-specific function to export the plot
pdf("outputs/file_name.pdf",width=8,height=8)

#filter and prep data for mapping
df <- data  #include full country name or ISO code. package can match either to its internal list of countries

#match data on ISO code. 
viz <- joinCountryData2Map(dF = as.data.frame(pasture_chg_annual_00_16), #need to coerce to df or theres an error
                           joinCode="ISO3", nameJoinColumn="iso_code")

#specify details for map: variable to map, title, legend breaks, colors
mapParams <- mapCountryData(viz, 
                            nameColumnToPlot="value", # replace w/ column with values you want to visualize
                            mapTitle="Title", 
                            numCats = 20, #number of bins   #there are many options for visualizing. you can use EITHER this or next line
                            #catMethod = seq(-3.2 , 3.2, .1), #replace 3.2 values with min and max you're trying to visualize
                            colourPalette = c("#00a990", "white","#a33332"),   #min and max are BTI colors
                            oceanCol = "aliceblue",  
                            addLegend = FALSE, #leave FALSE to replace with legend with multiple labels and intervals below
                            missingCountryCol = "grey")     #choose color for countries that don't join properly or that are missing in your dataset

#plot map with legend added
do.call(addMapLegend, c(mapParams, legendLabels="all", legendIntervals = "page",legendWidth=0.5))

dev.off()  #this tells R not to export anything else in the plot
