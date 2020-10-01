#template code for making BTI-style plot 
#attach packeges, load BTI colors, font, add_logo and save plot with logo functions
devtools::source_url("https://raw.githubusercontent.com/breakthroughinstitute/code_library/master/ggplot_bti_theme.R")


#manually create data frame to graph with data you have from another source and don't want to import 
data <- tibble(
  column1 = c("a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b"),
  column2 = c("group1", "group1", "group1","group2", "group2","group2", "group1", "group1","group1", "group1", "group1"),
  column3 = c(1,2,3,4,5,6, 7, 8, 9, 10, 11),
  column4 = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010)
  )

#faceted column plot
p <- data %>%  #select dataframe to use
  group_by(column1, column2) %>% summarize(average_value = mean(column3)) %>% #summarize by group
  ggplot(aes( #aes() is used to define the "aesthetics" like the variables used for x, y, color, shape, size etc. 
    x = column1, 
    y = average_value, 
    fill = column1)) + #you can have the fill, color of border, and other characteristics vary according to a variable's values. This iwll show up in legend by default
  geom_col(position = "dodge") + #this is a column plot with columns colored by group. deleting "dodge" will make it a stacked bar chart
  facet_wrap(vars(column2), #create a separate plot panel for each value of the selected variable(s)
             nrow = 1, #defines number of rows for displaying the panels
             ncol = 2, #defines number of columns for displaying the panels
             scales = "free_y") + # allows for different y axes for each plot panel. "free_x" does the same for x axis.
  labs(title = "Example: Mean Values by Group ", #graph title.
       x = "Observation Name", #x axis title 
       y = "Average Value") + #y axis title
  scale_fill_manual(values = bti_colors, name = "Legend") + #name should be the variable used for grouping / coloring
  scale_y_continuous(expand = c(0,0)) + #makes bottom of data align w/ x axis (removes space b/t axis and data)
  theme_bti() +
  theme( #text = element_text(family = "Futura"), # add only when done since you can't see plot when using Futura font
        ##titles    
        # plot.title = element_text(size = 24),  #  change title font size
        # strip.text.x = element_text(size = 14), # change facet title font size

        ## axis lines
        # axis.line.x = element_line(color="black", size = .25),
        # axis.line.y = element_line(color="black", size = .25),
    
        ##gridlines and border
        # panel.grid.major.y = element_line(size = .25), #add y axis grid line 
        # panel.grid.major.x = element_line(size = .25), #add x axis grid line
        # panel.border = element_rect(color = "black", fill = NA, size = 1), #add border to plots. Should only be used for faceted plots
        # axis.line.x = element_blank(), # if you add panel border, remove axes lines
        # axis.line.y = element_blank(), # if you add panel border, remove axes lines
        
        ##legend
        # legend.position = "none",  # remove legend
        # legend.text = element_text(size=14), #change font size of legend items
        # legend.title = element_text(size=14), #change font size of legend title
        
        ## axes
        # axis.title.y = element_text(size = 16), # change font size of axis title
        # axis.title.y = element_blank(), # remove axis title 
        # axis.title.x = element_text(size = 16), # change font size of axis title
        # axis.title.x = element_blank(), # remove axis title
    
        # axis.text.x = element_text(size = 16), #change font size of  axis label text 
        # axis.text.x = element_blank(), #remove axis labels
        # axis.text.y = element_text(size = 16), #change font size of  axis label text 
        # axis.text.y = element_blank(), #remove axis labels

        # axis.ticks.y = element_blank() #remove axis tick marks
        # axis.ticks.x = element_blank() #remove axis tick marks 
        ) + 
  guides(fill = guide_legend(nrow=1, byrow=TRUE)) #set number of rows for legend >1 to ensure it doesnt go off the sides of plot

p1

save_plot_with_logo(plot_name = p1, file_name = "column_plot_with_logo.png")



# Scatter Plot ------------------------------------------------------------
p2 <- data %>%  #select dataframe to use
  ggplot(aes( #aes() is used to define the "aesthetics" like the variables used for x, y, color, shape, size etc. 
    x = column4, 
    y = column3)) + #you can have the fill, color of border, and other characteristics vary according to a variable's values. This iwll show up in legend by default
  geom_point(aes(color = column2)) + #vary color by group
  labs(title = "Example:  All Values ", #graph title.
       x = "Year", #x axis title 
       y = "Value") + #y axis title
  scale_color_manual(values = bti_colors, name = "Legend") + #name should be the variable used for grouping / coloring
  theme_bti() +
  theme() # add any customizations in this theme argument

p2

save_plot_with_logo(plot_name = p2, file_name = "scatter_plot_with_logo.png")