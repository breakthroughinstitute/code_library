#set up
require(tidyverse); require(RCurl); require(grid); require(magick)
require(showtext) #for using downloaded Futura font (must download or buy futura from internet first)
bti_colors <- c("#0d4459", "#00a990", "#d05527", "#a33332",
                "#b381d0",  "#dfb9a6", "#b2b2b1", "#2A2A2A", "#ecc627")

#set font
font_add(family="Futura", regular = "~/Library/Fonts/Futura Medium.ttf")
showtext_auto()

#alternative value if you dont have futura on comptuer is "Helvetica"
font_family <-"Futura"   

## IMPORTANT NOTE: Using the FUTURA Font (or other ones using Showtext package) prevents plots 
## from showing up! You will need to export/save the plots to view them 
## or view them with x11(plot_name), but this graphics views has low resolution. 


# Function for Loading logo
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 4){
  
  # Requires magick R Package https://github.com/ropensci/magick
  
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
  
}


# Function for saving plot with logo
save_plot_with_logo <- function(plot_name, file_name, width_in = 6.5, height_in = 6.5, logo_scale_factor = 4){
  temp <- tempfile() #set temporary file location
  
  ggsave(filename = temp, device = "png", plot = plot_name, units = "in",
         width = width_in, #set desired plot width
         height = height_in) #set desired plot height  
  
  plot_with_logo <- add_logo(
    plot_path = temp, # url or local file for the plot
    logo_path = "https://thebreakthrough.imgix.net/Breakthrough_Institute_Logo_Medium.png", # url or local file for the logo
    logo_position = "bottom right", # choose a corner: 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = logo_scale_factor #as default. Can enter bigger number to make smaller, or smaller number to make logo bigger.
  )
  
  plot_with_logo 
  
  magick::image_write(plot_with_logo, file_name)
}


# Example plot with facets  ------------------------------------------------
p1 <- 
  mtcars %>% group_by(cyl, am) %>% summarize(avg_mpg = mean(mpg)) %>% 
  mutate(cyl = as.factor(cyl)) %>% 
  ggplot(aes(x = cyl, y = avg_mpg, fill = cyl)) + 
  geom_col(position = "dodge") + #this is a column plot with columns colored by scenario
  facet_wrap(vars(am), ncol = 2, scales = "free_y") + #create separate plots for each value of a particular variable
  labs(title = "Example: MPG by Gear and AM", x = element_blank(), y = "Miles Per Gallon (mpg)") +
  scale_fill_manual(values = bti_colors, name = "Gear") + #name should be the variable used for grouping / coloring
  theme(text =element_text(family = "Futura", size = 14),
        plot.title = element_text(color = "black",  size = 24),
        strip.text.x = element_text(size = 14),
        #axis lines
        # axis.line.x = element_line(color="black", size = .25),
        # axis.line.y = element_line(color="black", size = .25),
        #gridlines
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        #background and border
        panel.border = element_blank(), #element_rect(color = "black", size = 1)
        panel.background = element_rect(fill = "white", color = "black", size = 1),  #  #used for border around plot. set to black for faceted plots. white for others.
        #legend
        legend.position = "bottom", 
        legend.key = element_rect(fill = "#FFFFFF00"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        #axes
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank(), #element_blank removes x axis labels. can set font size with element_text(size = 16),
        axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank()#remove x axis labels
        #logo 
        ,plot.margin = unit(c(0.5, 0.5, 2, 0.5), "lines") #add margin for adding logo
        ) + 
  guides(fill = guide_legend(nrow=1, byrow=TRUE)) #set number of rows for legend to ensure it doesnt go off the sides of plot

p1


save_plot_with_logo(plot_name = p1, file_name = "plot_with_logo.png")
