#set up
library(tidyverse); library(RCurl); library(grid); library(magick)

#set fonts 
load_font <- function() {
  text_font <<-  "Manuale"
  title_font <<- "Futura"
  print("Note: Download Futura.ttc and Manuale-Regular.ttf fonts first. Default location should be Library/Fonts/ folder. Url: https://www.dropbox.com/work/Style%20Guide/BTI%20Letterhead/Letterhead%20Font")
  require(sysfonts) 
  require(showtext)
  sysfonts::font_add(family="Futura", regular = "~/Library/Fonts/Futura.ttc")
  sysfonts::font_add(family="Manuale", regular = "~/Library/Fonts/manuale-regular.ttf")
}

bti_colors <- c("#0d4459", "#00a990", "#d05527", "#a33332",
                "#b381d0",  "#dfb9a6", "#b2b2b1", "#2A2A2A", "#ecc627")


## IMPORTANT NOTE: Using the FUTURA and Manuale Fonts (or other ones using Showtext package) prevents plots 
## from showing up! You will need to export/save the plots to view them 
## or view them with x11(plot_name), but this graphics views has low resolution. 


# Function for Loading logo
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 4){
  
  # librarys magick R Package https://github.com/ropensci/magick
  
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
    logo_scale = logo_scale_factor # 4 as default. Can enter bigger number to make smaller, or smaller number to make logo bigger.
  )
  
  plot_with_logo 
  
  magick::image_write(plot_with_logo, file_name)
}

#general plot theme
theme_bti <- function (base_size = 14, base_family = "Helvetica") {
  theme_classic() %+replace% 
    theme(plot.title = element_text(color = "black",  size = 24), 
          strip.text.x = element_text(size = 14),
          strip.background = element_blank(),
          
          #gridlines
          panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), #no grid lines
          panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), #no grid lines
          #background and border
          panel.border = element_blank(), #no plot border
          #panel.background = element_rect(fill = "white"), #white panel background 
          #legend
          legend.position = "bottom", #legend default position is bottom
          legend.key = element_rect(fill = "#FFFFFF00", color = NA), #no border around legend items
          legend.text = element_text(size=14),
          legend.title = element_text(size=14),
          #axes
          axis.line.x = element_line(color="black", size = .5),
          axis.line.y = element_line(color="black", size = .5),
          axis.title.y = element_text(size = 16, angle = 90), 
          axis.title.x = element_text(size = 16), 
          #logo 
          plot.margin = unit(c(0.5, 0.5, 2, 0.5), "lines") # add space for logo
    )
}

#theme for adding fonts
add_fonts <- function () {
  theme(plot.title = element_text(family = title_font), 
        text = element_text(family =text_font))
}
          