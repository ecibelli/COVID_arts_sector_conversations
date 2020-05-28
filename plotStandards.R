# plot font size standards
baseSizeDef = 12
pieLabSize = 10

# three levels of fonts for other types of plots
bigLabSize = 20
medLabSize = 14
smallLabSize = 10

# default title size
titleSize = 14

# default top-of-bar text size for single facet plots
topTextSingleSize = 9
# default top-of-bar text size for wide multi-facet plots
topTextMultiSize = 6

# define CSC plotting colors
darkblue1    <- rgb(27,117,188,max=255)
lightblue2   <- rgb(138,194,233,max=255)
green3       <- rgb(173,211,99,max=255)
yellow4      <- rgb(255,194,14,max=255)
red5         <- rgb(218,33,40,max=255)

cscColors    <- c(darkblue1,lightblue2,green3,yellow4,red5)

# CSC labels
cscLabels = c("Excelling", "Strong", "Developing", "Emerging", "Incomplete Data")


# Define program type colors (these are just a guess)
greenCol = "#00A18E"
goldCol = "#FFC20E"
navyCol = "#003e7e"
aquaCol = "#008DA7"
greyCol = "#C3C3C3"
lavCol = "#7183B2"
seaCol = "#78C6BE"
progColors = c(greenCol, goldCol, navyCol, aquaCol, greyCol, lavCol, seaCol)

pastYearCol = greenCol
currentYearCol = goldCol


# specify some colors in advance
partner_color = "#007a6c"
teacher_color = "#a3adc6"
district_color = "#88a7c2"
other_comm_color = "#a09f9f"

# Define year vector for longitudinal plots
yrs = c("2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18")
# Colors for full longitudinal set
yrsCols = c(rep(pastYearCol, 5), currentYearCol)

# Define bare-ish theme for bar plots
sota_theme_bar = theme_classic(base_size = bigLabSize) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_blank(),
        legend.position = "none",
		strip.background = element_blank(),
		plot.title = element_text(hjust = 0.5, size = titleSize)) 

# Bar when you want a legend
sota_theme_bar_leg = theme_classic(base_size = bigLabSize) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        axis.line.y = element_blank(),
		strip.background = element_blank(),
		plot.title = element_text(hjust = 0.5, size = titleSize)) 
        #legend.position = "none") 

# bar when you want a legend and y axis
sota_theme_bar_leg_y = theme_classic(base_size = bigLabSize) +
  theme(axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = titleSize)) 

# bar when you want a legend and x axis
sota_theme_bar_leg_x = theme_classic(base_size = bigLabSize) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = titleSize)) 

# bar when you want a legend and both axes
sota_theme_bar_leg_ax = theme_classic(base_size = bigLabSize) +
  theme(#axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.x = element_blank(),
        #axis.line.y = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = titleSize)) 


# bar when you want both axes but no legend
sota_theme_bar_ax = theme_classic(base_size = bigLabSize) +
  theme(legend.position = "none",
    strip.background = element_blank(),
    plot.title = element_text(hjust = 0.5, size = titleSize)) 

# bar when you want just x axis, no legend
sota_theme_bar_x = theme_classic(base_size = bigLabSize) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = titleSize),
        legend.position = "none")

# bar when you want just y axis, no legend
sota_theme_bar_y = theme_classic(base_size = bigLabSize) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = titleSize),
        legend.position = "none")

# Horizontal bare bar
sota_theme_bar_hor = theme_classic(base_size = bigLabSize) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none",
		strip.background = element_blank(),
		plot.title = element_text(hjust = 0.5, size = titleSize)) 


# define really bare theme for pie plots
sota_theme_pie = theme_classic(base_size = bigLabSize+6) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.line = element_blank(),
        legend.position = "none",
		strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = titleSize)) 

# modified pie when you want a legend
sota_theme_pie_leg = theme_classic(base_size = bigLabSize+6) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.line = element_blank(),
        #legend.position = "none",
		strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = titleSize)) 


# Default map theme
fryMapTheme = theme(axis.text= element_blank(),
      axis.ticks=element_blank(),
      axis.title=element_blank(),
      #legend.title=element_blank(),
      panel.background = element_blank(), 
      plot.title = element_text(hjust=.5), 
      plot.subtitle = element_text(hjust=.5),
      panel.grid.major = element_blank(),
	  strip.background = element_blank(),
      legend.background = element_rect(size=0.5, linetype="solid", 
                                       colour ="black"))