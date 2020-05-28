# Build a single-file data visualization of participants in the Ingenuity ArtsEd Response Collective, Spring 2020
# https://www.ingenuity-inc.org/home/arc/

# Emily Cibelli
# emily.cibelli@gmail.com / ec@ingenuity-inc.org

# Last updated: 5.28.20

# ......................................................................................

library(tidyverse)
library(lubridate)
library(dbplyr)
library(xlsx)
library(scales)
library(waffle)
library(hrbrthemes)
library(gridExtra)

library(rgdal)
library(ggmap)

wd = "I:/Emily/COVID_arts_sector_conversations/"
setwd(wd)

# plot standards
source("plotStandards.R")

# Community area boundaries, last updated Jan 3, 2017
chitown<-readOGR("chicago_community_area_boundaries",
                 "geo_export_272f8222-06a8-4f1f-a587-7d8438a475b9") 
chitown.df<-fortify(chitown, region="area_numbe")

### Read in participant data -------------------------------------------------------------------

## find most recent data file

# all filenames in directory matching filenamename format:
# processed_attendance_data_anonymous_YYYY-MM-DD.csv
arc_file_list = list.files(paste(wd, "data/", sep = ""), pattern = "processed_attendance_data") 
# extract dates from filenames
arc_file_dates = gsub("processed_attendance_data_anonymous_", "", arc_file_list) # extract dates
arc_file_dates = gsub(".csv", "", arc_file_dates)
# find the most recent date and filename
recent_arc_file = which(arc_file_dates == max(arc_file_dates))
recent_arc_filename = paste("data/", arc_file_list[recent_arc_file], sep = "")

# read in participant data from the most recent filename
cc_df = read.csv(recent_arc_filename, head = T, row.names = 1) # cc = "community conversations"

### Read in demographic data ----------------------------

## anonymized school demographics with codes to match cc_df$Organization

# find the most recent matching file ("school_demos_anonymized_YYYY-MM-DD.csv")
school_demo_file_list = list.files(paste(wd, "data/", sep = ""), pattern = "school_demos") 
# extract dates from filenames
school_demo_file_dates = gsub("school_demos_anonymized_", "", school_demo_file_list) # extract dates
school_demo_file_dates = gsub(".csv", "", school_demo_file_dates)
# find the most recent date and filename
recent_school_demo_file = which(school_demo_file_dates == max(school_demo_file_dates))
recent_school_demo_filename = paste("data/", school_demo_file_list[recent_school_demo_file], sep = "")

demos_arcS = read.csv(recent_school_demo_filename, head = T, row.names = 1) 

# ...............

## census data for organizations located in Chicago community areas

# read in the census data
# source: https://datahub.cmap.illinois.gov/dataset/2010-census-data-summarized-to-chicago-community-areas/resource/b30b47bf-bb0d-46b6-853b-47270fb7f626
census = read.xlsx("data/censusData2010.xlsx", sheetIndex = 1) %>%
  select(GEOGNAME:P0050010) # just relevant columns

colnames(census) = c("community_area", "geo_key", "total_population", "white",
                     "black", "native_american", "asian", "hawaiian",
                     "other_na", "multi_racial", "latino")

# make the loop = loop
census$community_area = as.character(census$community_area)
loop_census_row = which(census$community_area == "The Loop")
census[loop_census_row,]$community_area = "Loop"


### Basic stats - counts of attendees and registrants ---------------------------------

## Who is at the table? 

# Participants can be in one of 4 categories (cc_df$OrgType):
# School - employees of a specific school in Chicago Public Schools
# District - employees of the CPS central office
# Partner - employees/artists working at an arts organization that partners with CPS schools (i.e. those in the artlook database; for more information, see chicago.artlookmap.com)
# Other - community members, funders, students, and others not affiliated with one of the 1st 3 categories

# Ingenuity facilitators are excluded from registration data.

# ...............

# Number of participants registered from each affiliation type
table(cc_df$OrgType)

# Number of attendees (and no-shows) from each affiliation type
xtabs(~OrgType + Attended, data = cc_df)

# Unique individuals registered
length(unique(tolower(cc_df$Email)))
       
# Unique individuals attending
length(unique(cc_df[cc_df$Attended == "Y",]$Email))

# Unique orgs/affiliations registered 
length(unique(cc_df$Organization))

# unique orgs/affiliations attending
length(unique(cc_df$Organization))

### Plot - map: location of attendees' organizations and schools ------------

## What is the geographic distribution of Chicago-based attendees?

# Note that geographic information is only available for Chicago-based CPS schools and arts partners.

# ...............

# filter to attended
cc_attended = filter(cc_df, Attended == "Y") %>%
  select(Email, Organization, OrgType, city, state, latitude, longitude) 

# drop duplicated individuals from multiple attendances
cc_attended$emailDupe = duplicated(cc_attended$Email)
cc_attended = cc_attended %>%
  filter(emailDupe == FALSE)

# ...........

# Get counts of groups (not used)
# We had originally included this as a legend, but decided against it as not all data values would be represented on the map (community members, non-Chicago-based organizations)
cc_attended$map_group = 
  ifelse(cc_attended$OrgType == "School", "from CPS schools",
         ifelse(cc_attended$OrgType == "Partner" & cc_attended$city == "Chicago",
                "from Chicago organizations",
                ifelse(cc_attended$OrgType == "Partner",
                  "from organizations outside Chicago",     
                  ifelse(cc_attended$OrgType == "District", 
                         "CPS district employees",
                         "community members"))))

# put group counts in a df
cc_attended_counts = data.frame(table(cc_attended$map_group))
# create a vector
cc_attended_counts$label = paste(cc_attended_counts$Freq, cc_attended_counts$Var1)

# .................

# Build map image
attendee_map = 
  ggplot() +
  # set up Chicago community area boundaries
  geom_polygon(data=chitown.df, 
             aes(x=long, y=lat, group=group), 
             fill='white', color='black', size = 1.15) + 
  # add points for CPS schools and Chicago-based arts partners
  geom_point(data= cc_attended[!(is.na(cc_attended$longitude)) &
                                 cc_attended$city == "Chicago",],
             alpha = 0.85,
             size = 4.5,
             shape = 21,
             stroke = 1.25,
             aes(longitude,latitude,
                 fill = OrgType)) +
  # one color for schools, one color for partners
  scale_fill_manual(values = c(partner_color, teacher_color)) +
  coord_map() +
  # custom simplified map aesthetics
  fryMapTheme +
  # remove bounding box
  theme(legend.position = "none",
        panel.border = element_blank()) +
  # apply legend as text for custom placement
  annotate("text", label = "    CPS schools with\n    participating teachers",
           color = teacher_color, size = 7, fontface = 2,
           x = -Inf, y = -Inf, hjust = "left", vjust = -3) +
  annotate("text", label = "    Participating Chicago\n    arts organizations",
           color = partner_color, size = 7, fontface = 2,
         x = -Inf, y = -Inf, hjust = "left", vjust = -1.25)

# Save map to disk
png(filename = "figures/map_attendees_chicago.png", res = 300,
    width = 2500*2, height = 1200*2)
attendee_map
dev.off()

### Plot - waffle: job roles of attendees -----------------------------------------

## What roles do participants fill? 

# These categories are somewhat loose, but roughly capture the following:
# CPS Administrator - principals and central office employees
# Executive Director - head of a non-profit/arts organization
# Program Manager - mid-level non-profit/arts management, program directors or officers
# Teacher - CPS teacher
# Teaching Artist - arts educators at non-profits and universities; those providing direct-service arts programming to students
# Community Member - those not falling into the above categories

# .............

# filter to unique attendees
role_df = filter(cc_df, Attended == "Y") %>%
  select("Email", "final_role") %>%
  distinct()

# how many individuals in each?
role_count = table(role_df$final_role)

# define a color gradient for plotting role frequency data in a waffle plot
# this aesthetic was selected to complement the custom colors in progColors, but not to perfectly match them, as those elements are used elsewhere
waffle_grad = c("#7183b2", "#858c9a", "#88a7c2", "#8ac2e9", "#abc2ac", "#dac254")

# ..............

# iron that waffle
waffle_roles = 
  waffle::waffle(role_count, rows = 5,
                                   colors = c(other_comm_color,
                                              district_color,
                                              waffle_grad[c(6,5)],
                                              teacher_color, 
                                              waffle_grad[4])) +
  theme(legend.text=element_text(size=18)) 

# save waffle plot to disk
png(filename = "figures/role_waffle.png", width = 3500, height = 800, res = 300)
waffle_roles
dev.off()

### Plot - bar: disciplines ---------------------------------

## What artistic disciplines are represented?

# This data is somewhat sparse - not all participants identified, and not all participants have an affiliation. But we'll use the data we do have.

# .......

# extract rows that are not NA 
cc_disc = filter(cc_df, !(is.na(cc_df$DisciplinesRepresented))) %>%
  select(Email, DisciplinesRepresented) %>%
  distinct() # restrict to unique individuals

# flag disciplines in each row
cc_disc$music = ifelse(grepl("Music", cc_disc$DisciplinesRepresented), "x", "")
cc_disc$visual = ifelse(grepl("Visual", cc_disc$DisciplinesRepresented), "x", "")
cc_disc$theatre = ifelse(grepl("Theatre", cc_disc$DisciplinesRepresented), "x", "")
cc_disc$dance = ifelse(grepl("Dance", cc_disc$DisciplinesRepresented), "x", "")
cc_disc$media = ifelse(grepl("Media", cc_disc$DisciplinesRepresented), "x", "")
cc_disc$literary = ifelse(grepl("Literary", cc_disc$DisciplinesRepresented), "x", "")
cc_disc$multi_disc = ifelse(grepl("Multi", cc_disc$DisciplinesRepresented), "x",
                                 ifelse(grepl(";", cc_disc$DisciplinesRepresented), "x", ""))

# Note that categories (including multi_disc) are not mutually exclusive

# rotate to long to enable easy tallying
cc_disc_long = cc_disc %>%
  select(-Email, -DisciplinesRepresented) %>%
  pivot_longer(everything(), names_to = "discipline", values_to = "values") %>%
  filter(values == "x")

# tally up
cc_disc_table = table(cc_disc_long$discipline)

# apply nice plotting names
names(cc_disc_table) = c("Dance", "Literary Arts", "Media Arts", "Multi-Disciplinary", 
          "Music", "Theatre", "Visual Arts")
cc_disc_table = as.data.frame(cc_disc_table) %>%
  setNames(c("disc", "count"))


# .............................

# Create a gradient
disc_bar_grad = c("#add363", "#68a78d", "#5a9e95", "#4d969d",
                  "#3888aa", "#3385ad", "#1b75bc") 

# find middle point between top and bottom of y-axis
max_disc_yaxis = max(cc_disc_table$count) + 0.2*(max(cc_disc_table$count))
middle_disc_yaxis = max_disc_yaxis/2

# barplot
discipline_bar = 
  ggplot(cc_disc_table) +
  geom_bar(aes(x = disc, y = count, fill = disc), stat = "identity") +
  sota_theme_bar +
  ylim(0, max_disc_yaxis) +
  geom_text(aes(x = disc, y = count, label = count, color = disc),
            size = 10, vjust = -0.5)+
  scale_fill_manual(values = disc_bar_grad) +
  scale_color_manual(values = disc_bar_grad) +
  # The lines below extend the x-axis to allow text ouside of the data range
  # This approach was superceded by an annotation panel (object #7 in grid.arrange)
  # because while this approach worked well with geom_bar, it was trickier with waffle.
  # geom_text(x = -0.3,
  #           y = middle_disc_yaxis,
  #           label = "Which\nartistic\ndisciplines\nare\nrepresented?",
  #           hjust = "center",
  #           size = 10, color = "grey30") +
  # coord_cartesian(xlim = c(-.5, 7), # This focuses the x-axis on the range of interest
  #                 clip = 'off') +   # This keeps the labels from disappearing
  # theme(plot.margin = unit(c(1,3,1,1), "lines"),
  #       axis.line = element_blank(),
  #       axis.title = element_blank())
  theme(axis.title = element_blank())

# save to disk
png(filename = "figures/discipline_bar.png", res = 300, width = 4000, height = 1500)
discipline_bar
dev.off()

# ............

# An alternative version of this plot, which has a big label on the side as a caption for the viewer.
# I opted not to stick with this strategy because while it worked well for geom_bar, it was harder to implement with the waffle plot, and getting the two to match in the final gridded image was proving difficult. So this approach was superceded by an annotation panel (object #7 in grid.arrange).

discipline_bar_alt = 
  ggplot(cc_disc_table) +
  geom_bar(aes(x = disc, y = count, fill = disc), stat = "identity") +
  sota_theme_bar +
  ylim(0, max_disc_yaxis) +
  geom_text(aes(x = disc, y = count, label = count, color = disc),
            size = 10, vjust = -0.5)+
  scale_fill_manual(values = disc_bar_grad) +
  scale_color_manual(values = disc_bar_grad) +
  # The lines below extend the x-axis to allow text ouside of the data range
  geom_text(x = -0.3,
            y = middle_disc_yaxis,
            label = "Which\nartistic\ndisciplines\nare\nrepresented?",
            hjust = "center",
            size = 10, color = "grey30") +
  coord_cartesian(xlim = c(-.5, 7), # This focuses the x-axis on the range of interest
                  clip = 'off') +   # This keeps the labels from disappearing
  theme(plot.margin = unit(c(1,3,1,1), "lines"),
        axis.line = element_blank(),
        axis.title = element_blank())

### Plot - pie: School demographics -----------------------------------------------------

## What are the demographics of schools taught at by participating teachers?

# To get percentages, first get total enrollment at each school = sum of all racial categories
# (first exclude other demos, as individuals in race categories will overlap with these categories)
school_enroll_total = demos_arcS %>%
  filter(!(demo %in% c("limited_english", "reduced_lunch", 
                       "special_education"))) %>%
  group_by(Organization) %>%
  dplyr::summarize(school_enrollment = sum(enrollment))

# total school enrollment across these groups
arc_school_enroll_total = sum(school_enroll_total$school_enrollment)

# sum of enrollment within each racial demographic category
school_demo_totals = demos_arcS %>%
  filter(!(demo %in% c("limited_english", "reduced_lunch", 
                       "special_education"))) %>%
  group_by(demo) %>%
  dplyr::summarize(demo_total = sum(enrollment))

# percentage in each category - divide by total summed enrollment
school_demo_totals$percent = round(100*(
  school_demo_totals$demo_total/arc_school_enroll_total), 0)

# add formatted category labels
school_demo_totals$demo_label = c("Asian", "African-American", 
                                  "Hawaiian/PI",
                                  "Latino", "Multi-Racial", "Native American",
                                  "Other", "White")

# Paste percentage onto labels so that they'll appear in the legend
# do this rather than labels over the pie to make small categories visible
school_demo_totals$demo_label = paste(
  school_demo_totals$demo_label, " - ", school_demo_totals$percent, "%", sep = "")

# pie chart
school_demo_pie = 
  # build pie chart
  ggplot(school_demo_totals, aes(x="", y=percent, fill=demo_label)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  # apply custom colors
  scale_fill_manual(
    values = c(progColors[c(1,6)], cscColors[2], progColors[c(2,7,4,5,3)])) +
  # custom bare theme with legend
  sota_theme_pie_leg +
  # custom legend title and size
  guides(fill=guide_legend(title="Demographics of\nparticipating CPS schools ")) +
  theme(legend.text=element_text(size=20),
        legend.title = element_text(size = 22))

png(filename = "figures/school_demos_pie.png", res = 300, width = 2000, height = 1000)
school_demo_pie
dev.off()

### Plot - pie: Partner demographics -------------------------------

# For participating organizations in Chicago, what are the demographics of the neighborhood they are based in?

# pull partner data from ARC participants
cc_orgs = filter(cc_df, OrgType == "Partner")

# extract data just for community areas where attending partners are
census_arc_orgs = filter(census, community_area %in% 
                           cc_orgs[cc_orgs$Attended == "Y",]$community_area)
# .................

# total population across these neighborhoods 
arc_org_pop_total = sum(as.numeric(as.character(census_arc_orgs$total_population)))

# rotate relevant data to long format for individual demographics 
census_arc_long = census_arc_orgs %>%
  dplyr::select(white:latino) %>%
  pivot_longer(everything(), names_to = "demo", values_to = "population")

# sum of demographics 
census_demo_totals = census_arc_long %>%
  group_by(demo) %>%
  summarize(demo_total = sum(as.numeric(as.character(population))))

# percentage in each category - divide by total summed enrollment
census_demo_totals$percent = round(100*(
  census_demo_totals$demo_total/arc_org_pop_total), 0)

# add formatted category labels (these will match school demos, which also use census categories)
census_demo_totals$demo_label = c("Asian", "African-American", 
                                  "Hawaiian/PI",
                                  "Latino", "Multi-Racial", "Native American",
                                  "Other", "White")

# Paste percentage onto labels so that they'll appear in the legend
census_demo_totals$demo_label = paste(
  census_demo_totals$demo_label, " - ", census_demo_totals$percent, "%", sep = "")

# pie chart
neighborhood_demo_pie =
  # bake the pie
  ggplot(census_demo_totals, aes(x="", y=percent, fill=demo_label))+
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  # apply colors
  scale_fill_manual(
    values = c(progColors[c(1,6)], cscColors[2], progColors[c(2,7,4,5,3)])) +
  # custom blank plot format with legend
  sota_theme_pie_leg +
  # label
  guides(fill=guide_legend(
    title="Neighborhood demographics\nof Chicago participants")) +
  theme(legend.text=element_text(size=20),
        legend.title = element_text(size = 22))

png(filename = "figures/neighborhood_demos_pie.png",
    res = 300, width = 2000, height = 1000)
neighborhood_demo_pie
dev.off()

### Annotated text block of attendance -----------------------

# For the top panel of the plot, report on attendees in various categories and build blocks of text.

# How many unique events
event_count = length(unique(cc_df$SessionDateTime))

# attendee counts in each group
district_att_count = length(unique(
  cc_attended[cc_attended$OrgType == "District",]$Email))
school_att_count = length(unique(
  cc_attended[cc_attended$OrgType == "School",]$Email))
partner_att_count = length(unique(
  cc_attended[cc_attended$OrgType == "Partner",]$Email))
other_att_count = length(unique(
  cc_attended[cc_attended$OrgType == "Other",]$Email))

# how many unique registrants?
unique_registrants = length(unique(tolower(cc_df$Email)))

# build the graphic
event_attendance_graphic = 
  ggplot() + 
  # pattern: big text for number, smaller text for category 
  # arts partners
  annotate("text", label = partner_att_count,
           color = partner_color, size = 24, fontface = 2,
           x = 0, y = 5, hjust = "left", vjust = "center")  +
  annotate("text", label = " attendees from\n arts partners",
           color = partner_color, size = 10, fontface = 2,
           x = 80, y = 5, hjust = "left", vjust = "center")  +
  # schools
  annotate("text", label = school_att_count,
           color = teacher_color, size = 24, fontface = 2,
           x = 330, y = 5, hjust = "left", vjust = "center")  +
  annotate("text", label = "attendees from\nCPS schools",
           color = teacher_color, size = 10, fontface = 2,
           x = 400, y = 5, hjust = "left", vjust = "center")  +
  # district employees
  annotate("text", label = district_att_count,
           color = district_color, size = 24, fontface = 2,
           x = 630, y = 5, hjust = "left", vjust = "center")  +
  annotate("text", label = "CPS district\nemployees",
           color = district_color, size = 10, fontface = 2,
           x = 700, y = 5, hjust = "left", vjust = "center")  +
  # community members
  annotate("text", label = other_att_count,
           color = other_comm_color, size = 24, fontface = 2,
           x = 930, y = 5, hjust = "left", vjust = "center")  +
  annotate("text", label = "community\nmembers",
           color = other_comm_color, size = 10, fontface = 2,
           x = 1000, y = 5, hjust = "left", vjust = "center")  +
  # event count below all of these numbers
  annotate("text", label = paste("at ", event_count, 
                                " ArtsEd Response Collective conversations since April 16, 2020", 
                                sep = ""),
           color = "black", size = 14, fontface = 2,
           x = 600, y = 4.6, hjust = "center", vjust = "top")  +
  # registration data
  annotate("text", label = paste("Note:", unique_registrants, 
"individuals have registered for events. All statistics reflect unique attendees; some attendees have joined multiple conversations."),
           color = "black", size = 6,
           x = 600, y = 4.4, hjust = "center", vjust = "top")  +
  sota_theme_pie +
  xlim(0, 1200) +
  ylim(4.25, 5.25) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 0.8,
        linetype = "dashed"))

# save to disk
png(filename = "figures/event_attendance_graphic.png", res = 300, width = 4800, height = 800)
event_attendance_graphic
dev.off()

### Annotations for disc bar and role waffle --------------------

# To provide context for the discipline bar chart and waffle, create annotations to sit in the image grid (to the left of each of these two).

# disciplines annotation
disc_bar_annotation = 
  ggplot() + 
  annotate("text", 
           label = "Which artistic\ndisciplines are\nrepresented?",
           size = 12, fontface = 2, color = "grey30",
           x = 0, y = 5, hjust = "center", vjust = "center")  +
  xlim(-5, 5) +
  sota_theme_pie

# job role annotation
role_waffle_annotation = 
  ggplot() + 
  annotate("text", 
           label = "Who's in\nthe room?",
           size = 12, fontface = 2, color = "grey30",
           x = 0, y = 5, hjust = "center", vjust = "center")  +
  xlim(-5, 5) +
  sota_theme_pie

### Fold it all up into one graphic ----------------------------

# This section defines a grid layout for all of the above graphics. 

# goal: one long strip across the top for the attendance stats (#1)
#       map on right 3rd of graphic below top strip (#3)
#       leftward 2/3rd below strip divided into 3 rows
#       first 2 rows have disciplines and roles (#2, #4)
#       last row divided between school + partner demo pies (#5, #6)
#       left edge (#7/8) contains labels for barplot and waffle

# height-to-width ratio is 4x6

# define layout
graphic_layout <- rbind(c(1,1,1,1,1,1),
                        c(7,2,2,2,3,3),
                        c(8,4,4,4,3,3),
                        c(5,5,6,6,3,3))

# bundle all images together             
all_together_now =  grid.arrange(event_attendance_graphic,
                                 discipline_bar,
                                 attendee_map,
                                 waffle_roles,
                                 school_demo_pie, neighborhood_demo_pie,
                                 layout_matrix = graphic_layout)
 
# build the filname with today's date
full_graphic_filename = sprintf("ARC_participant_visualization_%s.png",
                                Sys.Date())

# save to disk
png(filename = full_graphic_filename, res = 300,
    width = 7000, height = 4500)
grid.arrange(event_attendance_graphic,
             discipline_bar,
             attendee_map,
             waffle_roles,
             school_demo_pie, neighborhood_demo_pie,
             disc_bar_annotation, role_waffle_annotation,
             layout_matrix = graphic_layout)
dev.off()
