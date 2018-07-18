library(ggplot2)
library(ggthemes)
library(dplyr)
library(scales)
#library(svglite)

# read in data
dat <- read.csv('../../data/processed/CouncilDistrict_RouteStats.csv')

# create a variable with the percentage value as a decimal.
# this is used in place of mean_performance so that the scales function can be used to create % label on the y-axis
dat <- dat %>% mutate(mean_performance_decimal = (mean_performance / 100))

# dictate which order to chart each district and treat as a string rather than integer
dat$district_id <-factor(dat$district_id, levels=c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10'))

# define max y-axis, min y-axis, city mean performance, and target performance value
max <- 0.85
min <- 0.6
target <- 0.8

# create variables for the colors of selected bar, not select bars, horizontal city average performance line, and target performance line
selected_color <- '#4d6379'
not_selected_color <- '#ced6dc'
target_color <- '#00c5ff'

# for loop to create and save all of the graphics
for (i in seq_along(dat$district_id)) { 
  # create plot for each district in dat
  
  # assign the value of i to a variable
  district <- dat$district_id[i]
  
  # create a field to save whether a district is selected or not for each iteration, this is used for coloring the bars
  dat <- dat %>% mutate(selected = ifelse(district_id == district,"selected", "not_selected"))
  
  # create the plot
  plot <- 
    ggplot(dat, aes(x=district_id, y=mean_performance_decimal, fill=selected))+
    geom_bar(width = 0.7, stat="identity",position="identity")+
    theme_wsj()+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.35),
          legend.position="none",
          panel.background = element_rect(fill = "#ffffff", colour = NA),
          rect = element_rect(fill = "#ffffff", colour = NA)
    )+
    coord_cartesian(ylim = c(min, max))+
    geom_hline(yintercept = target, colour = target_color, size=1)+
    geom_text(aes(1, target, label = 'target', vjust = 1.25))+
    # optional chart title
    #labs(title="Philadelphia Tranist Grades", subtitle=paste0("How City Council District ", district, " Stacks Up"))+
    scale_fill_manual(values = c(not_selected_color, selected_color))+
    # convert mean_performance_decimal into a percentage for the label
    scale_y_continuous(labels = scales::percent)
  
  # for viewing in the side panel only
  print(plot)
  
  # save each plot as a png. The dimensions can be changed. To save as svg, change the file ending and install the svglite library.
  ggsave(file=paste0("../../deliverables/figures/district_",district,".png"), plot=plot, width=9.25, height=5)
  
}
