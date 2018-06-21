library(ggplot2)
library(ggthemes)
library(dplyr)
#library(svglite)

# read in data
dat <- read.csv('../../data/processed/CouncilDistrict_RouteStats.csv')

# create a name field with the district id
dat <- dat %>% mutate(district_name = paste0("District", " ", district_id))

# dictate which order to chart each district in the graph
dat$district_name <-factor(dat$district_name, levels=c('District 1', 'District 2', 'District 3', 'District 4', 'District 5', 
                                                       'District 6', 'District 7', 'District 8', 'District 9', 'District 10'))

# calculate max y-axis, min y-axis, and city average for mean performance
max <- (max(dat$mean_performance) + 5)
min <- (min(dat$mean_performance) - 10)
city_mean <- mean(dat$mean_performance)

# define target performance value
target <- 80

# create variables for the colors of selected bar, not select bars, horizontal city average line, and target performance line
selected_color <- '#00c5ff'
not_selected_color <- '#ced6dc'
city_avg_color <- '#323232'
target_color <- '#00eb56'

# for loop to create and save all of the graphics
for (i in seq_along(dat$district_id)) { 
  # create plot for each district in dat
  
  # assign the value of i to a variable
  district <- dat$district_id[i]
  
  # create a field to save whether a district is selected or not for each iteration, this is used for coloring the bars
  dat <- dat %>% mutate(selected = ifelse(district_id == district,"selected", "not_selected"))
  
  # create the plot
  plot <- 
    ggplot(dat, aes(x=district_id, y=mean_performance, fill=selected))+
    geom_bar(width = 0.7, stat="identity",position="identity")+
    theme_wsj()+
    theme(axis.text.x = element_text(angle = 0, hjust = 1),
          legend.position="none",
          panel.background = element_rect(fill = "#ffffff", colour = NA),
          rect = element_rect(fill = "#ffffff", colour = NA)
          )+
    coord_cartesian(ylim = c(min, max))+
    geom_hline(yintercept = city_mean, colour = city_avg_color, size=1)+
    geom_text(aes(1, city_mean, label = 'average', vjust = 1.25))+
    geom_hline(yintercept = target, colour = target_color, size=1)+
    geom_text(aes(1, target, label = 'target', vjust = 1.25))+
    
    # optional chart title
    #labs(title="Philadelphia Tranist Grades", subtitle=paste0("How City Council District ", district, " Stacks Up"))+
    scale_fill_manual(values = c(not_selected_color, selected_color))
  
  # for viewing in the side panel only
  print(plot)
  
  # save each plot as a png. The dimensions can be changed. To save as svg, change the file ending and install the svglite library.
  ggsave(file=paste0("../../deliverables/figures/district_",district,".png"), plot=plot, width=9.25, height=5)
  
}
