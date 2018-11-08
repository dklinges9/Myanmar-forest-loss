#Myanmar NASA Project
#Forest Loss, 2001-2016 Table
#David Klinges
#klingesd@si.edu

setwd('Z:/Interns/David/Myanmar/ArcMap_working_folder/Myanmar_townships')
getwd()
install.packages('dplyr')
install.packages('tidyr')
install.packages('ggplot2')
install.packages('RColorBrewer')
install.packages('data.table')
install.packages('doParallel')
install.packages('scales')
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(data.table)
library(RcolorBrewer)
library(doParallel)
library(scales)
#Objective:
#Plot the temporal change of deforestation in each township
#Automate this process, as so many townships
#Independent: year
#Dependent: % of total forest area in 2000 lost
#Title each plot to make township, state, and total forest area in 2000 apparent

############## Modified version of Myanmar_table.R. #########################
#This script was used to test plot characteristics by generating a single plot.


#Load data: deforestation in Myanmar as determined by Hansen 2016
#Deforestation is given in Landsat pixels (30m x 30m)
Myanmar_table = read.csv("towndeforestation_total.csv")

#Change column names
Myanmar <- setnames(Myanmar_table, old = c(7:22), new = c("2016", "2015", "2014", "2013",
                                                          "2012", "2011", "2010", "2009",
                                                          "2008", "2007", "2006", "2005",
                                                          "2004", "2003", "2002", "2001"))

#Convert pixels to km2
pix_to_km2 <- function(x) {x * 900 / 1000000}
#Myanmar <- mutate(Myanmar, treecover2000_km2 = sapply(select(Myanmar, 23), pix_to_km2))


#In order to determine an appropriate Y-axis scale for deforestation, sort data by amount of loss
loss <- gather(Myanmar, "Year", "Loss", 7:22)
lossper <- mutate(loss, Loss = Loss / treecover2000)
lossper <- arrange(lossper, Loss)

#Calculate average amount of forest loss in a given year, for all townships
lossmean <- mean(lossper$Loss)

#Sort table by Region and then distrt
Myanmar.sorted <- arrange(Myanmar, Region, desc(totalloss))


Ayeyarwady <- filter(Myanmar.sorted, Region == "Ayeyarwady")

# Loop tasks over all 286 township rows
for (i in 27) {

    #Subset to a particular row
    township <- filter(Ayeyarwady, ID == i)
    
    #Create variables for strings that will be referenced later
    town <- as.character(township$Township)
    district <- as.character(township$District)
    region <- as.character(township$Region)
    forest2000 <- formatC(
      as.character((sapply(township$treecover2000, km2_to_acre))),
      format = "e", digits = 2)
    totalloss <- formatC(
      as.character((sapply(township$totalloss, km2_to_acre))),
      format = "e", digits = 2)
    #Create new data frame of just two columns, the year, and the amount of
    #forest loss (expressed as a % of total forest in 2000)
    township <- gather(township, "Year", "Loss", 7:22)
    township <- mutate(township, Loss = Loss / treecover2000)
    township <- select(township, Year, Loss)
    
    #Turn into a data frame (is this necessary? who knows)
    township <- data.frame(township)
    
    township$Year <- as.integer(township$Year)
    
    #Create plot
    plot <- ggplot(township, aes(x = Year, y = Loss, fill = Loss)) +
      geom_col() +
      scale_fill_gradient2(low = '#003300', high = '#990000', mid = '#eae43f',
                           midpoint = lossmean, limits = c(0, .011), oob = squish) +
      ylab("Deforested Area as % of Forest Cover in 2000") +
      coord_cartesian(xlim = NULL, ylim = c(0.00, 0.03), expand = TRUE) +
      theme(axis.text.x = element_text(size = 10)) +
      theme(axis.text.y = element_text(size = 14)) +
      theme(axis.title.x = element_text(size = 16, face = 'bold')) +
      theme(axis.title.y = element_text(size = 12, face = 'bold')) + 
      scale_x_continuous(breaks=seq(2001, 2016, 5 )) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      theme(plot.title = element_text(size = 16)) +
      labs(title = paste('Forest Loss,', town, 'in', district, 'of', region),
           subtitle = paste('Total Deforestation =', totalloss, 'acres;', 'Total Forest in 2000 =', forest2000, 'acres'))
    
    if ((summarise(township, max(Loss))) > 0.03) {
      plot <- plot + coord_cartesian(xlim = NULL, ylim = 
                                       c(0.00, as.numeric(summarise(township, max(Loss)))), expand = TRUE)}
    
    #Save plot
    #ggsave(paste('Tantabin (Shwebo).png'), width = 10, height = 10)
    
}

plot(plot)

Myanmar.sorted <- sort(Myanmar$Region)
