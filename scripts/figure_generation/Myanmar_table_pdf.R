#Myanmar NASA Project
#Forest Loss, 2001-2016 Table
#David Klinges
#klingesd@si.edu

setwd('Z:/Interns/David/Myanmar/ArcMap_working_folder/Myanmar_townships')

Sys.setenv(JAVA_HOME='C:/Program Files (x86)/Java/jre1.8.0_162')
Sys.getenv("JAVA_HOME")

install.packages('dplyr')
install.packages('tidyr')
install.packages('ggplot2')
install.packages('RColorBrewer')
install.packages('data.table')
install.packages('doParallel')
install.packages('rJava')
install.packages('ReporteRs')
install.packages('rmarkdown')
install.packages('tinytex')
install.packages('devtools')
devtools::install_version("rmarkdown", version = "1.8", repos = "http://cran.us.r-project.org")

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(data.table)
library(RColorBrewer)
library(doParallel)
library(rJava)
library(ReporteRs)
library(rmarkdown)
library(tinytex)
library(devtools)

#Objective:
#Plot the temporal change of deforestation in each township
#Automate this process, as so many townships
#Independent: year
#Dependent: % of total forest area in 2000 lost
#Title each plot to make township, state, and total forest area in 2000 apparent

############## Modified version of Myanmar_table.R. #########################
#This is an incomplete script that ~attempted~ to generate a pdf.
#See Myanmar_table_Rmarkdown for successful pdf generation.

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

figures <- vector(mode = "list", length = 0)
figures


plotfun <- function(i) {
    #Subset to a particular row
    township <- filter(Myanmar, ID == i)
    
    #Create variables for strings that will be referenced later
    town <- as.character(township$Township)
    region <- as.character(township$Region)
    forest2000 <- as.character((sapply(township$treecover2000, pix_to_km2)))

    #Create new data frame of just two columns, the year, and the amount of
    #forest loss (expressed as a % of total forest in 2000)
    township <- gather(township, "Year", "Loss", 7:22)
    township <- mutate(township, Loss = Loss / treecover2000)
    township <- select(township, Year, Loss)
    
    #Turn into a data frame (is this necessary? who knows)
    township <- data.frame(township)
    
    #Create plot
    plot <- ggplot(township, aes(x = Year, y = Loss, fill = Loss)) +
      geom_col() +
      scale_fill_gradient2(low = '#003300', high = '#990000', mid = '#eae43f', midpoint = lossmean,
      limits = c(0, .011), oob = squish) +
      ylab("Deforested Area as % of Forest Cover in 2000") +
      coord_cartesian(xlim = NULL, ylim = c(0.00, 0.03), expand = TRUE) +
      theme(axis.text.x = element_text(size = 10)) +
      theme(axis.text.y = element_text(size = 14)) +
      theme(axis.title.x = element_text(size = 16, face = 'bold')) +
      theme(axis.title.y = element_text(size = 12, face = 'bold')) + 
      scale_x_continuous(breaks=c(2001, 2006, 2011, 2016)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      ggtitle(paste('Forest Loss in', town, 'of', region)) +
      labs(subtitle = paste('Township Total Forest in 2000 =', forest2000, 'km^2')) +
      theme(plot.title = element_text(size = 20))
    
    
    if ((summarise(township, max(Loss))) > 0.03) {
      plot <- plot + coord_cartesian(xlim = NULL, ylim = 
      c(0.00, as.numeric(summarise(township, max(Loss)))), expand = TRUE)}
    
    #Save plot to list
    return(plot)
}


# Loop tasks over all 286 township rows
for (i in (1:nrow(Myanmar))) {
  plot(plotfun(i))
}

figures



