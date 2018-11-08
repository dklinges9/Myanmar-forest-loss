#Myanmar NASA Project
#Forest Loss, 2001-2016 Table
#David Klinges
#klingesd@si.edu

setwd('Z:/Interns/David/Myanmar/ArcMap_working_folder/Myanmar_townships/Township_Tables')
install.packages('dplyr')
install.packages('tidyr')
install.packages('ggplot2')
install.packages('data.table')
install.packages('scales')
library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(scales)

#Objective:
#Plot the temporal change of deforestation in each township
#Independent: year
#Dependent: % of total forest area in 2000 lost
#Automate this process to be conducted for all 286 townships in Myanmar
#Title each plot to make township, state, and total forest area in 2000 apparent

################### IMPORT AND PREP DATA ##############
#Load data: deforestation in Myanmar as determined by Hansen 2016
#Deforestation is given in Landsat pixels (30m x 30m)
Myanmar_table = read.csv("towndeforestation_total.csv")

#Change column names to just year
Myanmar <- setnames(Myanmar_table, old = c(7:22), new = c("2016", "2015", "2014", "2013",
                                                          "2012", "2011", "2010", "2009",
                                                          "2008", "2007", "2006", "2005",
                                                          "2004", "2003", "2002", "2001"))

pix_to_km2 <- function(x) {x * 900 / 1000000} #Convert pixels to km2
km2_to_acre <- function(x) {x * 247.105} #Convert km2 to acres

############### CALC PROPORTIONAL LOSS AND BY TOWNSHIP/REGIONS LOSS ##############

#Determine an appropriate Y-axis scale for deforestation, sort data by amount of loss
loss <- gather(Myanmar, "Year", "Loss", 7:22) #convert columns into rows
loss$Year <- as.integer(loss$Year)
lossper <- mutate(loss, Loss = Loss / treecover2000) #calculate % loss
lossper <- arrange(lossper, Loss) #sort data by amount of loss
lossmean <- mean(lossper$Loss) #Calculate average amount of forest loss in a given year, for all townships


#Add column for percent loss
Myanmar <- mutate(Myanmar, percentloss = totalloss/treecover2000)

#Sort table by Region and then percentloss
Myanmar <- arrange(Myanmar, Region, percentloss)
##note: for some reason, arranging the dataframe as such did not generate figures in the same order. Odd.

#Export edited Myanmar data
write.csv(Myanmar, "towndeforestation_acre.csv")



##Aggregate to by township loss
bytownship <- group_by(lossper, Township, ID) %>%
  summarise(Loss = sum(Loss)) %>%
  arrange(Township)

bytownship$ID <- (1:nrow(bytownship))

#Export by township loss
write.csv(bytownship, "percentlosstownship.csv")

##Aggregate to by region loss
byregion <- group_by(lossper, Region) %>%
  summarise(Total = sum(totalloss), perloss = mean(Loss)) %>%
  arrange(Region)

byregionbyyear <- group_by(lossper, Region, Year) %>%
  summarise(Total = sum(totalloss), perloss = mean(Loss)) %>%
  arrange(Region)


#Export by region loss
write.csv(byregion, "percentlossregion.csv")


##Aggregate to annual loss
byyear <- group_by(lossper, Year) %>%
  summarise(perloss = mean(Loss)) %>%
  arrange(Year)


#Export by annual loss
write.csv(byyear, "percentlossyear.csv")


##################IQR CALCULATIONS#######################

##for loss
boxstats <- boxplot.stats(loss$Loss)
#define outliers (lying outside 1.5 * IQR)
outliers_loss <- boxstats$out
#Exclude outliers
no_out_loss <- filter(loss, Loss < min(outliers_loss))


##for lossper
boxstats <- boxplot.stats(lossper$Loss)
#define outliers (lying outside 1.5 * IQR)
outliers_lossper <- boxstats$out
#Exclude outliers
no_out_lossper <- filter(lossper, Loss < min(outliers_lossper))


##################IDENTIFY OUTLIER TOWNS####################

##for absolute loss
out_towns_loss <- filter(loss, Loss > min(outliers_loss))
out_towns_loss <- group_by(out_towns_loss, Township) %>%
  summarise(Loss = sum(Loss)) %>%
  arrange(Township)

#outlier towns for just year 2013
loss_2013 <- filter(loss, Year == 2013)
boxstats_2013 <- boxplot.stats(loss_2013$Loss)
outliers_loss_2013 <- boxstats_2013$out
out_towns_loss_2013 <- filter(loss_2013, Loss > min(outliers_loss_2013))
out_towns_loss_2013 <- group_by(out_towns_loss_2013, Township) %>%
  summarise(Loss = sum(Loss)) %>%
  arrange(Township)


##for percent loss
out_towns_lossper <- filter(lossper, Loss > min(outliers_lossper))
out_towns_lossper <- group_by(out_towns_lossper, Township) %>%
  summarise(Loss = sum(Loss)) %>%
  arrange(Township)

#outlier towns for just year 2013
lossper_2013 <- filter(lossper, Year == 2013)
boxstats_2013 <- boxplot.stats(lossper_2013$Loss)
outliers_lossper_2013 <- boxstats_2013$out
out_towns_lossper_2013 <- filter(lossper_2013, Loss > min(outliers_lossper_2013))
out_towns_lossper_2013 <- group_by(out_towns_lossper_2013, Township) %>%
  summarise(Loss = sum(Loss)) %>%
  arrange(Township)

################### BUILD PLOT #########################
for (i in (1:nrow(Myanmar))) { #Loop tasks over all 286 township rows
  
  #Subset to a particular row
  township <- filter(Myanmar, ID == i)
  
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
  
  #Create new data frame of just two columns: the year, and the amount of
  #forest loss (expressed as a % of total forest in 2000). This is the data displayed on plots.
  township <- gather(township, "Year", "Loss", 7:22)
  township <- mutate(township, Loss = Loss / treecover2000)
  township <- select(township, Year, Loss)
  
  #Turn into a data frame (is this necessary?  ¯\_(???)_/¯ )
  township <- data.frame(township)
  
  #Convert year into integer so that the x-axis scale can later be manipulated
  township$Year <- as.integer(township$Year)
  
  #Build plot
  plot <- ggplot(loss, aes(x = Year, y = Loss)) +
    geom_point(color = "black") +
    geom_smooth(data = loss, color = "black") +
    #geom_point(data = no_out_loss, color = "blue") +
    #geom_smooth(data = no_out_loss, color = "blue") +
    coord_cartesian(xlim = NULL, ylim = c(0, 40000), expand = FALSE) +
    scale_y_continuous(breaks=seq(10000, 50000, 10000)) +
    scale_x_continuous(breaks=seq(2001, 2016, 1)) +
    ylab("") +
    xlab("") +
    theme(axis.text.x = element_text(size = 16)) +
    #theme(axis.title.x = element_text(size = 18, face = 'bold')) +
    theme(axis.text.y = element_text(size = 16)) +
    #theme(axis.title.y = element_text(size = 12, face = 'bold')) +
    #theme(axis.title.y=element_text(angle = 45, hjust = 1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
  plot
  
  #Save plot to a file named after the township
  setwd('Z:/Interns/David/Myanmar/ArcMap_working_folder/Myanmar_townships/Figures')
  ggsave('absoluteloss.png', width = 12, height = 10)
  
  
  plot <- ggplot(lossper, aes(x = Year, y = Loss)) +
    geom_point(color = "black") +
    geom_smooth(data = lossper, color = "black") +
    coord_cartesian(xlim = NULL, ylim = c(0.00, 0.05), expand = FALSE) +
    scale_y_continuous(breaks=seq(0.01, 0.05, .01)) +
    scale_x_continuous(breaks=seq(2001, 2016, 1)) +
    ylab("") +
    xlab("") +
    theme(axis.text.x = element_text(size = 16)) +
    #theme(axis.title.x = element_text(size = 18, face = 'bold')) +
    theme(axis.text.y = element_text(size = 16)) +
    #theme(axis.title.y = element_text(size = 12, face = 'bold')) +
    #theme(axis.title.y=element_text(angle = 45, hjust = 1)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
  plot  
  
    scale_fill_gradient2(low = '#003300', high = '#990000', mid = '#eae43f',
                         midpoint = lossmean, limits = c(0, .011), oob = squish) +
    ggtitle(paste('Forest Loss,', town, 'in', district, 'of', region)) +
    theme(plot.title = element_text(size = 20))
  labs(subtitle = paste('Township Total Forest in 2000 =', forest2000, 'acres')) +
    labs(subtitle = paste('Total Deforestation in township =', totalloss, 'acres'))
  theme(plot.title = element_text(size = 20))
  
  #The y-axis was set to a limit = 0.03, so that the majority of the data is displayed on a reasonable scale.
  #However, we want to incude all data, including that above 0.03
  if ((summarise(township, max(Loss))) > 0.03) {
    plot <- plot + coord_cartesian(xlim = NULL, ylim = 
                                     c(0.00, as.numeric(summarise(township, max(Loss)))), expand = TRUE)}
  

}


#Profit


########################STATS###############################
cor.test(lossper$Year, lossper$Loss)

cor.test(remove_outliers$Year, remove_outliers$Loss)
