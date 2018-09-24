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

##for absolute loss
boxstats <- boxplot.stats(loss$Loss)
#define outliers (lying outside 1.5 * IQR)
outliers <- boxstats$out
#Exclude outliers
no_out_loss <- filter(loss, Loss < min(outliers))


##for percent loss
boxstats <- boxplot.stats(lossper$Loss)
#define outliers (lying outside 1.5 * IQR)
outliers <- boxstats$out
#Exclude outliers
no_out_lossper <- filter(lossper, Loss < min(outliers))


#############KRUSKAL WALLIS WITH OUTLIERS VS NO OUTLIERS###############


##for absolute loss
#subset data to just 2011-2016
loss_2011 <- filter(loss, Year > 2010)
loss_2011 <- mutate(loss_2011, df = "total")
no_out_loss_2011 <- filter(no_out_loss, Year > 2010)
no_out_loss_2011 <- mutate(no_out_loss_2011, df = "no.out")
kruskalready_loss_2011 <- rbind(loss_2011, no_out_loss_2011)
kruskalready_loss_2011$df <- as.factor(kruskalready_loss_2011$df)
kruskal.test(Loss ~ df, data = kruskalready_loss_2011)

#for data 2001-2016
loss <- mutate(loss, df = "total")
no_out_loss <- mutate(no_out_loss, df = "no.out")
kruskalready_loss <- rbind(loss, no_out_loss)
kruskalready_loss$df <- as.factor(kruskalready_loss$df)
kruskal.test(Loss ~ df, data = kruskalready_loss)


##for percent loss
#subset data to just 2011-2016
lossper_2011 <- filter(lossper, Year > 2010)
lossper_2011 <- mutate(lossper_2011, df = "total")
no_out_lossper_2011 <- filter(no_out_lossper, Year > 2010)
no_out_lossper_2011 <- mutate(no_out_lossper_2011, df = "no.out")
kruskalready_lossper_2011 <- rbind(lossper_2011, no_out_lossper_2011)
kruskalready_lossper_2011$df <- as.factor(kruskalready_lossper_2011$df)
kruskal.test(Loss ~ df, data = kruskalready_lossper_2011)

#for data 2001-2016
lossper <- mutate(lossper, df = "total")
no_out_lossper <- mutate(no_out_lossper, df = "no.out")
kruskalready_lossper <- rbind(lossper, no_out_loss)
kruskalready_lossper$df <- as.factor(kruskalready_lossper$df)
kruskal.test(Loss ~ df, data = kruskalready_lossper)

