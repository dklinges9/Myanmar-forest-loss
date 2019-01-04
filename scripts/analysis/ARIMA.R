# ARIMA analysis
# This script preps data for, and conducts, ARIMA time series
#   analyses on forest patch area loss

## Workspace prep ###################
library(datasets)
library(stats)
library(zoo)
library(tidyverse)

data("AirPassengers")

country_loss <- read.csv("./data/outputs/Myanmar_TownshipForestLossFragmentation.csv")

## Data curation #################
# Select down to just total area of patches lost
country_loss <- country_loss %>%
  select(Township, Year, Total.Area.of.Loss.Patches) %>%
  rename(patch_loss_area = `Total.Area.of.Loss.Patches`)

# Remove NAs
country_loss <- na.omit(country_loss)

# Total loss for country across 17 years
country_loss_avg <- country_loss %>%
  group_by(Year) %>%
  summarize(avg = mean(patch_loss_area))

country_loss_sum <- country_loss %>%
  group_by(Year) %>%
  summarize(sum = sum(patch_loss_area))

# Create a list of township names
town_names <- unique(country_loss$Township)

# Create a list of datasets, one for each town
towns <- list()

for (i in 1:length(town_names)) {
  out <- country_loss %>%
    filter(Township == as.character(town_names[[i]])) %>%
    select(patch_loss_area)
  towns[[i]] <- ts(out)
}

## Prep and conduct ARIMA ###############

# Turneach dataset into a time series
timeSeries <- as.ts(timeSeries, start = 2001, )


  # Set start and end dates
  start <- timeSeries$Year[1] # year 1 = 2001
  # end <- max(timeSeries$Year)
  end <- max(((max(timeSeries$Year) - start)), 5) * 12
        
  end <-   max(((1958 - start)), 5) * 12
  
      
        data <- towns[[1]]
        
        modelFit <- arima(data, order = c(1, 0, 0), 
                      seasonal = list(order = c(2, 1, 0), period = NA),
                      method= "ML")
        
        fit <- predict(modelFit, n.ahead = 17)
        
        
        list(fit = fit, data = data)
        
    
        
        
## Sample ARIMA shiny app NOT MY CODE ###################

    output$output_text1 <- renderText(
      {
        start <- input$year[1]
        end  <-  start + max(((input$year[2] - start)), 5)
        
        toDate <- end + (as.integer(input$interval) / 12)
        
        text <- paste("Airline Passengers Trend Forecast From Year [", end, " - ", toDate,
                      "], Based On Observed Airline Traffic From Year [", start, " - ", end, "]" ,  sep = "") 
        
        text
      })
    
    output$output_text2 <- renderText(
      {
        start <- input$year[1]
        end  <-  start + max(((input$year[2] - start)), 5)
        
        toDate <- end + (as.integer(input$interval) / 12)
        
        text <- paste("Airline Passengers Forecast From Year [", end, " - ", toDate,
                      "], Based On Observed Airline Traffic From Year [", start, " - ", end, "]" ,  sep = "") 
        
        text
      })
    
    output$prediction <- renderDataTable(
      {
        prediction <- model()
        
        data <- data.frame(Year = as.Date(as.yearmon(time(prediction$fit$pred))),
                           Passengers = as.matrix(as.integer(prediction$fit$pred * 1000)))
        
        data
      }, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 12))
    
    output$airlinePlot <-  renderPlot(
      {
        prediction <- model()
        
        fit <- prediction$fit
        
        # error bounds at 95% confidence level
        upperBound <- fit$pred + 2 * fit$se
        lowerBound <- fit$pred - 2 * fit$se
        
        ts.plot(prediction$data, fit$pred, upperBound, lowerBound,
                col = c(1, 2, 4, 4), lty = c(1, 1, 2, 2),
                gpars=list(xlab="Year", ylab="Passengers (in Thousands)"))
        
        legend("topleft", col = c(1, 2, 4), lty = c(1, 1, 2),
               c("Actual (in Thousands)", "Forecast (in Thousands)", "95% Confidence Interval"))
      }
    )

