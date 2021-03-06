# ARIMA analysis
# This script preps data for, and conducts, ARIMA time series
#   analyses on forest patch area loss

## README ############
# - Code is sectioned according to what data is used and how it is prepped
# - Each section contains data curation, transformation, visualization, etc.
# - ARIMA analyses are held in different sections below


## Workspace and data prep ###################
library(datasets)
library(stats)
library(zoo)
library(tidyverse)
library(drLumi)

allyears_pertown <- read_csv("./data/outputs/Myanmar_TownshipForestLossFragmentation.csv")

# Select down to just total area of patches lost
allyears_pertown <- allyears_pertown %>%
  select(-X1) %>%
  rename(township = Township) %>%
  rename(year = Year) %>%
  rename(number_patches = `Number.of.Loss.Patches`) %>%
  rename(avg_patch_area = Avg.Patch.Size) %>%
  rename(patch_loss_area = `Total.Area.of.Loss.Patches`)

# Remove NAs
allyears_pertown <- na.omit(allyears_pertown)

forest_loss <- read_csv("./data/intermediate/township_level/forest_loss/forest_loss.csv")

## * Per town loss across all years  ###########

# Initial distribution
hist(allyears_pertown$patch_loss_area, breaks = 50)

# Right skew. Need to go down the ladder of transformations
# Square root, θ = 1/2
allyears_pertown$square_root <- (allyears_pertown$patch_loss_area)^(0.5)
hist(allyears_pertown$square_root, breaks = 50)
# Cube root, θ = 1/3
allyears_pertown$cube_root <- (allyears_pertown$patch_loss_area)^(0.3333)
hist(allyears_pertown$cube_root, breaks = 50)
# Log, θ = 0
hist(log(allyears_pertown$patch_loss_area), breaks = 100)
# Reciprocal root, θ = -1/2
allyears_pertown$recip_root <- -1/(allyears_pertown$patch_loss_area)^0.5
hist(allyears_pertown$recip_root, breaks = 50)

# Reciprocal root is too far, now left skewed. Cube root 
#   and log are closest to normal


## * Per town loss sum for all years ################

sumyears_pertown <- allyears_pertown %>%
  select(-year) %>%
  group_by(township) %>%
  summarize_all(mean)

# Only select towns in top 10 percentile of forest loss
sumyears_pertown_10p <- sumyears_pertown %>%
  filter(patch_loss_area >= 40771683.7) # loss of 29th highest township
  
## * Top 10th percentile loss across all years #############

forest_loss_10p <- forest_loss %>%
  select(-region, -district) %>%
  group_by(township) %>%
  summarize_all(sum) %>%
  arrange(desc(percentloss)) %>%
  slice(1:29)
  
allyears_pertown_10p <- allyears_pertown %>%
  filter(township %in% forest_loss_10p$township)


# Summarized across the country
country_loss_avg_10p <- allyears_pertown_10p %>%
  group_by(year) %>%
  summarize(patch_loss_area = mean(patch_loss_area))


# Save characters by creating objects
x <- country_loss_avg_10p$year
y <- country_loss_avg_10p$patch_loss_area

# Plot to check out the time series
plot(x, y, pch=19)

# Get non-linear model
exp_model <- nls(y ~ exp(a + b * (x-2000)), start = list(a = 0, b = 1))

# Check correlation
cor(y,predict(exp_model))

# 0.9557971....pretty good. Let's add the fitted curve to the plot
lines(x, predict(exp_model, list(x = x)))

## * Total loss for country across all years ##########
# Average loss per township
country_loss_avg <- country_loss %>%
  group_by(Year) %>%
  summarize(avg = mean(patch_loss_area))
# Sum of losses in all townships
country_loss_sum <- country_loss %>%
  group_by(Year) %>%
  summarize(sum = sum(patch_loss_area))

plot(country_loss_sum)

# Convert to time series
country_loss_sum <- ts(country_loss_sum, start = 2001,
                          end = 2017, frequency = 5)

country_loss <- ts(country_loss, start = 2001,
                   end = 2017, frequency = 5)

# Decompose country loss into components
decomposed <- decompose(country_loss)
plot(decomposed)

lm(Year ~ sum, country_loss_sum)

## Separated pre and post policy intervention ########

## * Pre and post intervention, country-wide loss #########
countrywide_pre <- allyears_pertown %>%
  filter(year < 2011) %>%
  select(-township) %>%
  group_by(year) %>%
  summarize_all(mean)

countrywide_post <- allyears_pertown %>%
  filter(year >= 2011) %>%
  select(-township) %>%
  group_by(year) %>%
  summarize_all(mean)

# Make time series of patch loss area
countrywide_pre_PLA_ts <- ts(countrywide_pre$patch_loss_area, 
                             start = 2001, 
                             frequency = 1)

# User Holt Winters filtering instead...decompose spits out an error that the
#   data has less than two periods because it doesn't pick up any seasonality.
# This is because the data is annual and there is no seasonlity. As a fix,
#   HoltWinters allows the user to specify gamma = FALSE (means no seasonality)
countrywide_pre_PLA_HW <- HoltWinters(countrywide_pre_PLA_ts, 
                                      beta = FALSE, gamma = FALSE)

# Decompose time series to figure out trends
plot(decompose(countrywide_pre_PLA_HW))

# Still getting errors from not having enough intervals....but there's clearly
#   an upwards trends in the data, so this step isn't that necessary

# Fit a non-linear trend line to the data




## * Pre and post intervention, per-town loss ##########
## ++ Transform per-town pre and post data ##############
# Initial distributions of per town loss
hist(pertown_pre$patch_loss_area, breaks = 50)
hist(pertown_post$patch_loss_area, breaks = 50)

# Right skew. Need to go down the ladder
# Square root, θ = 1/2
pertown_pre$square_root <- (pertown_pre$patch_loss_area)^(0.5)
hist(pertown_pre$square_root, breaks = 50)
pertown_post$square_root <- (pertown_post$patch_loss_area)^(0.5)
hist(pertown_post$square_root, breaks = 50)


# Cube root, θ = 1/3
pertown_pre$cube_root <- (pertown_pre$patch_loss_area)^(0.3333)
hist(pertown_pre$cube_root, breaks = 50)
pertown_post$cube_root <- (pertown_post$patch_loss_area)^(0.3333)
hist(pertown_post$cube_root, breaks = 50)

# Log, θ = 0
hist(log(pertown_pre$patch_loss_area), breaks = 50)
hist(log(pertown_post$patch_loss_area), breaks = 50)

# Initial distributions of avg patch area
hist(pertown_pre$avg_patch_area, breaks = 20)
hist(pertown_post$avg_patch_area, breaks = 20)

# Square root, θ = 1/2
pertown_pre$patch_area_square_root <- (pertown_pre$avg_patch_area)^(0.5)
hist(pertown_pre$patch_area_square_root, breaks = 50)
pertown_post$patch_area_square_root <- (pertown_post$avg_patch_area)^(0.5)
hist(pertown_post$patch_area_square_root, breaks = 50)

## ++ Pre intervention, per-town loss ###########
pertown_pre <- allyears_pertown %>%
  filter(year < 2011)

# Plot trends
pertown_pre_avgs <- pertown_pre %>%
  select(-township) %>%
  group_by(year) %>%
  summarize_all(mean)

# Save characters by creating objects, and include a log transformation
x <- pertown_pre_avgs$year - 2000
y <- pertown_pre_avgs$patch_loss_area
logy <-  log(pertown_pre_avgs$patch_loss_area)
sqrooty <- pertown_pre_avgs$patch_loss_area^(0.5)

# Plot to check out the time series
plot(x, y, pch=19)
fit  <- lm(y~x)
cor(y,predict(fit))
lines(x, predict(fit, list(x = x)))

# Plot for log transformation
plot(x, logy, pch=19)
fit2  <- lm(logy~x)
cor(logy,predict(fit2))
lines(x, predict(fit2, list(x = x)))

# Plot for square root transformation
plot(x, sqrooty, pch=19)
fit2  <- lm(sqrooty~x)
cor(sqrooty,predict(fit2))
lines(x, predict(fit2, list(x = x)))

# # Get non-linear model
# # Use a self-starting exponential curve, which appears to fit well
# exp_model <- nls(y ~ SSexp(x, .1, 2000000),  control = list(maxiter = 500),
#                  trace = TRUE, alg = 'plinear')
# 
# exp_model <- nls(y ~ exp(b * (x-2000)), start = list(b = 1))
# 
# # Check correlation
# cor(y,predict(exp_model))
# lines(x, predict(exp_model, list(x = x)))

## *** Pre intervention, top 10th percentile of loss ##########

pertown_pre_10p <- pertown_pre %>%
  filter(township %in% forest_loss_10p$township)

# ggplot it
plot <- ggplot(pertown_pre_10p, aes(year, patch_loss_area)) +
  geom_jitter()
plot

# Average across townships
pertown_pre_10p_avgs <- pertown_pre_10p %>%
  select(-township) %>%
  group_by(year) %>%
  summarize_all(mean)

# ggplot it
plot <- ggplot(pertown_pre_10p_avgs, aes(year, patch_loss_area)) +
  geom_point()
plot

## ++ Post intervention, per-town loss ############
pertown_post <- allyears_pertown %>%
  filter(year >= 2011)

# Plot trends
pertown_post_avgs <- pertown_post %>%
  select(-township) %>%
  group_by(year) %>%
  summarize_all(mean)

# Save characters by creating objects, and include a log transformation
x <- pertown_post_avgs$year - 2000
y <- pertown_post_avgs$patch_loss_area
logy <-  log(pertown_post_avgs$patch_loss_area)
sqrooty <- pertown_post_avgs$patch_loss_area^(0.5)

# Plot to check out the time series
plot(x, y, pch=19)
fit  <- lm(y~x)
cor(y,predict(fit))
lines(x, predict(fit, list(x = x)))

# Plot for log transformation
plot(x, logy, pch=19)
fit2  <- lm(logy~x)
cor(logy,predict(fit2))
lines(x, predict(fit2, list(x = x)))

# Plot for square root transformation
plot(x, sqrooty, pch=19)
fit2  <- lm(sqrooty~x)
cor(sqrooty,predict(fit2))
lines(x, predict(fit2, list(x = x)))

## *** Post intervention, top 10th percentile of loss ##########

pertown_post_10p <- pertown_post %>%
  filter(township %in% forest_loss_10p$township)

# ggplot it
plot <- ggplot(pertown_post_10p, aes(year, patch_loss_area)) +
  geom_jitter()
plot

# Average across townships
pertown_post_10p_avgs <- pertown_post_10p %>%
  select(-township) %>%
  group_by(year) %>%
  summarize_all(mean)

# ggplot it
plot <- ggplot(pertown_post_10p_avgs, aes(year, patch_loss_area)) +
  geom_point()
plot

x <- pertown_post_10p_avgs$year
y <- pertown_post_10p_avgs$patch_loss_area

a_start <- 100000000
b_start <- log(2)
log_model <- nls(y ~ x-2000, start = list(a = a_start, b = b_start))
# Check correlation
cor(y,predict(log_model))

# 0.9557971....pretty good. Let's add the fitted curve to the plot
lines(x, predict(log_model, list(x = x)))

## * Messing around with per-town time series #############
timeSeries_pre <- as.ts(pertown_pre$patch_loss_area, start = 2001, end = 2010,
                    frequency = 1)

timeSeries_post <- as.ts(pertown_post$patch_loss_area, start = 2011, end = 2017,
                        frequency = 1)

start <- timeSeries_pre$Year[1] # year 1 = 2001
# end <- max(timeSeries$Year)
end <- max(((max(timeSeries_pre$Year) - start)), 5) * 12

end <-   max(((1958 - start)), 5) * 12


## Separate each town ############
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
timeSeries <- as.ts(timeSeries, start = 2001, end = 2017,
                    frequency = 1)


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

