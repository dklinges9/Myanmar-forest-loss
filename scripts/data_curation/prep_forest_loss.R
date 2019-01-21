

library(tidyverse)

filenames <- list.files(path = "./data/intermediate/township_level/forest_loss", full.names=TRUE)  
data_list <- lapply(filenames, function(i) {
  read.csv(i, header=TRUE)
})

# I couldn't get the mutate call to work inside of a loop. Wasn't worth my time
#   to keep messing around with it, so I just did the curation manually.
# Shame on me.
i <- 16 # Index in data_list
data_list[[i]] <- data_list[[i]] %>%
  mutate(year = paste0(2000 + i)) %>%
  rename(region = NAME_1) %>%
  rename(district = NAME_2) %>%
  rename(township = NAME_3) %>%
  rename(loss = paste0("loss_20", i))

forest_loss <- bind_rows(data_list)

write_csv(forest_loss, "./data/intermediate/township_level/forest_loss/forest_loss.csv")
