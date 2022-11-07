# Objective: check to see if the RF albedo is different based on the different rcmip scenarios 
# Take away: yes the albedo RF are different for each future ssp scenario there does not appear 
# to be a clear relationship between LUC emissions and the albdeo RF 
# Remainign questions: What porition of the total RF budget does this represent? How senstive if Hector to changes in 
# this? Does it matter in the big scheme of things? How would one even test for this? With a pulse or step experiment? 
library(dplyr)
library(magrittr)
library(ggplot2)
library(hector)

system.file("input/tables", package = "hectordata") %>% 
  list.files(pattern = "rcmip", full.names = TRUE) %>% 
  lapply(function(f){
    scn <- gsub(x = basename(f), pattern = "rcmip_|_emiss-constraints_rf.csv", replacement = "")
    data <- read.csv(f, comment.char = ";")
    
    out <- rbind(data.frame(year = data$Date, 
                            value = data$luc_emissions, 
                            variable = LUC_EMISSIONS()), 
                 data.frame(year = data$Date, 
                            value = data$RF_albedo, 
                            variable = RF_ALBEDO()))
    out$scenario <- scn
    return(out)
  }) %>% 
  do.call(what = "rbind") -> 
  out



out %>% 
 # filter(year %in% 2020:2100) %>% 
  filter(grepl(pattern = "ssp", x = scenario)) %>% 
  ggplot(aes(year, value, color = scenario)) + 
  geom_line() + 
  facet_wrap("variable", scales = "free")

  
  
  out %>% 
   filter(year %in% 2020:2100) %>% 
    tidyr::spread(variable, value) %>% 
    filter(grepl(pattern = "ssp", x = scenario)) %>% 
    ggplot(aes(luc_emissions, RF_albedo, color = scenario)) + 
    geom_line()
    NULL





