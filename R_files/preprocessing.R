source("R_files/convert_excel_to_r.R")

#### Libraries ####
library(tidyverse)
library(aTSA)
library(ggplot2)

#### population projection ####
 # transform to long 
    ECON_pop_long <- pivot_longer(
        ECON_pop
        , cols = c(2:5)
        , names_to = "Region"
        , values_to = "Population"
    )

 # check population trends 
    ECON_pop_long %>% 
        ggplot(aes(x = Year, y = Population)) +
        geom_point() +
        geom_smooth(method = "lm", se = F, colour = "grey", size = 0.5 ) +
        facet_grid(Region~.,scales="free") +
        theme_bw()
        
 # ACF plots    
    acf(diff(ECON_pop$`West Rarita`)) # cut-off after 1 lag
    acf(diff(ECON_pop$`East Rarita`)) # cut-off after 1 lag
    acf(diff(ECON_pop$`Central Rarita`)) # cut-off after 1 lag

# PACF plots 
    pacf(diff(ECON_pop$`West Rarita`))
    pacf(diff(ECON_pop$`East Rarita`))
    pacf(diff(ECON_pop$`Central Rarita`))
   # nothing
    
 # augmented dickey-fuller test: check if time series is stationery 
    adf.test(diff(ECON_pop$`West Rarita`)) # lag 1 with trend -> p-value = 0.16 -> don't need ARIMA
    adf.test(diff(ECON_pop$`East Rarita`)) # lag 1 with trend -> p-value = 0.99 -> don't need ARIMA
    adf.test(diff(ECON_pop$`Central Rarita`)) # lag 1 with trend -> p-value = 0.01 -> ARIMA(0,1,1)
   
 # fit models - TBC
    # west and east =  linear regression
    
    # central = ARIMA(0,1,1) doesn't work yet
    pop_central <- ts(ECON_pop$`Central Rarita`)
    fit_pop_central <- arima(pop_central, c(0, 1, 1))
    pred <- predict(fit_pop_central, n.ahead = 10)
    ts.plot(pop_central,2.718^pred$pred, log = "y", lty = c(1,3))
    
    