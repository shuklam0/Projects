
# Library for saving data
library(data.table)
library(tidyverse)
# library(COVID19)
# library(countrycode)

# File to save Data
COVID19 = data.table(COVID19::covid19())

# Manipulation in COVID19 data
COVID19[, country := administrative_area_level_1]
COVID19[, continent := countrycode::countrycode(sourcevar = id, origin = "iso3c", destination = "continent")];
COVID19[is.na(continent), continent := "Other"]
# COVID19[, day_count := date - first(date[deaths>0]), by=country];
COVID19[, active := confirmed - (recovered + deaths)]
# COVID19[, flag := lapply(X = COVID19$key_google_mobility, FUN = flag)]

# Update Changes in Table
COVID19[, tests_new := tests - shift(tests), by=id]
COVID19[, confirmed_new := confirmed - shift(confirmed), by=id]
COVID19[, recovered_new := recovered - shift(recovered), by=id]
COVID19[, deaths_new := deaths - shift(deaths), by=id]

# Top 3 Countries by Number of cases each day
D2 = COVID19 %>% group_by(date) %>% top_n(n = 3, wt = confirmed) %>% data.table %>% subset(confirmed>25000)
COVID19.Top3 = c("Brazil", "China", "Germany", "Spain", "India", "Italy", "Russia", "United States")


########### Transform data #############

COVID19.cols = c("id","date", "population", "iso_alpha_3", "iso_alpha_2", "iso_numeric", "currency", "administrative_area_level", "administrative_area_level_1", "administrative_area_level_2", "administrative_area_level_3", "latitude", "longitude", "key", "key_apple_mobility", "key_google_mobility", "country", "continent")
COVID19.long = COVID19 %>% melt(id.vars = COVID19.cols)

# X1 = COVID19 %>% melt(id.vars = c("date", "id",  "population", "country", "continent"))

# X1 = COVID19 %>% melt(id.vars = c("date", "id")) %>% data.table
# X2 = unique(X1[,-"date"]) %>% group_by(id, variable) %>% summarize(L = length(value)) %>% dcast(formula = id ~ variable)
# X3 = apply(X = X2, MARGIN = 2, FUN = unique) %>% lapply(FUN = length) %>% data.frame %>% melt(id.vars="id") %>% data.table
# X4 = X3[value==1,-"id"]
     # reshape(data = X2, direction = "wide",  timevar = id, ids = variable, ) %>% 
# match(COVID19.cols, colnames(COVID19))
