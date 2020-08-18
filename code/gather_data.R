library(tidyverse)
library(lubridate)


state.abb <- state_pops[,1]
state.name <- state_pops[,5]
tracking_temp <- janitor::clean_names(read_csv("https://covidtracking.com/api/v1/states/daily.csv")) %>%
    mutate(date=lubridate::ymd(date)) %>%
    select(date:negative) %>%
    pivot_longer(positive:negative,names_to="measure",values_to="count")
jhu_temp <- read_csv(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/",
                            "csse_covid_19_time_series/","time_series_covid19_deaths_US.csv")) %>%
    select(-UID, -iso2, -iso3, -code3, -Admin2, -FIPS,
           -Lat, -Long_ , -Country_Region, -Combined_Key, -Population) %>%
    gather(key = "date_str", value = deaths, - Province_State) %>%
    group_by(Province_State, date_str) %>%
    summarize(count = sum(deaths, na.rm = T))  %>%
    ungroup()  %>%
    mutate(date = mdy(date_str), state = state.abb[match(Province_State,state.name)]) %>%
    add_column(measure = "death") %>%
    select(date,state,measure,count,-date_str, -Province_State)
covus <- rbind(tracking_temp,jhu_temp) %>%
    arrange(date)

covtot <- read_csv(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/",
                          "csse_covid_19_time_series/","time_series_covid19_deaths_US.csv")) %>%
    select(-UID, -iso2, -iso3, -code3, -Admin2, -FIPS,
           -Lat, -Long_ , -Country_Region, -Combined_Key, -Population) %>%
    gather(key = "date_str", value = deaths, - Province_State) %>%
    group_by(date_str) %>%
    summarize(count = sum(deaths, na.rm = T))  %>%
    ungroup()  %>%
    mutate(date = mdy(date_str)) %>%
    select(date,count,-date_str) %>%
    arrange(date)

gm_spec <- cols(country_region_code = col_character(),
                country_region = col_character(),
                sub_region_1 = col_character(),
                sub_region_2 = col_character(),
                metro_area = col_character(),
                iso_3166_2_code = col_character(),
                census_fips_code = col_character(),
                date = col_date(),
                retail_and_recreation_percent_change_from_baseline = col_integer(),
                grocery_and_pharmacy_percent_change_from_baseline = col_integer(),
                parks_percent_change_from_baseline = col_integer(),
                transit_stations_percent_change_from_baseline = col_integer(),
                workplaces_percent_change_from_baseline = col_integer(),
                residential_percent_change_from_baseline = col_integer())
url = "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
google_mobility <-   janitor::clean_names(read_csv(url, col_types = gm_spec)) %>%
    rename(iso3166_2 = iso_3166_2_code,
           retail = retail_and_recreation_percent_change_from_baseline,
           grocery = grocery_and_pharmacy_percent_change_from_baseline,
           parks = parks_percent_change_from_baseline,
           transit = transit_stations_percent_change_from_baseline,
           workplaces = workplaces_percent_change_from_baseline,
           residential = residential_percent_change_from_baseline) %>%
    pivot_longer(retail:residential, names_to = "type", values_to = "pct_diff")

json_data <- jsonlite::fromJSON("https://covid19-static.cdn-apple.com/covid19-mobility-data/current/v3/index.json")
url <- paste0("https://covid19-static.cdn-apple.com", json_data$basePath, json_data$regions$`en-us`$csvPath)
apple_mobility <- janitor::clean_names(readr::read_csv(url)) %>%
    pivot_longer(cols = starts_with("x"), names_to = "date", values_to = "index") %>%
    mutate(
        date = stringr::str_remove(date, "x"),
        date = stringr::str_replace_all(date, "_", "-"),
        date = as_date(date)) %>%
    rename(score = index)

save(covus,covtot,file="../data/cov.RData")
save(apple_mobility,file="../data/apple.RData")
save(google_mobility,file="../data/google.RData")
