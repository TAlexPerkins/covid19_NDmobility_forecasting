library(tidyverse)
library(lubridate)
library(covidcast)
library(gtrendsR)

state_pops = read.csv('../data/state_pops.csv',header=F,  stringsAsFactors = F)
state_pops = state_pops[which(rowSums(is.na(state_pops[,2:3]))<2),]
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

covhosp <- read_csv("https://healthdata.gov/sites/default/files/reported_hospital_utilization_timeseries_20201129_2141.csv") %>%
    select(state, date, previous_day_admission_adult_covid_confirmed, previous_day_admission_pediatric_covid_confirmed) %>%
    mutate(date = date-1) %>%
    add_column(measure = "incident_hospitalizations") %>%
    mutate(count = previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed) %>%
    select(date,state,measure,count)
    

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


wwearing_mask <- suppressMessages(covidcast_signal(data_source = "fb-survey",
                                                   signal = "smoothed_wwearing_mask",
                                                   start_day = "2020-09-08", end_day = "2020-12-08",
                                                   geo_type = "state"))
facemasks.data <- as.tibble(wwearing_mask) %>%
    select(date=time_value,geo=geo_value,value) %>%
    mutate(geo=toupper(geo))
state_pops = read.csv('../data/state_pops.csv',header=F,  stringsAsFactors = F)
state_pops = state_pops[which(rowSums(is.na(state_pops[,2:3]))<2),]
STATES = state_pops[,1]
merge.date <- min(facemasks.data$date)
max.date <- max(facemasks.data$date)
facemasks.data.temp <- facemasks.data[0,]
for (S in STATES) {
    print(S)
    if (S %in% unique(facemasks.data$geo)) {
        if (S=="DC"){
            merge.date = merge.date+1
        }
        merge.value <- facemasks.data %>%
            filter(geo==S & date == merge.date)
        merge.value <- merge.value$value
        fms <- gtrends("face masks",geo=paste0("US-",S),
                       time=paste("2020-01-01",merge.date))$interest_over_time    
        fms <- as.tibble(fms) %>%
            select(date,geo,value=hits) %>%
            mutate(geo = substring(geo,4,5)) %>%
            mutate(date = as.Date(date)) %>%
            mutate(value = as.numeric(str_replace(value,"<1","0"))) %>%
            mutate(value = cumsum(value)) %>%
            mutate(value = value*merge.value/max(value))
        facemasks.data.temp <- rbind(facemasks.data.temp,fms)
    } else {
        max.value <- facemasks.data %>%
            filter(date == max.date)
        max.value <- mean(max.value$value)
        fms <- gtrends("face masks",geo="US",
                       time=paste("2020-01-01",Sys.Date()))$interest_over_time    
        fms <- as.tibble(fms) %>%
            select(date,geo,value=hits) %>%
            mutate(geo = S) %>%
            mutate(date = as.Date(date)) %>%
            mutate(value = cumsum(value)) %>%
            mutate(value = value*max.value/max(value))
        facemasks.data.temp <- rbind(facemasks.data.temp,fms)
    }
}
facemasks <- rbind(facemasks.data,facemasks.data.temp) %>%
    arrange(date,geo)

save(facemasks,file="../data/facemasks.RData")
save(covus,covtot,covhosp,file="../data/cov.RData")
save(apple_mobility,file="../data/apple.RData")
save(google_mobility,file="../data/google.RData")
