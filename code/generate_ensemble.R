# read in states that have been processed
state_pops = read.csv('../data/state_pops.csv',header=F)
f = list.files('../output/')
state_list = unique(substr(f,17,18))

# loop through states and compile their results in the right format
for(STATE in state_list){
  
  # get state name for writing to file
  STATE_NAME = as.character(unlist(
    state_pops[state_pops[,1]==STATE,2:3][
      which(!is.na(state_pops[state_pops[,1]==STATE,2:3]))][1]))

  # get state populaltion
  pop = state_pops[state_pops[,1]==STATE,4]
  
  # read in first model fit for that state to know how to set up storage
  f = list.files('../output/')
  f = f[substr(f,0,3)=='mod']
  f = f[grep(STATE,f)]
  load(paste('../output/',f[1],sep=''))
  # samples.list = list()
  # npi.mat = matrix(NA,nrow(df),length(f))
  deathPreds.array = array(NA,c(length(f),nrow(deathPreds),ncol(deathPreds)))
  # deviance = numeric()
  
  # read in all model fits for that state and store results
  for(ii in 1:length(f)){
    load(paste('../output/',f[ii],sep=''))
    # samples.list[[ii]] = samples[,1:12]
    # print(median(samples[,14]))
    # npi.mat[,ii] = df$mobility
    deathPreds.array[ii,,] = ifelse(is.na(deathPreds),0,deathPreds)
    # deviance[ii] = mean(-2*samples[,'Lposterior'])
  }
  
  # time frame of analysis
  t = df$doy

  # calculate weights for models  
  # wts = exp(-(deviance-min(deviance))) / sum(exp(-(deviance-min(deviance))))
  
  # take random draws from mean to obtain posterior predictions
  deathPreds.array = array(
    rpois(prod(dim(deathPreds.array)),deathPreds.array),
    dim(deathPreds.array))
  deathPredsCum.array = deathPreds.array
  for(ii in 1:dim(deathPredsCum.array)[1]){
    for(jj in 1:dim(deathPredsCum.array)[3]){
      deathPredsCum.array[ii,,jj] = cumsum(deathPredsCum.array[ii,,jj])
    }
  }
  
  # quantiles that need to be computed
  quantiles = c(
    0.01,0.025,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,
    0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.975,0.99)
  
  # prepare quantiles for daily incidence
  daily.quantiles = as.numeric(apply(deathPreds.array,2,function(x)
    quantile(x,quantiles))[,nrow(df)+(3:44)])
  df.forecast.deathInc.daily.quantile = data.frame(
    forecast_date = max(df$date)+2,
    target = rep(paste(1:42,' day ahead inc death',sep=''),each=length(quantiles)),
    location = NA,
    location_name = STATE_NAME,
    target_end_date = rep(max(df$date)+(3:44),each=length(quantiles)),
    type = 'quantile',
    quantile = quantiles,
    value = daily.quantiles)
  
  # prepare point estimates for daily incidence
  daily.points = as.numeric(apply(deathPreds.array,2,mean)[nrow(df)+(3:44)])
  df.forecast.deathInc.daily.point = data.frame(
    forecast_date = max(df$date)+2,
    target = paste(1:42,' day ahead inc death',sep=''),
    location = NA,
    location_name = STATE_NAME,
    target_end_date = max(df$date)+(3:44),
    type = 'point',
    quantile = NA,
    value = daily.points)
  
  # prepare quantiles for daily cumulative
  daily.quantiles = as.numeric(apply(deathPredsCum.array,2,function(x)
    quantile(x,quantiles))[,nrow(df)+(2:43)])
  df.forecast.deathCum.daily.quantile = data.frame(
    forecast_date = max(df$date)+2,
    target = rep(paste(1:42,' day ahead cum death',sep=''),each=length(quantiles)),
    location = NA,
    location_name = STATE_NAME,
    target_end_date = rep(max(df$date)+(3:44),each=length(quantiles)),
    type = 'quantile',
    quantile = quantiles,
    value = daily.quantiles)
  
  # prepare point estimates for daily cumulative
  daily.points = as.numeric(apply(deathPredsCum.array,2,mean)[nrow(df)+(3:44)])
  df.forecast.deathCum.daily.point = data.frame(
    forecast_date = max(df$date)+2,
    target = paste(1:42,' day ahead cum death',sep=''),
    location = NA,
    location_name = STATE_NAME,
    target_end_date = max(df$date)+(3:44),
    type = 'point',
    quantile = NA,
    value = daily.points)
  
  # generate weekly forecast from daily forecast
  deathPreds.array.weekly = apply(deathPreds.array,c(1,3),function(x)
    aggregate(x[nrow(df)+(3:44)],by=list(rep(1:6,each=7)),FUN=sum)[,2])
  deathPredsCum.array.weekly = apply(deathPredsCum.array,c(1,3),function(x)
    aggregate(x[nrow(df)+(3:44)],by=list(rep(1:6,each=7)),FUN=mean)[,2])
  
  # prepare quantiles for weekly incidence
  weekly.quantiles = as.numeric(apply(deathPreds.array.weekly,1,function(x)
    quantile(x,quantiles)))
  df.forecast.deathInc.weekly.quantile = data.frame(
    forecast_date = max(df$date)+2,
    target = rep(paste(1:6,' week ahead inc death',sep=''),each=length(quantiles)),
    location = NA,
    location_name = STATE_NAME,
    target_end_date = rep(max(df$date)+2+7*(1:6),each=length(quantiles)),
    type = 'quantile',
    quantile = quantiles,
    value = weekly.quantiles)
  
  # prepare point estimates for weekly incidence
  weekly.points = as.numeric(apply(deathPreds.array.weekly,1,mean))
  df.forecast.deathInc.weekly.point = data.frame(
    forecast_date = max(df$date)+2,
    target = paste(1:6,' week ahead inc death',sep=''),
    location = NA,
    location_name = STATE_NAME,
    target_end_date = max(df$date)+2+7*(1:6),
    type = 'point',
    quantile = NA,
    value = weekly.points)
  
  # prepare quantiles for weekly cumulative
  weekly.quantiles = as.numeric(apply(deathPredsCum.array.weekly,1,function(x)
    quantile(x,quantiles)))
  df.forecast.deathCum.weekly.quantile = data.frame(
    forecast_date = max(df$date)+2,
    target = rep(paste(1:6,' week ahead cum death',sep=''),each=length(quantiles)),
    location = NA,
    location_name = STATE_NAME,
    target_end_date = rep(max(df$date)+2+7*(1:6),each=length(quantiles)),
    type = 'quantile',
    quantile = quantiles,
    value = weekly.quantiles)
  
  # prepare point estimates for weekly cumulative
  weekly.points = as.numeric(apply(deathPredsCum.array.weekly,1,mean))
  df.forecast.deathCum.weekly.point = data.frame(
    forecast_date = max(df$date)+2,
    target = paste(1:6,' week ahead cum death',sep=''),
    location = NA,
    location_name = STATE_NAME,
    target_end_date = max(df$date)+2+7*(1:6),
    type = 'point',
    quantile = NA,
    value = weekly.points)
  
  # combine all targets into a single data frame for this state
  eval(parse(text=paste('
    df.forecast_',STATE,' = rbind(',
      paste(ls()[substr(ls(),0,12)=='df.forecast.'],collapse=','),')',sep='')))
  
  print(STATE)
}

# combine forecast info across all states
eval(parse(text=paste('
    df.forecast = rbind(',paste(ls()[substr(ls(),0,12)=='df.forecast_'],collapse=','),')',sep='')))

# add FIPS codes for each state
library(tidyverse)
df.forecast = dplyr::select(df.forecast, -location) %>%
  mutate(location_name = as.character(location_name))
fips_codes = read_csv('../data/fips_codes.csv')
fips = fips_codes %>%
  dplyr::select( state_code, state_name) %>%
  rename(location = state_code, location_name = state_name) %>%
  unique()
df.forecast = left_join(df.forecast, fips, by = "location_name") %>%
  dplyr::select(forecast_date, target, location, location_name,
                target_end_date, type, quantile, value)



# save forecast for all states
save(df.forecast,file='../output/forecast_states_20200601.RData')
write_csv(df.forecast,path='../forecasts/2020-06-01-NotreDame-mobility.csv')
