# read in list of states with model fits
state_pops = read.csv('../data/state_pops.csv',header=F)
f = list.files('../output/')
state_list = unique(substr(f,17,18))

# set up 
STATE = state_list[1]
f = list.files('../output/')
f = f[substr(f,0,3)=='mod']
f = f[grep(STATE,f)]
load(paste('../output/',f[1],sep=''))
deathPreds.array = array(0,c(length(f),nrow(deathPreds),ncol(deathPreds)))

for(STATE in state_list){

  pop = state_pops[state_pops[,1]==STATE,4]
  
  f = list.files('../output/')
  f = f[substr(f,0,3)=='mod']
  f = f[grep(STATE,f)]
  load(paste('../output/',f[1],sep=''))
  
  samples.list = list()
  npi.mat = matrix(NA,nrow(df),length(f))
  # deathPreds.array = array(NA,c(length(f),nrow(deathPreds),ncol(deathPreds)))
  deviance = numeric()
  
  for(ii in 1:length(f)){
    load(paste('../output/',f[ii],sep=''))
    # samples.list[[ii]] = samples[,1:12]
    # print(median(samples[,14]))
    # npi.mat[,ii] = df$mobility
    deathPreds.array[ii,,] = 
      deathPreds.array[ii,,] + ifelse(is.na(deathPreds),0,deathPreds)
    # deviance[ii] = mean(-2*samples[,'Lposterior'])
  }
  
}
  
t = df$doy

# wts = exp(-(deviance-min(deviance))) / sum(exp(-(deviance-min(deviance))))

deathPreds.array = array(
  rpois(prod(dim(deathPreds.array)),deathPreds.array),
  dim(deathPreds.array))
deathPredsCum.array = deathPreds.array
for(ii in 1:dim(deathPredsCum.array)[1]){
  for(jj in 1:dim(deathPredsCum.array)[3]){
    deathPredsCum.array[ii,,jj] = cumsum(deathPredsCum.array[ii,,jj])
  }
}

quantiles = c(0.01,0.025,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.975,0.99)

daily.quantiles = as.numeric(apply(deathPreds.array,2,function(x)
  quantile(x,quantiles))[,nrow(df)+(3:44)])
df.forecast.deathInc.daily.quantile = data.frame(
  forecast_date = max(df$date)+2,
  target = rep(paste(1:42,' day ahead inc death',sep=''),each=length(quantiles)),
  location = NA,
  location_name = as.character(state_pops[state_pops[,1]==STATE,2]),
  target_end_date = rep(max(df$date)+(3:44),each=length(quantiles)),
  type = 'quantile',
  quantile = quantiles,
  value = daily.quantiles)

daily.points = as.numeric(apply(deathPreds.array,2,mean)[nrow(df)+(3:44)])
df.forecast.deathInc.daily.point = data.frame(
  forecast_date = max(df$date)+2,
  target = paste(1:42,' day ahead inc death',sep=''),
  location = NA,
  location_name = as.character(state_pops[state_pops[,1]==STATE,2]),
  target_end_date = max(df$date)+(3:44),
  type = 'point',
  quantile = NA,
  value = daily.points)

daily.quantiles = as.numeric(apply(deathPredsCum.array,2,function(x)
  quantile(x,quantiles))[,nrow(df)+(2:43)])
df.forecast.deathCum.daily.quantile = data.frame(
  forecast_date = max(df$date)+2,
  target = rep(paste(1:42,' day ahead cum death',sep=''),each=length(quantiles)),
  location = NA,
  location_name = as.character(state_pops[state_pops[,1]==STATE,2]),
  target_end_date = rep(max(df$date)+(3:44),each=length(quantiles)),
  type = 'quantile',
  quantile = quantiles,
  value = daily.quantiles)

daily.points = as.numeric(apply(deathPredsCum.array,2,mean)[nrow(df)+(3:44)])
df.forecast.deathCum.daily.point = data.frame(
  forecast_date = max(df$date)+2,
  target = paste(1:42,' day ahead cum death',sep=''),
  location = NA,
  location_name = as.character(state_pops[state_pops[,1]==STATE,2]),
  target_end_date = max(df$date)+(3:44),
  type = 'point',
  quantile = NA,
  value = daily.points)



deathPreds.array.weekly = apply(deathPreds.array,c(1,3),function(x)
  aggregate(x[nrow(df)+(3:44)],by=list(rep(1:6,each=7)),FUN=sum)[,2])
deathPredsCum.array.weekly = apply(deathPredsCum.array,c(1,3),function(x)
  aggregate(x[nrow(df)+(3:44)],by=list(rep(1:6,each=7)),FUN=mean)[,2])

weekly.quantiles = as.numeric(apply(deathPreds.array.weekly,1,function(x)
  quantile(x,quantiles)))
df.forecast.deathInc.weekly.quantile = data.frame(
  forecast_date = max(df$date)+2,
  target = rep(paste(1:6,' week ahead inc death',sep=''),each=length(quantiles)),
  location = NA,
  location_name = as.character(state_pops[state_pops[,1]==STATE,2]),
  target_end_date = rep(max(df$date)+2+7*(1:6),each=length(quantiles)),
  type = 'quantile',
  quantile = quantiles,
  value = weekly.quantiles)

weekly.points = as.numeric(apply(deathPreds.array.weekly,1,mean))
df.forecast.deathInc.weekly.point = data.frame(
  forecast_date = max(df$date)+2,
  target = paste(1:6,' week ahead inc death',sep=''),
  location = NA,
  location_name = as.character(state_pops[state_pops[,1]==STATE,2]),
  target_end_date = max(df$date)+2+7*(1:6),
  type = 'point',
  quantile = NA,
  value = weekly.points)

weekly.quantiles = as.numeric(apply(deathPredsCum.array.weekly,1,function(x)
  quantile(x,quantiles)))
df.forecast.deathCum.weekly.quantile = data.frame(
  forecast_date = max(df$date)+2,
  target = rep(paste(1:6,' week ahead cum death',sep=''),each=length(quantiles)),
  location = NA,
  location_name = as.character(state_pops[state_pops[,1]==STATE,2]),
  target_end_date = rep(max(df$date)+2+7*(1:6),each=length(quantiles)),
  type = 'quantile',
  quantile = quantiles,
  value = weekly.quantiles)

weekly.points = as.numeric(apply(deathPredsCum.array.weekly,1,mean))
df.forecast.deathCum.weekly.point = data.frame(
  forecast_date = max(df$date)+2,
  target = paste(1:6,' week ahead cum death',sep=''),
  location = NA,
  location_name = as.character(state_pops[state_pops[,1]==STATE,2]),
  target_end_date = max(df$date)+2+7*(1:6),
  type = 'point',
  quantile = NA,
  value = weekly.points)



eval(parse(text=paste('
  df.forecast_US = rbind(',paste(ls()[substr(ls(),0,12)=='df.forecast.'],collapse=','),')',sep='')))
df.forecast_US$location = 'US'
df.forecast_US$location_name = 'United States'

