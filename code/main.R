#======================================================================
# Title: LAX Ground Vehicle Traffic Volume
# Author: Brady Chiu
# Date: July 29, 2016
#======================================================================
## @knitr notes

## @knitr setup
work_dir="/Users/bradychiu/Dropbox/r/lax_ground_vehicle_traffic_volume"
data_dir="data/"
deliverable_dir="deliverables/"
# setwd(work_dir)

package_loader<-function(package_names){
  for(i in package_names){
    if(!i %in% rownames(installed.packages())) install.packages(i, repos="http://cran.r-project.org")
    library(i, character.only=T)
  }
}
package_names<-c("data.table", "dplyr", "forecast", "ggplot2", "knitr", "lubridate", "stringr", "tidyr")
package_loader(package_names)

## @knitr functions
get_data<-function(data_file_paths){
  bind_rows(lapply(data_file_paths, fread, na.strings="", stringsAsFactors=F)) %>%
    data.frame()
}

get_table<-function(df,dig=2){
  kable(df, align='r', digits=dig, format.args=list(big.mark=","))
}

# calculate mean absolute error
get_mae<-function(actuals, predicted){
  err<-abs(actuals-predicted) 
  return (mean(err))
}
# calculate mean absolute percentage error
get_mape<-function(actuals, predicted){
  err<-abs(actuals-predicted)/actuals
  return (mean(err))
}
# calculate mean squared error
get_mse<-function(actuals, predicted){
  err<-(actuals-predicted)^2
  return(mean(err))
}
# calculate mean percentage error
get_mpe<-function(actuals, predicted){
  err<-(actuals-predicted)/actuals
  return(mean(err))
}

## @knitr parameters
validation_periods<-3

## @knitr data
# file.remove(file.path(work_dir, data_dir, "lax_ground_vehicle_traffic_volume.Rds"))
if(!file.exists(file.path(work_dir, data_dir, "lax_ground_vehicle_traffic_volume.Rds"))){
  file_names<-c(
    "Los_Angeles_International_Airport_-_Ground_Vehicle_Traffic_Volume.csv"
  )
  lax_ground_vehicle_traffic_volume<-get_data(file.path(work_dir, data_dir, file_names)) %>%
    mutate(ReportingMonth=mdy_hms(ReportingMonth, tz="America/Los_Angeles")) %>%
    dplyr::rename(reporting_month=ReportingMonth,
                  action=EntryExit,
                  level=UpperLower,
                  vehicles=TOTAL.VEHICLES) %>%
    arrange(reporting_month, action, level)
  saveRDS(lax_ground_vehicle_traffic_volume, file.path(work_dir, data_dir, "lax_ground_vehicle_traffic_volume.Rds"))
}else{
  lax_ground_vehicle_traffic_volume<-readRDS(file.path(work_dir, data_dir, "lax_ground_vehicle_traffic_volume.Rds"))
}

## @knitr data_sanity_check_1
lax_ground_vehicle_traffic_volume %>%
  group_by(reporting_month, action) %>%
  summarize(vehicles=sum(vehicles)) %>%
  ungroup() %>%
  ggplot(aes(x=reporting_month, y=vehicles, color=action, group=action))+
  geom_line(size=1)+
  scale_x_datetime(name="Reporting Month",
                   date_minor_breaks="1 months")+
  scale_y_continuous(name="Vehicles",
                     breaks=seq(0,10000000,500000),
                     labels=format(seq(0,10000000,500000), big.mark=",", scientific=F))+
  scale_color_discrete(name="Action")+
  theme()

## @knitr data_sanity_check_2
lax_ground_vehicle_traffic_volume %>%
  filter(year(reporting_month)==2014,
         month(reporting_month)==4) %>%
  get_table()

## @knitr data_sanity_check_3
lax_ground_vehicle_traffic_volume_edit_1<-lax_ground_vehicle_traffic_volume %>%
  unique()

lax_ground_vehicle_traffic_volume_edit_1 %>%
  filter(year(reporting_month)==2014,
         month(reporting_month)==4) %>%
  get_table()

## @knitr data_sanity_check_4
mean_exit_entry_delta<-lax_ground_vehicle_traffic_volume_edit_1 %>%
  group_by(reporting_month, action) %>%
  summarize(vehicles=sum(vehicles)) %>%
  ungroup() %>%
  spread(action, vehicles) %>%
  mutate(exit_entry_delta=ifelse(Exit>Entry, Exit-Entry, NA)) %>%
  .$exit_entry_delta %>%
  mean(na.rm=T)

lax_ground_vehicle_traffic_volume_edit_2<-lax_ground_vehicle_traffic_volume_edit_1 %>%
  group_by(reporting_month, action) %>%
  summarize(vehicles=sum(vehicles)) %>%
  ungroup() %>%
  spread(action, vehicles) %>%
  mutate(Exit=ifelse(reporting_month==ymd("2014-09-01", tz="America/Los_Angeles"), Entry+mean_exit_entry_delta, Exit)) %>%
  gather(action, vehicles, -reporting_month)

lax_ground_vehicle_traffic_volume_edit_2 %>%
  ggplot(aes(x=reporting_month, y=vehicles, color=action, group=action))+
  geom_line(size=1)+
  scale_x_datetime(name="Reporting Month",
                   date_minor_breaks="1 months")+
  scale_y_continuous(name="Vehicles",
                     breaks=seq(0,10000000,500000),
                     labels=format(seq(0,10000000,500000), big.mark=",", scientific=F))+
  scale_color_discrete(name="Action")

## @knitr data_sanity_check_5
lax_ground_vehicle_traffic_volume_cleaned<-lax_ground_vehicle_traffic_volume_edit_2 %>%
  group_by(reporting_month) %>%
  summarize(vehicles=max(vehicles)) %>%
  ungroup()

lax_ground_vehicle_traffic_volume_cleaned %>%
  ggplot(aes(x=reporting_month, y=vehicles))+
  geom_line(size=1)+
  scale_x_datetime(name="Reporting Month",
                   date_minor_breaks="1 months")+
  scale_y_continuous(name="Vehicles",
                     breaks=seq(0,10000000,500000),
                     labels=format(seq(0,10000000,500000), big.mark=",", scientific=F))

## @knitr data_setup
lax_vehicles<-lax_ground_vehicle_traffic_volume_cleaned %>%
  mutate(dataset=ifelse(row_number()<=nrow(.)-validation_periods, "training", "test"))
lax.training<-lax_vehicles %>%
  filter(dataset=="training") %>%
  dplyr::select(-dataset)
lax.test<-lax_vehicles %>%
  filter(dataset=="test") %>%
  dplyr::select(-dataset)

lax_vehicles %>%
  ggplot(aes(x=reporting_month, y=vehicles, color=dataset))+
  geom_line(size=1)+
  ggtitle("Data Setup")+
  scale_x_datetime(name="Reporting Month")+
  scale_y_continuous(name="Vehicles",
                     breaks=seq(-1000000,1000000,250000),
                     labels=format(seq(-1000000,1000000,250000), big.mark=",", scientific=F))

## @knitr exploring_patterns
lax.training %>%
  mutate(month=factor(format(reporting_month ,"%b"), level=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>%
  ggplot(aes(x=month, y=vehicles, color=month))+
  geom_boxplot()+
  scale_x_discrete(name="Month")+
  scale_y_continuous(name="Vehicles",
                     breaks=seq(0,4000000,200000),
                     labels=format(seq(0,4000000,200000), big.mark=",", scientific=F))+
  theme(legend.position="none")

## @knitr time_series_setup
lax.training %>%
  mutate(diff1=vehicles-dplyr::lag(vehicles, 1)) %>%
  ggplot(aes(x=reporting_month, y=diff1))+
  geom_line(size=1)+
  ggtitle("Month-to-Month Change")+
  scale_x_datetime(name="Reporting Month")+
  scale_y_continuous(name="Vehicles",
                     breaks=seq(-1000000,1000000,250000),
                     labels=format(seq(-1000000,1000000,250000), big.mark=",", scientific=F))

acf(diff(lax.training$vehicles),
    lag.max=sum(!is.na(lax.training$vehicles)))

lax.ts12<-ts(lax.training$vehicles, frequency=12, start=c(year(min(lax.training$reporting_month)),
                                                                   month(min(lax.training$reporting_month))))

## @knitr stl_analysis
lax.stl<-stl(lax.ts12, s.window="periodic", t.window = 24)
plot(lax.stl)

lax.training %>%
  mutate(trend=lax.stl$time.series[,2],
         seasonal_trend=lax.stl$time.series[,1]+lax.stl$time.series[,2]) %>%
  dplyr::rename(actuals=vehicles) %>%
  gather(metric, value, actuals:seasonal_trend) %>%
  mutate(metric=factor(metric, levels=c("trend", "seasonal_trend"))) %>%
  filter(metric!="actuals") %>%
  ggplot(aes(x=reporting_month, y=value, color=metric, group=metric))+
  geom_line(size=1)+
  scale_x_datetime(name="Reporting Month",
                   date_minor_breaks="2 months")+
  scale_y_continuous(name="Vehicles",
                     breaks=seq(0,10000000,500000),
                     labels=format(seq(0,10000000,500000), big.mark=",", scientific=F))

lax.training %>%
  mutate(seasonal_trend=lax.stl$time.series[,1]+lax.stl$time.series[,2]) %>%
  dplyr::rename(actuals=vehicles) %>%
  gather(metric, value, actuals:seasonal_trend) %>%
  ggplot(aes(x=reporting_month, y=value, color=metric, group=metric))+
  geom_line(size=1)+
  scale_x_datetime(name="Reporting Month",
                   date_minor_breaks="2 months")+
  scale_y_continuous(name="Vehicles",
                     breaks=seq(0,10000000,500000),
                     labels=format(seq(0,10000000,500000), big.mark=",", scientific=F))

## @knitr stl_validation
lax.stl.fcast<-data.frame(forecast.stl(lax.stl, h=validation_periods, method="ets"))
lax.stl.predict<-lax.test %>%
  dplyr::rename(actuals=vehicles) %>%
  mutate(predicted=lax.fcast$Point.Forecast)

lax.training %>%
  dplyr::rename(actuals=vehicles) %>%
  gather(metric, value, -reporting_month) %>%
  bind_rows(lax.stl.predict %>%
              gather(metric, value, -reporting_month)) %>%
  ggplot(aes(x=reporting_month, y=value,
             color=factor(metric, levels=c("predicted", "actuals"))))+
  geom_line(size=1)+
  ggtitle("STL Validation")+
  scale_x_datetime(name="Reporting Month",
                   date_minor_breaks="2 months")+
  scale_y_continuous(name="Vehicles",
                     breaks=seq(0,10000000,500000),
                     labels=format(seq(0,10000000,500000), big.mark=",", scientific=F))+
  scale_color_hue(name="")

get_mae(lax.stl.predict$actuals, lax.stl.predict$predicted)
get_mape(lax.stl.predict$actuals, lax.stl.predict$predicted)
get_mse(lax.stl.predict$actuals, lax.stl.predict$predicted)
get_mpe(lax.stl.predict$actuals, lax.stl.predict$predicted)

## @knitr hw_analysis
# lax.hw<-HoltWinters(lax.ts12, gamma=F) # trend only
lax.hw<-HoltWinters(lax.ts12) # seasonal + trend
lax.hw.fcast<-forecast.HoltWinters(lax.hw, h=validation_periods)
plot(lax.hw.fcast)


