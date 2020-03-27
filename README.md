# globtemp
Function for plotting global temperature anomalies from [NASA/GISS](https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv), [Copernicus](https://climate.copernicus.eu/sites/default/files/2020-02/ts_12month_anomaly_Global_ea_2t_202001_v01.csv), and/or [HadCRUT4 annual](https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.annual_ns_avg.txt) / [HadCRUT4 monthly](https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.monthly_ns_avg.txt), relative to desired reference period.

## Prerequisites
R package ggplot2

## Included functions
* readCopernicus.r, readHadCRUT.r, and readNASA.r: functions for reading data from Copernicus/HadCRUT/NASA tables, respectively.
* anom2anom.r: function for changing the reference period for timeseries of anomalies.
* distrCol.r: function for distribution of colors blue/red/black for anomalies under/over/equal to desired reference period.

## User input
* Dataset(s) to plot: e.g. datasets <- c('NASA') for only NASA/GISS or datasets <- c('NASA','Copernicus') for comparing NASA/GISS and Copernicus values
* Start and end of desired reference period: e.g. refs<-1981 and refe<-2010
* Period: e.g. 'Yearly', or 'January', or 'February',...
* Option (TRUE/FALSE) to also plot data relative to original reference period: e.g. orig<-F
* Option (TRUE/FALSE) to save .png

* Datafiles (change? see issue 7)

## Examples in R
```
source("globtemp.r")

globtemp(datasets=c('NASA'),refs=1961,refe=1990,period='Yearly',orig=F,save_option=F,save_name=NA)
```
![test](/example_yearly.png)

```
source("globtemp.r")

globtemp(datasets=c('NASA','Copernicus','HadCRUT'),refs=1981,refe=2010,period='January',orig=F,save_option=T,save_name="example_compare.png")
```
![test](/example_compare.png)
