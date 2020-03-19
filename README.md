# globtemp

Plot global temperature anomalies relative to desired reference period.

## Prerequisites
globtemp requires the R package ggplot2

## Functions:
* readCopernicus.r, readHadCRUT.r, and readNASA.r: functions for reading data from Copernicus/HadCRUT/NASA tables, respectively.
* anom2anom.R: function for changing the reference period for timeseries of anomalies.
* distrCol.R: function for distribution of colors blue/red/black for anomalies under/over/equal to desired reference period.

## Data: 
* [NASA/GISS](https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv)
* [Copernicus](https://climate.copernicus.eu/sites/default/files/2020-02/ts_12month_anomaly_Global_ea_2t_202001_v01.csv)
* [HadCRUT4 annual](https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.annual_ns_avg.txt)
* [HadCRUT4 monthly](https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.monthly_ns_avg.txt)

## User input:
* Dataset(s) to plot: e.g. datasets <- c('NASA') for only NASA/GISS or datasets <- c('NASA','Copernicus') for comparing NASA/GISS and Copernicus values
* Datafiles: e.g. filenameNASA <- "data/GLB.Ts+dSST_Jan2020.csv"
* Start and end of desired reference period: e.g. 
refs<-1981 and refe<-2010
* Type of data: e.g. 'Yearly', 'Jan', 'Feb',...
* Option (True/False) to also plot data relative to original reference period: e.g. orig<-F

## Example:
![test](/NASA_09032020.png)
