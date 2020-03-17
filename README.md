# globtemp

Plot global temperature anomalies for desired reference period

![test](/NASA_09032020.png)


## Functions:
* anom2anom.R: gereral function for changing the reference period for timeseries of anomalies
* distrCol.R: general function for distribution of colors blue/red/black for anomalies under/over/equal to desired reference period
* readCopernicus.R, readHadCRUT.R, and readNASA.R: functions for reading data from Copernicus/HadCRUT/NASA tables respectively, and giving out data frame for desired values, e.g. yearly data or only January data


## Data: 
* [NASA/GISS](https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv)
* [Copernicus](https://climate.copernicus.eu/sites/default/files/2020-02/ts_12month_anomaly_Global_ea_2t_202001_v01.csv)
* [HadCRUT annual](https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.annual_ns_avg.txt)
* [HadCRUT monthly](https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.monthly_ns_avg.txt)

## User input:
* choose dataset(s) to plot: e.g. datasets <- c('NASA') for only NASA/GISS or datasets <- c('NASA','Copernicus') for comparing NASA/GISS and Copernicus values
* check that most recent (or just desired) files are used e.g. 
filenameNASA <- "data/GLB.Ts+dSST_Jan2020.csv"
filenameCop <-"data/ts_1month_anomaly_Global_ea_2t_202001_v01.csv"
filenameHadYearly <-""
filenameHadMonthly <-""
*
