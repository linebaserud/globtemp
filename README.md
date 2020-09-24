# globtemp
Function for plotting updated monthly or yearly global temperature anomalies from [NASA/GISS](https://data.giss.nasa.gov/gistemp), [Copernicus](https://climate.copernicus.eu/climate-bulletins), and/or [HadCRUT4](https://www.metoffice.gov.uk/hadobs/hadcrut4) relative to desired reference period.

## Prerequisites
R packages ggplot2 and lubridate (included in tidyverse)

## Functions
* readCopernicus.r, readHadCRUT.r, and readNASA.r: functions for reading the most resent data from Copernicus/HadCRUT/NASA, respectively.
* anom2anom.r: function for changing the reference period for timeseries of anomalies.
* distr_col.r: function for distribution of colors blue/red/black for anomalies under/over/equal to desired reference period.

## User input
* Dataset(s) to plot: e.g. datasets <- c('NASA') for only NASA/GISS or datasets <- c('NASA','Copernicus') for comparing NASA/GISS and Copernicus values
* Start and end of desired reference period: e.g. refs<-1981 and refe<-2010
* Period: e.g. 'Yearly', or 'January', or 'February',...
* Option (TRUE/FALSE) to also plot data relative to original reference period: e.g. orig<-F
* Option (TRUE/FALSE) to save .png

* Datafiles (change? see issue 7)

## Examples R
```
globtemp(datasets=c('NASA'),refs=1961,refe=1990,period='Yearly',orig=F,save_option=F,save_name=NA)
```
![test](/example_yearly.png)

```
globtemp(datasets=c('NASA','Copernicus','HadCRUT'),refs=1981,refe=2010,period='January',orig=F,save_option=T,save_name="example_compare.png")
```
![test](/example_compare.png)

## Contact

Email: Line Båserud (lineb@met.no)
