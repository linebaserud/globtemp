# globtemp
Function for plotting updated monthly or yearly global temperature anomalies from [NASA/GISS](https://data.giss.nasa.gov/gistemp), [Copernicus](https://climate.copernicus.eu/climate-bulletins), and/or [HadCRUT4](https://www.metoffice.gov.uk/hadobs/hadcrut4) relative to desired reference period.

## Prerequisites
R packages ggplot2 and lubridate (included in tidyverse)

## Functions
* readCopernicus.r, readHadCRUT.r, and readNASA.r: functions for reading the most resent data from Copernicus/HadCRUT/NASA, respectively.
* anom2anom.r: function for changing the reference period for timeseries of anomalies.
* distr_col.r: function for distribution of colors blue/red/black for anomalies under/over/equal to desired reference period.

## User input
Dataset(s) to plot, start and end of desired reference period, period of the year (e.g. 'Yearly', or 'January', or 'February',...), and option (TRUE/FALSE) to save plot as .png

## Examples R
```
globtemp(datasets=c('NASA'),refs=1961,refe=1990,period='Yearly',save_option=F,save_name=NA)
```
![test](figs/example_yearly.png)

```
globtemp(datasets=c('NASA','Copernicus','HadCRUT'),refs=1981,refe=2010,period='September',save_option=T,save_name="example.png")
```
![test](figs/example_sept2.png)

## Contact

Email: Line Båserud (lineb@met.no)
