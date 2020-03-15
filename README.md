# covid19

Interactive time series for COVID-19 data. The data are obtained from
[https://github.com/CSSEGISandData/COVID-19/](https://github.com/CSSEGISandData/COVID-19/) and are copyright Johns
Hopkins University. Further information on the ultimate data sources
are available from [the Johns Hopkins repository] (https://github.com/CSSEGISandData/COVID-19/blob/master/README.md).

Countries are only included if there have been more than 100 cases, and
the time axis is set to be zero at the date where 100 cases were reached. If there
is not a day with exactly 100 cases, the date is adjusted on the assumption
of a 33% daily relative increase at that time.