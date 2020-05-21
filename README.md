Shiny visualization of covid rates in US counties as reported by the NYTimes

Data source: https://github.com/nytimes/covid-19-data

To run server locally:
```
library(shiny)
runApp('https://github.com/hbeale/shiny_nyt_covid_rates_US_counties') # this is a path
```

To publish to shiny.io:

```
rsconnect::setAccountInfo(name='[yourname]',
                          token='[yourtoken]',
                          secrety='[yoursecret]')

rsconnect::deployApp('shiny_nyt_covid_rates_US_counties', forceUpdate = TRUE) # the first arg is a path

```
