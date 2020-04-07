# shiny_nyt_covid_rates_domestic
 Shiny visualization of covid rates reported by the NYTimes

Data source: https://github.com/nytimes/covid-19-data

To run server locally:
```
library(shiny)
runApp('shiny_nyt_covid_rates_domestic') # this is a path
```

To publish to shiny.io:

```
rsconnect::setAccountInfo(name='[yourname]',
                          token='[yourtoken]',
                          secrety='[yoursecret]')

rsconnect::deployApp('shiny_nyt_covid_rates_domestic', forceUpdate = TRUE) # the first arg is a path

```