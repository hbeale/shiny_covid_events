Shiny visualization of covid rates in US counties as reported by the NYTimes & JHU. This repository lives at https://github.com/hbeale/shiny_covid_events

Data sources: 
https://github.com/nytimes/covid-19-data
https://github.com/CSSEGISandData/COVID-19 via https://github.com/datasets/covid-19

To run server locally:
```
library(shiny)
runApp('https://github.com/hbeale/shiny_covid_events') # this is a path
```

To publish to shiny.io:

```
rsconnect::setAccountInfo(name='[yourname]',
                          token='[yourtoken]',
                          secrety='[yoursecret]')

rsconnect::deployApp('shiny_covid_events', forceUpdate = TRUE) # the first arg is a path

```
