library(shiny)
library(ggplot2)
library(tidyverse)
library(scales)
library(tidyquant)
library(lubridate)
library(TTR)
library(rlang) # as_string

# Load data

covid_events_by_county <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

covid_events_with_change <- covid_events_by_county %>% 
  group_by(county) %>%
  mutate(change_in_cases = cases - lag(cases),
         change_in_deaths = deaths - lag (deaths), 
         state_abbrev = state.abb[match(state, state.name)],
         county_label = paste0(county, ", ", state_abbrev))

counties <- sort(unique(covid_events_with_change$county_label))

initial_counties_of_interest <- tibble(county_label = c("Santa Cruz, CA",
                                                "San Francisco, CA",
                                                "Santa Clara, CA",
                                                "Los Angeles, CA",
                                                "Marin, CA",
                                                "Montgomery, MD",
                                                "Nantucket, MA",
                                                "New York City, NY"))


# Define UI

ui <- fluidPage(
  
  titlePanel("COVID-19 events from NYTimes dataset"),
  
  sidebarPanel(
    
    sliderInput('start_date', 'Start date', min=min(covid_events_by_county$date), max=max(covid_events_by_county$date), value=as.Date("2020-03-01")),
    
    # selectInput('x', 'X', names(dataset)),
    selectInput('y', 'Y axis - select cases or deaths', c("cases", "deaths")),  
    selectInput('moving_average_1', 'Moving average 1 (in days; red)', 1:30, selected = 5),
    selectInput('moving_average_2', 'Moving average 2 (in days; pink)', 1:30, selected = 3),
    selectInput('counties_of_interest', 'Counties (click and type to add)', counties, selected = initial_counties_of_interest$county_label, multiple = TRUE),
    checkboxInput('f_log_transform_y', 'Use logarithmic Y axis?', value = TRUE),
  ),
  
  mainPanel(
    p("This plot shows the change per day in COVID-19 cases and deaths. The data was collected by the New York Times, and downloaded for this app from https://github.com/nytimes/covid-19-data. Note that the Y axis values are different for each plot."),
    br(),
    p("Notes about the data:"),
    p("When the information is available, the NYT investigators count patients where they are being treated, not necessarily where they live. The original data are the cumulative number of confirmed cases and deaths as reported that day in that county or state; I subtract the previous day's counts from each day to obtain the change per day. For more information and caveats about the original data, see https://github.com/nytimes/covid-19-data."),
    plotOutput('plot')
  )
)

# Define server function
server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    variable_for_x =  "date"   
    if(input$y == "cases") variable_for_y = "change_in_cases"    
    if(input$y == "deaths") variable_for_y = "change_in_deaths"    
    
    county_data_to_plot <- covid_events_with_change %>%
      ungroup %>%
      mutate(y_value = county_data_to_plot %>% pull(variable_for_y)) %>%
      filter(county_label %in% input$counties_of_interest) %>%
      filter(date > input$start_date)  %>%
      group_by(county) %>%
      arrange(date) %>%
      mutate(ma_1 = TTR::SMA(y_value, n=as.numeric(input$moving_average_1)),
             ma_2 = TTR::SMA(y_value, n=as.numeric(input$moving_average_2)))
    
    # plot_title
    plot_title <- paste0("COVID-19 ", input$y, " from ", input$start_date, 
                         " to ", max(county_data_to_plot$date))
    
 
    
    p <- county_data_to_plot %>%
#      ggplot(aes_string(x=variable_for_x, y=input$y)) +
      ggplot(aes(date, y_value)) +
      geom_point() +
      facet_wrap(~county_label, scales="free_y") +
      geom_ma(ma_fun = SMA, n = as.numeric(input$moving_average_1), color = "red") +
      geom_ma(ma_fun = SMA, n = as.numeric(input$moving_average_2), color = "pink") +
      theme_grey(base_size=16) +
      expand_limits(y=0) +
      scale_x_date(date_labels = "%m/%d") +
      ggtitle(plot_title) 
    
    if (input$f_log_transform_y) {
      p <- p + 
        scale_y_log10(paste(input$y, "per day (log10)"), 
                      label=comma) 
    } else {
      p <- p + 
        scale_y_continuous(paste(input$y, "per day"), 
                           label=comma)
    }
    
    print(p)
    
  }, height=700)
  
}


# Create Shiny object
shinyApp(ui = ui, server = server)