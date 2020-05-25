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

covid_county_events_with_change <- covid_events_by_county %>% 
  filter(county != "Unknown") %>%
  mutate(state_abbrev = state.abb[match(state, state.name)],
         county_label = paste0(county, ", ", state_abbrev)) %>%
  group_by(county_label) %>%
  arrange(date) %>%
  mutate(new_cases = ifelse(cases<lag(cases), 0, cases - lag(cases)),
         new_deaths = ifelse(deaths<lag(deaths), 0, deaths - lag (deaths))) %>%
  ungroup

covid_state_events_with_change <- covid_events_by_county %>% 
  group_by(state, date) %>%
  summarize(cases = sum(cases),
            deaths = sum(deaths)) %>%
  group_by(state) %>%
  arrange(date) %>%
  mutate(new_cases = ifelse(cases<lag(cases), 0, cases - lag(cases)),
         new_deaths = ifelse(deaths<lag(deaths), 0, deaths - lag (deaths))) %>%
  ungroup

counties <- sort(unique(covid_county_events_with_change$county_label))

initial_counties_of_interest <- tibble(county_label = c("Santa Cruz, CA",
                                                "San Francisco, CA",
                                                "Santa Clara, CA",
                                                "Los Angeles, CA",
                                                "Marin, CA",
                                                "Montgomery, MD",
                                                "Nantucket, MA",
                                                "New York City, NY",
                                                "Clear Creek, CO",
                                                "Montrose, CO"
                                                ))

states_in_data <- sort(unique(covid_state_events_with_change$state))

initial_states_of_interest <- c("California",
 "Maryland", "Massachusetts", "New York", "Colorado")



# Define UI

ui <- fluidPage(
  
  titlePanel("COVID-19 events from NYTimes dataset"),
  
  sidebarPanel(
    
    sliderInput('start_date', 'Start date', min=min(covid_events_by_county$date), max=max(covid_events_by_county$date), value=as.Date("2020-03-01")),
    sliderInput('end_date', 'End date', min=min(covid_events_by_county$date), max=max(covid_events_by_county$date), value=max(covid_county_events_with_change$date)),
    # selectInput('x', 'X', names(dataset)),
    selectInput('y', 'Y axis - select cases or deaths', c("cases", "deaths")),  
    selectInput('region_type', 'Region type - select counties or states', c("counties", "states"),
                selected = "counties"),  
    numericInput('moving_average_1', 'Moving average (in days)', 15, 1, 60),
    radioButtons('region_display_option', "Region selection", c("Show biggest movers*", "Show specified regions"), selected = "Show biggest movers*"),
    
    conditionalPanel(
      condition = "input.region_display_option == 'Show biggest movers*'",
      radioButtons('by_percent_or_abs', "", c("Biggest by percent", "Biggest in absolute numbers"), selected = "Biggest in absolute numbers"),
      numericInput('change_range', 'Consider changes in the last X days', 14, min=7, max=60),
      numericInput('min_absolute_changes', 'Minimum absolute change', 20, min=1, max=6000),
      checkboxInput('f_show_increases_only', 'Show_increases_only?', value = TRUE),
      
    ),
    conditionalPanel(
      condition = "input.region_display_option == 'Show specified regions' && input.region_type == 'counties'",
      selectInput('counties_of_interest', 'Counties (click and type to add)', 
                  counties, selected = initial_counties_of_interest$county_label, multiple = TRUE)
    ),
    conditionalPanel(
      condition = "input.region_display_option == 'Show specified regions' && input.region_type == 'states'",
      selectInput('states_of_interest', 'States (click and type to add)', 
                  states_in_data, selected = initial_states_of_interest, multiple = TRUE)
    ),
    
    checkboxInput('f_log_transform_y', 'Use logarithmic Y axis?', value = FALSE),
    checkboxInput('f_consistent_y_axes', 'Use consistent Y axes across counties?', value = FALSE),
    checkboxInput('show_inferred_case_prevalance', 'Show inferred case prevalance?', value = FALSE),
    conditionalPanel(
      condition = "input.show_inferred_case_prevalance",
      numericInput('overall_deaths_per_case', 'Overall deaths per case (for inferred case prevalence; purple)', 0.01, min=0, max=1),
      numericInput('death_lag', 'Time from infection to death for fatalities (for inferred case prevalance; purple")', 17, min=0)),
    h4("Notes about the data:"),
    p("When the information is available, the NYT investigators count patients where they are being treated, not necessarily where they live. The original data are the cumulative number of confirmed cases and deaths as reported that day in that county or state; I subtract the previous day's counts from each day to obtain the change per day. Click", 
      a("here", href = "https://github.com/nytimes/covid-19-data"), 
      "for more information and caveats about the original data."),
    br(),
    h4("*Notes about finding the biggest movers:"),
    p("The counties with the greatest changes are identified by comparing the median values from two different weeks. For example, when viewing cases with the default setting of 14, the median number of cases in the past week is compared to the median number of cases in the 7 day span 14-21 days ago. The purple rectangles highlight the time span from which the median is calculated. The purple bar shows the median value.")
  ),
  mainPanel(
    p("This plot shows the change per day in COVID-19 cases and deaths. The", 
      a("underlying code", href="https://github.com/hbeale/shiny_nyt_covid_rates_US_counties"),
      "gathers",
      a("data", href="https://github.com/nytimes/covid-19-data"),
      "collected by the ",
      a("New York Times.", href="https://www.nytimes.com/"),
      "Note that the Y axis values are different for each plot."),
    p("See explanatory notes below the plot."),
    br(),
    plotOutput('plot')
)
)

# Define server function
server <- function(input, output) {
  
  output$plot <- renderPlot({
    
    if(input$region_type == "counties") {
      region_data <- covid_county_events_with_change %>%
        rename(region = county_label) %>%
        na.omit
    } else {
      region_data <- covid_state_events_with_change %>%
        rename(region = state) %>%
        na.omit
    }
    
    variable_for_x =  "date"   
    if(input$y == "cases") variable_for_y = "new_cases"    
    if(input$y == "deaths") variable_for_y = "new_deaths"
    
    region_data_with_y <- region_data %>%
      mutate(y_value = region_data %>% pull(variable_for_y))
    
    if(input$region_display_option == "Show specified regions") { 
      if (input$region_type == "counties") {
        selected_regions_of_interest <- input$counties_of_interest
      } else {
        selected_regions_of_interest <- input$states_of_interest
      }
    } else {
      window_end <- max(region_data_with_y$date)
      default_window_in_days <- input$change_range
      window_start <- window_end - days(default_window_in_days)
      min_absolute_changes <- input$min_absolute_changes
      
      window_changes <- region_data_with_y %>% 
        mutate(in_first_window = date %in% c(window_start - days(0:6)),
               in_second_window = date %in% c(window_end - days(0:6))
        ) %>%
        group_by(region) %>%
        summarize(median_count_in_first_window_end = median(y_value[in_first_window]),
                  median_count_in_second_window_end = median(y_value[in_second_window])) %>%
        mutate(count_change = median_count_in_first_window_end-median_count_in_second_window_end,
               pct_count_change = 100 * (median_count_in_second_window_end - median_count_in_first_window_end)
               /median_count_in_first_window_end) %>%
        na.omit %>% 
        filter(abs(count_change) > min_absolute_changes)
      
      if (input$f_show_increases_only) window_changes <- filter(window_changes, count_change < 0)
      
      if (input$by_percent_or_abs == "Biggest by percent") {
        window_changes <- arrange(window_changes, desc(abs(pct_count_change)))
      } else {
        window_changes <- arrange(window_changes, desc(abs(count_change))) 
      }
      plots_per_page <- 9
      if (nrow(window_changes) > plots_per_page) {
        selected_window_changes <- window_changes[1:plots_per_page,]
      } else {
        selected_window_changes <- window_changes
      }
      selected_regions_of_interest <- selected_window_changes$region
    }
    
    region_data_to_plot <- region_data_with_y %>%
      filter(region %in% selected_regions_of_interest) %>%
      filter(date > input$start_date & date < input$end_date)  %>%
      group_by(region) %>%
      arrange(date) %>%
      mutate(predicted_case_rate_from_deaths = lead(new_deaths, input$death_lag)/input$overall_deaths_per_case)

    # plot_title
    plot_title <- paste0("COVID-19 ", input$y, " from ", input$start_date, 
                         " to ", max(region_data_to_plot$date))
    
    validate(
      need(n_distinct(region_data_to_plot$region)>0, "No data match your criteria. Consider changing 'Minimum absolute change' or viewing data at the state level.")
    )
    
    p <- region_data_to_plot %>%
#      ggplot(aes_string(x=variable_for_x, y=input$y)) +
      ggplot(aes(x=date, y=y_value)) +
      geom_point(alpha=0.5) +
      geom_ma(ma_fun = SMA, n = as.numeric(input$moving_average_1), 
              color = "red", size = 2, linetype = 1, alpha = 0.5) +
      theme_grey(base_size=16) +
      expand_limits(y=0) +
      scale_x_date(date_labels = "%m/%d") +
      ggtitle(plot_title) 

    if(input$region_display_option == "Show biggest movers*"){
      p <- p +
        geom_segment(data = selected_window_changes,
                   aes(y=median_count_in_first_window_end, yend=median_count_in_first_window_end), 
                   x=window_start - days(6), xend = window_start, color = "purple", size = 3, alpha = 1) +
        geom_segment(data = selected_window_changes,
                     aes(y=median_count_in_second_window_end, yend=median_count_in_second_window_end), 
                     x=window_end - days(6), xend = window_end, color = "purple", size = 3, alpha = 1) +
      geom_rect(ymin=-Inf, ymax=Inf, xmin=window_start - days(6), xmax = window_start, 
                fill = NA, color = "purple", alpha = 0.2) +
        geom_rect(ymin=-Inf, ymax=Inf, xmin=window_end - days(6), xmax = window_end, 
                     fill = NA, color = "purple", alpha = 0.2) #+
      #        geom_point(data = selected_window_changes,

                 #                  aes(y=median_count_at_week_of_window_end), 
                 # x=window_end - days(3), color = "purple", shape = 1, size = 3, alpha = 2)
    }
    
    if(input$y == "cases" & input$show_inferred_case_prevalance) {
      p <- p +
        geom_point(aes(y=predicted_case_rate_from_deaths), color = "purple")
    }
    
    if (input$f_log_transform_y) {
      p <- p + 
        scale_y_log10(paste("New", input$y, "per day (log10)"), 
                      label=comma) 
    } else {
      p <- p + 
        scale_y_continuous(paste("New", input$y, "per day"), 
                           label=comma)
    }
    
    if (input$f_consistent_y_axes) { 
      p <- p + facet_wrap(~region) 
    } else {
      p <- p + facet_wrap(~region, scales="free_y")
    }
    
    print(p)
    
  }, height=700)
  
}


# Create Shiny object
shinyApp(ui = ui, server = server)
