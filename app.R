#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(tidyverse)
library(lubridate)
url_path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

if (file.exists("last_download.rds")) {
    last_download <- readRDS("last_download.rds")
    current_date_time <- now(tzone = "UTC")
}
if (!file.exists("last_download.rds") || as.POSIXct(last_download, tz = "UTC") + dhours(51) <= current_date_time) {
    data <- c(
        confirmed = 'time_series_19-covid-Confirmed.csv',
        deaths = 'time_series_19-covid-Deaths.csv',
        recovered = 'time_series_19-covid-Recovered.csv'
    ) %>% 
        map_dfr(~read_csv(file.path(url_path, .x)), .id = "type")
    cv19 <- data %>%
        pivot_longer(
            cols = -(type:Long),
            names_to = "date",
            values_to = "value"
        ) %>% 
        mutate(date = lubridate::mdy(date))
    
    post_100 <- cv19 %>% 
        filter(
            type %in% c("confirmed", "deaths")
        ) %>% 
        mutate(country = coalesce(`Province/State`, `Country/Region`)) %>% 
        group_by(date, country = `Country/Region`, type) %>% 
        summarise(value = sum(value)) %>% 
        group_by(country) %>% 
        filter(any(value >= 100)) %>% 
        filter(date >= min(date[type == "confirmed" & value >= 100])) %>% 
        mutate(days_post_100 = (min(date) %--% date) / ddays(1))
    
    last_download <- max(cv19$date)
    saveRDS(last_download, "last_download.rds")
    saveRDS(cv19, "cv19.rds")
    saveRDS(post_100, "post_100.rds")
} else {
    post_100 <- readRDS("post_100.rds")
}

country_choices <- post_100 %>% 
    filter(type == "confirmed") %>% 
    group_by(country) %>% 
    summarise(label = sprintf("%s (%d)", country[1], max(value))) %>% 
    ungroup() %>% 
    arrange(country)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 interactive time series"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            div(strong("Last updated: "), textOutput("last_updated", inline = TRUE)),
            div(strong("Countries to include:")),
            div(
                style = "column-count: 2;",
                checkboxGroupInput(
                    "countries",
                    label = NULL,
                    choiceNames = country_choices$label,
                    choiceValues = country_choices$country,
                    selected = "United Kingdom"
                )
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h3("Confirmed cases"),
           plotOutput("cv19_plot"),
           h3("Deaths"),
           plotOutput("death_plot"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$last_updated <- renderText(format(last_download, "%Y-%m-%d"))
    
    vals <- reactiveValues(post_100_f = NULL)
    
    observeEvent(input$countries, {
        if (length(input$countries) > 0) {
            vals$post_100_f <- post_100 %>% filter(
                country %in% input$countries
            )
        }
    })
    
    output$cv19_plot <- renderPlot({
        if (!is.null(vals$post_100_f)) {
            vals$post_100_f %>% 
                filter(type == "confirmed") %>% 
                ggplot(aes(days_post_100, value)) +
                geom_line(aes(colour = country), size = 1.5) +
                scale_y_log10() +
                facet_wrap(quos(type)) +
                geom_line(data = tibble(
                    days_post_100 = c(0, max(vals$post_100_f$days_post_100)),
                    value = c(100, 100 * (4/3) ^ max(days_post_100))
                ),
                linetype = 2)
        }
    })
    
    output$death_plot <- renderPlot({
        if (!is.null(vals$post_100_f)) {
            vals$post_100_f %>% 
                filter(type == "deaths") %>% 
                ggplot(aes(days_post_100, value)) +
                geom_line(aes(colour = country), size = 1.5) +
                scale_y_log10() +
                facet_wrap(quos(type)) +
                geom_line(data = tibble(
                    days_post_100 = c(0, max(vals$post_100_f$days_post_100)),
                    value = c(1, 1 * (4/3) ^ max(days_post_100))
                ),
                linetype = 2)
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
