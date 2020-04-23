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

app_version <- 0.05
last_app_version <- 0

if (file.exists("last_download.rds")) {
    last_download <- readRDS("last_download.rds")
    if (file.exists("last_app_version.rds")) {
        last_app_version <- readRDS("last_app_version.rds")
    }
    current_date_time <- now(tzone = "UTC")
}
if (!file.exists("last_download.rds") ||
    as.POSIXct(last_download, tz = "UTC") + dhours(51) <= current_date_time ||
    last_app_version != app_version) {
    url_path <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
    data <- c(
        confirmed = 'time_series_covid19_confirmed_global.csv',
        deaths = 'time_series_covid19_deaths_global.csv',
        recovered = 'time_series_covid19_recovered_global.csv'
    ) %>% 
        map_dfr(~read_csv(file.path(url_path, .x)), .id = "type")
    
    cv19 <- data %>%
        pivot_longer(
            cols = -(type:Long),
            names_to = "date",
            values_to = "value"
        ) %>% 
        mutate(date = lubridate::mdy(date))
    
    cv19_g <- cv19 %>% 
        filter(
            type %in% c("confirmed", "deaths")
        ) %>% 
        mutate(country = coalesce(`Province/State`, `Country/Region`)) %>% 
        group_by(date, country = `Country/Region`, type) %>% 
        summarise(value = sum(value))
    
    post_100_corr <- cv19_g %>% 
        group_by(country) %>% 
        filter(any(value >= 100)) %>% 
        filter(date >= min(date[type == "confirmed" & value >= 100]) - ddays(1)) %>% 
        summarise(correction_for_100 = min(date) - ddays(1) *
                   log(min(value[type == "confirmed" & value >= 100]) / 100) / log(4 / 3) - 1)# Correction for values above 100
    
    post_100 <- cv19_g %>% 
        inner_join(post_100_corr %>% select(country, correction_for_100), by = "country") %>% 
        mutate(days_post_100 = (correction_for_100 %--% date) / ddays(1)) %>% 
        filter(days_post_100 > -1)
    
    timing_table <- cv19_g %>% 
        inner_join(post_100_corr %>% select(country, correction_for_100), by = "country") %>% 
        mutate(days_post_100 = (correction_for_100 %--% date) / ddays(1)) %>% 
        group_by(country) %>% 
        group_modify(~tibble(
            date_of_100 = floor_date(.x$correction_for_100[1], "days"),
            cases = max(.x$value),
            cases_model = list(lm(log(value) ~ days_post_100, data = filter(.x, type == "confirmed", value >= 100))),
            cases_slope = exp(cases_model[[1]]$coefficients[2]),
            cases_doubling_time = log(2) / cases_model[[1]]$coefficients[2],
            deaths = max(.x$value[.x$type == "deaths"]),
            deaths_model = list(tryCatch(lm(log(value) ~ days_post_100, data = filter(.x, type == "deaths", value >= 10)), error = function(e) NA_real_)),
            deaths_slope = tryCatch(exp(deaths_model[[1]]$coefficients[2]), error = function(e) NA_real_),
            deaths_doubling_time = tryCatch(log(2) / deaths_model[[1]]$coefficients[2], error = function(e) NA_real_)
        )) %>% 
        ungroup()
        
    
    last_download <- max(cv19$date)
    saveRDS(last_download, "last_download.rds")
    saveRDS(app_version, "last_app_version.rds")
    saveRDS(cv19, "cv19.rds")
    saveRDS(post_100, "post_100.rds")
    saveRDS(timing_table, "timing_table.rds")
} else {
    post_100 <- readRDS("post_100.rds")
    timing_table <- readRDS("timing_table.rds")
}

country_choices <- timing_table %>% 
    transmute(
        country,
        label = sprintf("%s (%d)", country, cases)
    ) %>% 
    ungroup() %>% 
    arrange(country)

# Define UI for application that draws a histogram
ui <- function(request) {
    fluidPage(
    
    # Application title
    titlePanel("COVID-19 interactive time series"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            div(strong("Most recent data: "), textOutput("last_updated", inline = TRUE)),
            div(strong("Countries to include")," (cases):"),
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
            div("Dashed line indicates 33% daily increase"),
            h3("Confirmed cases"),
            plotOutput("cv19_plot"),
            h3("Deaths"),
            plotOutput("death_plot"),
            h3("Timings"),
            tableOutput("timing_table"),
            p("Cases model fitted from 100 cases on; death model fitted from 10 deaths on. Both models fitted as linear models with formula of log(value) ~ days.")
        )
    ),
    p(
        "Interactive time series for COVID-19 data. The data are obtained from",
        a(href = "https://github.com/CSSEGISandData/COVID-19/",
          "https://github.com/CSSEGISandData/COVID-19/"), ") and are copyright Johns
                Hopkins University. Further information on the ultimate data sources
                are available from ",
        a(href = "https://github.com/CSSEGISandData/COVID-19/blob/master/README.md", "the Johns Hopkins repository")
    ),
    p("Countries are only included if there have been more than 100 cases, and
                the time axis is set to be zero at the date where 100 cases were reached. If there
                is not a day with exactly 100 cases, the date is adjusted on the assumption
                of a 33% daily relative increase at that time."
    ),
    p("Code for this Shiny app is available from ",a(href = "https://github.com/NikNakk/covid19", "My GitHub"))
    )}


server <- function(input, output, session) {
    
    observe({
        reactiveValuesToList(input)
        session$doBookmark()
    })
    
    onBookmarked(function(url) {
        updateQueryString(url)
    })
    
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
    
    output$timing_table <- renderTable({
        if (!is.null(input$countries)) {
            timing_table %>% 
                filter(country %in% input$countries) %>% 
                transmute(
                    Country = country,
                    `Total cases` = as.integer(cases),
                    `Date reached 100` = format(date_of_100, "%Y-%m-%d"),
                    `Cases: daily increase` = sprintf("%0.1f%%", 100 * (cases_slope - 1)),
                    `Cases: doubling time (days)` = sprintf("%0.1f", cases_doubling_time),
                    `Deaths` = as.integer(deaths),
                    `Deaths: daily increase` = sprintf("%0.1f%%", 100 * (deaths_slope - 1)),
                    `Deaths: doubling time (days)` = sprintf("%0.1f", deaths_doubling_time),
                )
        }
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
