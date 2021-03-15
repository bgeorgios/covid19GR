# load packages
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(zoo)
library(ggplot2)
library(scales)

get.data <- function() {
  # Scrapes vaccination and COVID-19 data from respective github repositories
  
  # Returns:
  # A dataframe containing data for Greece
  vaccine.url <-
    'https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_global.csv'
  vaccine.global <- read_csv(vaccine.url)
  vaccine.greece <-
    vaccine.global %>% filter(Country_Region == 'Greece') %>% select(Date, People_partially_vaccinated, People_fully_vaccinated)
  vaccine.date <- as.character(vaccine.greece[['Date']])
  covid.url <-
    'https://raw.githubusercontent.com/datasets/covid-19/main/data/time-series-19-covid-combined.csv'
  covid.global <- read.csv(covid.url)
  covid.greece <-
    covid.global %>% filter(Country.Region == 'Greece') %>% select(Date, Confirmed, Deaths)
  covid.greece <-
    covid.greece %>% mutate(Daily.Cases = diff(zoo(Confirmed), na.pad = TRUE))
  covid.greece <-
    covid.greece %>% mutate(Daily.Deaths = diff(zoo(Deaths), na.pad = TRUE))
  covid.greece <- covid.greece %>% filter(Date %in% vaccine.date)
  combined.greece <- cbind(vaccine.greece, covid.greece)
  combined.greece[['Date']] <- NULL
  combined.greece[['Date']] <- as.Date(combined.greece[['Date']])
  return(combined.greece)
}

# Build User-Interface for Shiny app
my.ui <-
  shinyUI(fluidPage(
    titlePanel(
      h1("Tracking vaccination against COVID-19 in Greece", align = 'center'),
      windowTitle = 'covid19GR - bgeorgios'
    ),
    fluidRow(column(6,
                    plotOutput(outputId = "plot1")),
             column(6,
                    plotOutput(outputId = "plot2"))),
    fluidRow(column(6,
                    plotOutput(outputId = "plot3")),
             column(6,
                    plotOutput(outputId = "plot4")))
  ))

# Define Server for Shiny app
my.server <- function(input, output) {
  combined.greece <- reactive({
    invalidateLater(1000 * 60 * 60 * 24)
    get.data()
  })
  output$plot1 <-
    renderPlot({
      ggplot(data = combined.greece(), aes(x = Date, y = People_fully_vaccinated)) +
        geom_line(color = "#339999", size = 1.25) + xlab("") + ylab('People fully vaccinated\n') +
        scale_x_date(date_breaks = "1 week", date_labels = "%d %b %Y") +
        theme(axis.text.x = element_text(
          angle = 45,
          vjust = 0.5,
          hjust = 1
        )) +
        scale_y_continuous(labels = comma) +
        theme(text = element_text(size = rel(4)))
    })
  output$plot3 <-
    renderPlot({
      ggplot(data = combined.greece(), aes(x = Date, y = Daily.Cases)) +
        geom_line(color = "#FF9999", size = 1.25) + xlab("") + ylab('Daily cases\n') +
        scale_x_date(date_breaks = "1 week", date_labels = "%d %b %Y") +
        theme(axis.text.x = element_text(
          angle = 45,
          vjust = 0.5,
          hjust = 1
        )) +
        scale_y_continuous(labels = comma) +
        theme(text = element_text(size = rel(4)))
    })
  output$plot2 <-
    renderPlot({
      ggplot(data = combined.greece(), aes(x = Date, y = People_partially_vaccinated)) +
        geom_line(color = "#FF9933", size = 1.25) + xlab("") + ylab('People partially vaccinated \n') +
        scale_x_date(date_breaks = "1 week", date_labels = "%d %b %Y") +
        theme(axis.text.x = element_text(
          angle = 45,
          vjust = 0.5,
          hjust = 1
        )) +
        scale_y_continuous(labels = comma) +
        theme(text = element_text(size = rel(4)))
    })
  output$plot4 <-
    renderPlot({
      ggplot(data = combined.greece(), aes(x = Date, y = Daily.Deaths)) +
        geom_line(color = "#CC0000", size = 1.25) + xlab("") + ylab('Daily Deaths \n') +
        scale_x_date(date_breaks = "1 week", date_labels = "%d %b %Y") +
        theme(axis.text.x = element_text(
          angle = 45,
          vjust = 0.5,
          hjust = 1
        )) +
        scale_y_continuous(labels = comma) +
        theme(text = element_text(size = rel(4)))
    })
}

# Deploy Shiny app
shinyApp(ui = my.ui, server = my.server)
