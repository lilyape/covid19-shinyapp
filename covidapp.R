library(shiny)
library(covdata)
library(tidyverse)

data("covnat")


ui <- fluidPage(
  titlePanel("COVID-19 Cases by Country"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("country", "Select Country (or countries):", choices = unique(covnat["cname"]), multiple = TRUE, selected = "Afghanistan"),
      dateRangeInput("daterange", "Select Date Range:", 
                     start = "2019-12-31",
                     end = Sys.Date()),
      checkboxInput("linear", "Add trend line?", value = FALSE),
      submitButton("Apply Changes", icon("refresh"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("New Cases", plotOutput("cases")),
        tabPanel("New Deaths", plotOutput("deaths")),
        tabPanel("Cumulative Cases", plotOutput("cumcases")),
        tabPanel("Cumulative Deaths", plotOutput("cumdeaths"))
      )
    )
  )
)

server <- function(input, output) {
  
  countrydata <- reactive({
    covnat %>% 
      filter(cname == input$country, date >= input$daterange[1], date <= input$daterange[2])
  })
  
  cumdata <- reactive({
    covnat %>% 
      filter(date == input$daterange[2])
  })
  
  output$cases = renderPlot({
    
    casesPlot <- countrydata() %>% 
      ggplot(aes(x = date, y = cases, colour = cname)) + 
      geom_line() + ggtitle("New COVID-19 cases by country over time") + 
      xlab("Date") + ylab("Number of New Cases") + labs(colour = "Country", caption = "Data: https://www.ecdc.europa.eu/") +
      scale_y_continuous(labels = scales::comma) + theme_minimal()
    
    if(input$linear){
      casesPlot = casesPlot + geom_smooth(method = "lm")
    }
    print(casesPlot)
  })
  
  output$deaths = renderPlot({
    
    deathsPlot <- countrydata() %>% 
      ggplot(aes(x = date, y = deaths, colour = cname)) + 
      geom_line() + ggtitle("New COVID-19 related deaths by country over time") +
      xlab("Date") + ylab("Number of New Deaths") + labs(colour = "Country", caption = "Data: https://www.ecdc.europa.eu/") +
      scale_y_continuous(labels = scales::comma) + theme_minimal()
    
    if(input$linear){
      deathsPlot = deathsPlot + geom_smooth(method = "lm")
    }
    print(deathsPlot)
  })
  
  output$cumcases = renderPlot({
    
    countrydata() %>%
    ggplot(aes(x = date, y = cu_cases, colour = cname)) + 
    geom_line() + ggtitle("Cumulative COVID-19 cases by country over time") +
    xlab("Date") + ylab("Number of Cases") + labs(colour = "Country", caption = "Data: https://www.ecdc.europa.eu/") +
    scale_y_continuous(labels = scales::comma) + theme_minimal()
    
  })
  
  output$cumdeaths = renderPlot({
    
    countrydata() %>%
      ggplot(aes(x = date, y = cu_deaths, colour = cname)) + 
      geom_line() + ggtitle("Cumulative COVID-19 related deaths by country over time") +
      xlab("Date") + ylab("Number of Deaths") + labs(colour = "Country", caption = "Data: https://www.ecdc.europa.eu/") +
      scale_y_continuous(labels = scales::comma) + theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
