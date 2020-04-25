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
library(shinythemes)
library(readr)
library(plotly)

primary_data <- read_rds("primary_data.rds")
inequality <- read_rds("inequality_data.rds")
education <- read_rds("education_data.rds")

ui <- fluidPage(theme = shinytheme("cosmo"),
  titlePanel("Social Spending Programs in the OECD"),
  tabsetPanel(
    tabPanel("About",
             h2("What is the OECD?"),
             h4("Founded in 1961, the Organisation for Economic Co-operation and
                Development (OECD) is an intergovernmental economic organisation
                with 36 member countries as of 2020. Its primary goal is to 
                stimulate economic progress, facilitate world trade, and collect
                data regarding economic development."),
               mainPanel(
                 plotlyOutput("map"),
                 h2("About the Data"),
                 h4()
                 )
               ),
    tabPanel("Overview", 
           mainPanel(
             h3("Social Spending Intro"), 
             plotOutput("preImage"),
             h3("This graph is an demonstration of social spending in the OECD Countries"))),
    tabPanel("Econ",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "gdp_per_capita",
              label = "Economic Changes",
              choices = c("GDP Per Capita in US Dollars" = "GDP Per Capita"
            )
          )
        ),
        mainPanel(
          plotOutput("gdp_per_cap_graph"))
        )),
    tabPanel("About", 
           titlePanel("About"),
           h3("Project Background and Motivations"),
           p(""),
           h3("About Me"),
           p("My name is George Dalianis and I study government and economics at Harvard University. 
             You can reach me at gdalianis@college.harvard.edu."))))

server <- function(input, output) {
  output$map <- renderPlotly({
    primary_data %>%
      mutate(x = rep(1, 778)) %>%
      plot_geo(locationmode = "country names") %>%
      add_trace(z = ~x, locations = ~country_name, showscale = FALSE)
  })
  output$preImage <- renderImage({
    
    filename <- normalizePath(file.path("oecd_social_benefits.png"))
    
    list(src = filename,
         alt = 'plot',
         height = 600,
         width = 600)
  }, deleteFile = FALSE)
  output$gdp_per_cap_graph <- renderPlot({
    if(input$gdp_per_capita == "GDP per Capita") {
      y_value <- primary_data$gdp_per_capita
      y_lab <- "GDP per Capita"
      gdp_title <- "OECD GDP per Capita"
    }
    primary_data %>%
      filter(year == 2017) %>%
      ggplot(aes(pct_soc_spending, gdp_per_capita)) +
      geom_point() +
      labs(title = "z",
           x = "Country",
           y = "x") +
      theme_classic()
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
