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
             p("Founded in 1961, the Organization for Economic Co-operation and
                Development (OECD) is an intergovernmental economic organization
                consisting of 36 member countries. Its primary goal is to 
                stimulate international economic progress, facilitate world trade, and collect
                data regarding economic development."),
               mainPanel(
                 plotlyOutput("map"),
                 h2("History of the OECD"),
                 p("In accordance with the US-financed Marshall Plan, the Organization
                   for European Economic Cooperation (OEEC) was established in 1948,
                   with the goal of restructuring the war-ravaged European continent.
                   Individual European governments, many of which were in grave economic circumstances, 
                   recognized the interdependence of their economies, paving the 
                   way for an era of mutual economic cooperation and trust. In 1960, 
                   Canada and the United States both joined by signing the new OECD Convention.
                   Other nations began to join, sparked by Japan's entry in 1964."),
                 plotlyOutput("rich_v_poor"))),
    tabPanel("Overview", 
           mainPanel(
             h3("Social Spending Intro"), 
             p("In this project, I am defining social spending as expenditure comprising in-cash benefits, 
               direct in-kind provision of goods and services, and tax breaks. The OECD defines 
               'social' programs as those which involve redistribution across households and 
               classes. This data includes public benefits, meaning that federal and state 
               governments control financial flows, not private agents. Private transfers are not 
               included in these calculations."),
             plotOutput("image"),
             h3("This graph is an demonstration of social spending in the OECD Countries"),
             p("This graph examines the connection between GDP per capita and life expectancy. T
               he size of each circle in the graph indicates total population."),
             plotOutput("animation"))),
    tabPanel("Economic Factors",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "gdp_per_capita",
              label = "Select Input:",
              choices = c("Economic Inequality", "GDP Per Capita (PPP-adjusted)", "Life Expectancy"), selected = "Economic Inequality")),
        mainPanel(
          plotOutput("gdp_per_cap_graph"))),
        mainPanel(
          p(strong("Economic Inequality:"), "The measure of inequality used in this graph is the Gini coefficient.
            The Gini coefficient is a common gauge of economic inequality, measuring
            income distribution across a population. The coefficient is between 0
            and 1, with 0 meaning perfect equality, and 1 meaning perfect inequality.
            A higher coefficient indicates greater inequality, as it indicates that 
            higher income individuals are receiving much larger percentages of the
            total income of the population. While the Gini can sometimes overstate
            inequality, it is still an important component of a nation's income distribution."),
          h2("Labor Market Freedom"),
          plotOutput("labor_market_graph"),
          p("Analysis of Coefficient and stuff"))),
    tabPanel("About",
             h2("About the Data"),
             p("All data was sourced from the",
               a(href = "https://data.worldbank.org/", "World Bank"),
               "and the",
               a(href = "https://data.oecd.org/", "OECD."),
               "Specifically, I used data regarding",
               a(href = "https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?locations=OE", "GDP"),
               "and",
               a(href = "https://data.worldbank.org/indicator/SP.POP.TOTL?locations=OE", "population"),
               "from the World Bank. I utilized OECD data for",
               a(href = "https://data.oecd.org/socialexp/social-spending.htm", "social spending,"),
               a(href = "https://data.oecd.org/healthstat/life-expectancy-at-birth.htm", "life expectancy,"),
               a(href = "https://data.oecd.org/healthstat/life-expectancy-at-birth.htm", "economic inequality,"),
               "and",
               a(href = "https://data.oecd.org/eduatt/adult-education-level.htm", "education"),
               "statistics."),
             h2("Project Motivations"),
             p("After taking coursework in comparative politics and economics, I became very interested in international economic growth."),
             h2("About Me"),
             p("My name is George Dalianis, and I am a freshman at Harvard College studying government and economics. The source code for this project on my", 
               a(href = "https://github.com/georgedalianis7", "Github"), "account. Contact me at gdalianis@college.harvard.edu.")
    )
  ))
           

server <- function(input, output) {
  output$map <- renderPlotly({
    primary_data %>%
      mutate(x = rep(1, 778)) %>%
      filter(year == 2016) %>%
      plot_geo(locationmode = "country names") %>%
      add_trace(z = ~x, locations = ~country_name, showscale = FALSE) %>%
      layout(title = "Current OECD Member Nations",
             annotations = list(x = 0.9, y = -0.05, text = "Source: OECD", showarrow = FALSE))
  })
  output$rich_v_poor <- renderPlotly({
    primary_data$year <- as.numeric(primary_data$year)
    richest_poorest <- primary_data %>%
      group_by(year) %>%
      filter(year <= 2017) %>%
      filter(country_name %in% c("Luxembourg", "Greece")) %>%
      ggplot(aes(x = year, y = gdp_per_capita, color = country_name)) + 
      geom_line() + 
      geom_vline(xintercept = 2008, linetype = "dashed") + 
      scale_color_manual(labels = c("Greece", "Luxembourg"),
                         values = c("blue", "red")) +
      theme_classic() + 
      labs(title = "Richest and Poorest OECD Countries: 1995-2016",
           subtitle = "Measured by GDP Per Capita, US$",
           x = "Year",
           y = "GDP Per Capita, US$",
           color = "Country")
    richest_poorest1 <- ggplotly(richest_poorest) %>%
      layout(autosize = FALSE) 
  })
  
  output$image <- renderPlot({
      readRDS(file = "soc_spending_plot.rds")
  })
  
  output$animation <- renderImage({
    list(src = "life_exp_animation",
         contentType = "image/gif",
         width = 600)}, deleteFile = FALSE)
  
  
  output$gdp_per_cap_graph <- renderPlot({
    if(input$gdp_per_capita == "Economic Inequality") {
      readRDS(file = "ineq_regression.rds")
    }
    else if(input$gdp_per_capita == "GDP Per Capita (PPP-adjusted)"){
      readRDS(file = "soc1_regression.rds")
    } 
    else{
      readRDS(file = "life_exp_regression.rds")
    }
    
  })
  output$labor_market_graph <- renderPlot({
    readRDS(file = "mark_regression.rds")
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
