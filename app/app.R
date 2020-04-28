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
choices1 <- read_rds("choices_data.rds")

ui <- fluidPage(theme = shinytheme("cosmo"),
  titlePanel("Social Spending Programs in the OECD"),
  tabsetPanel(
    tabPanel("OECD Overview",
             h2("What is the OECD?"),
             p("Founded in 1961, the Organization for Economic Co-operation and
                Development (OECD) is an intergovernmental economic organization
                consisting of 36 member countries. Its primary goal is to 
                stimulate international economic progress, facilitate world trade, and collect
                data regarding economic development. The OECD was established to honor 
               Secretary of State George Marshall's request for 'some agreement among 
               the countries of Europe as to the requirements of the situation and the part those 
               countries themselves will take.'"),
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
    tabPanel("Social Spending: A Closer Look", 
           mainPanel(
             h3("Social Spending Intro"), 
             p("In this project, I am defining social spending as expenditure comprising in-cash benefits, 
               direct in-kind provision of goods and services, and tax breaks. The OECD defines 
               'social' programs as those which involve redistribution across households and 
               classes. This data includes public benefits, meaning that federal and state 
               governments control financial flows, not private agents. Private transfers are not 
               included in these calculations."),
             plotOutput("image"),
             h3("OECD Development Over Time"),
             p("This graph examines the connection between GDP per capita and life expectancy. The size of each circle in the graph indicates total population."),
             plotOutput("animation"))),
    tabPanel("Country Comparison",
             titlePanel(
               textOutput("graph_title")
             ),
             sidebarLayout(
               sidebarPanel(
                 selectInput("country", label = "Select Country:",
                             choices = choices1, selected = "Australia"),
                htmlOutput("graph_des")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Social Spending",
                            plotlyOutput("fin_plot"))
                 )
               )
             )),
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
            inequality, it is still an important component of a nation's income 
            distribution. The coefficient between inequality and social spending
            displays a slight negative relationship, with a coefficient of -.004. This means that a 1% increase in social spending as a percent of GDP is associated with a .004 decline in the Gini coefficient. So increasing social spending programs is associated with less inequality."),
          p(strong("GDP Per Capita:"), "Correlation Coefficient of 830.05. A 1% increase in social spending as a % of GDP is associated with an increase in GDP per capita by about $830."),
          p(strong("Life Expectancy:"), "Correlation Coefficient of 0.10. A 1% increase in social 
            spending as a % of GDP is associated with a .1 increase in life expectancy."),
          h2("Labor Market Regulation"),
          plotOutput("labor_market_graph"),
          p(strong("Labor Market Regulation:"), "Correlation Coefficient between 
              social and spending and labor regulations is 0.05. For every 1% increase
              in social spending, the labor regulation index score increases by half of
              one point. The labor regulation index, used by the OECD to observe the ease
              at which workers can switch jobs and start businesses, increases as the
              labor market is more restrictive. We can see that increased social spending
              is associated with a more restrictive labor market, which could hurt 
              workers looking to switch jobs, and makes it harder to fire workers 
              who are not performing well.")
          )),
    tabPanel("Social/Societal Factors",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId = "migration",
                   label = "Select Input:",
                   choices = c("Migration", "Unemployment Spending", "Pension Spending"), selected = "Migration")),
               mainPanel(
                 plotOutput("migration_graph")
               )
                 ),
             mainPanel(
             plotOutput("migration_gra")
                 )),
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
               "statistics. The earliest year of data used in this project is 1995 
               and the latest is 2016. New OECD member countries include Lithuania (2018), 
               Latvia (2016), Slovenia (2010), and Israel(2010). Data from these
               countries was excluded in certain graphs because they were not OECD 
               members at the time studied in the graphs."),
             h2("Project Motivations"),
             p("I'm interested in institutions/policies that either spur or hinder 
               economic growth. By examining factors like GDP per capita, labor markets,
               life expectancy, and their connection social spending, I was able to
               understand the broader policies behind economic growth.
               I first considered examining the conditions of a
               single country, Greece, but I found the larger trends within OECD
               member nations to be much more intriguing."),
             h2("About Me"),
             p("My name is George Dalianis, and I am a freshman at Harvard College studying government and economics. I hope that you enjoyed my project! The source code for this project on my", 
               a(href = "https://github.com/georgedalianis7", "Github"), "account. Contact me at gdalianis@college.harvard.edu.")
    )))
  
           

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
  output$migration_graph <- renderPlot({
    if(input$migration == "Unemployment Spending") {
      readRDS(file = "mig_graph.rds")
    }
    else if(input$migration == "Pension Spending"){
      readRDS(file = "pen_graph.rds")
    }
    else{
      readRDS(file = "mig_2.rds")
    }
    
    
  })
  
  output$graph_title <- renderText({
    paste("Social Spending By Country")
  })
  output$graph_des <- renderText({
  paste("Choose an OECD member nation to see total social spending as a percent of GDP, from 1995 to 2018")
  })
  graph_react <- reactive({
    primary_data %>%
      filter(country_name == input$country) %>%
      na.omit()
  })
  output$fin_plot <- renderPlotly({
    
    data <- graph_react()
    
    x <- ggplot(data, aes(x = as.numeric(year), y = pct_soc_spending)) + 
      geom_line(color = "blue") +
      geom_point(aes(text = paste0("Year: ", year, "\n",
                                   "Soc. Spending: ",
                                   round(pct_soc_spending, 2),
                                   "% of GDP", sep = "")), color = "blue") +
      labs(title = paste(data$country_name),
           x = "Year",
           y = "Social Spending as % of GDP") +
      theme_classic()
    ggplotly(x, tooltop = "text")
  })
  output$migration_gra <- renderPlot({
    readRDS(file = "migrationd.rds")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
