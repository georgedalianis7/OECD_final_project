#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- navbarPage(
  "Social Spending Programs in the OECD",
  tabPanel("Model",
           fluidPage(
             titlePanel("Model Title"),
               mainPanel(plotOutput("preImage")))),
  tabPanel("Discussion",
           titlePanel("Discussion Title"),
           p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
  tabPanel("About", 
           titlePanel("About"),
           h3("Project Background and Motivations"),
           p(""),
           h3("About Me"),
           p("My name is George Dalianis and I study government and economics at Harvard University. 
             You can reach me at gdalianis@college.harvard.edu.")))

server <- function(input, output) {
  output$preImage <- renderImage({
    
    filename <- normalizePath(file.path("oecd_social_benefits.png"))
    
    list(src = filename,
         alt = 'plot',
         height = 700)
   
    
  }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
