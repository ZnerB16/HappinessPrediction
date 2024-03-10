library(shiny)
library(shinythemes)
library(ggplot2)
setwd("C:/Users/Brenz Gwynne Hababag/Desktop/ZnerB/Skwela La Kolohiya/Second Year/Second Sem/Data Analytics/Codes/MP 2/HABABAG_LE1") 
df <- read.csv("world_happiness_report_2015_2022_cleaned.csv", header = TRUE, encoding = "utf-8")

southEast <- df[which(df$Region == "Southeastern Asia"),]

happiness <- as.numeric(southEast$Happiness.Score)
econ <- as.numeric(southEast$Economy)
social <- as.numeric(southEast$Family)
health <- as.numeric(southEast$Health)
years <- as.numeric(southEast$Year)

# Define UI 
ui <- fluidPage(theme = shinytheme("cosmo"),
                
                # Application title
                
                navbarPage(title = "Southeast Asia Happiness Report",
                           tabPanel("About",
                                    sidebarLayout( 
                                      
                                      sidebarPanel(
                                        img(src = "me.jpg", width = "200px", 
                                            style = "display: block; margin-left: auto; margin-right: auto; border-radius: 50%;"
                                        ),
                                        h3("About Me:", style = "text-align = center;"),
                                        h4("I am Brenz Gwynne M. Hababag, a student of the University of
                             Southeastern Philippines (USeP). I am currently a 2nd Year studying 
                             the course Bachelor of Science in Computer Science Major in Data Science.",
                                           style = "text-align: justify; text-justify: inter-word;")
                                        
                                      ),
                                      mainPanel(
                                        h3("This web project is a learning experience and requirement of 
                          CS227 - Data Analytics Learning Evidence 1."
                                           , style = "text-align: justify; text-justify: inter-word;")
                                      )
                                    )
                           ),
                           tabPanel("Regression",
                                    navbarPage("Linear Regression(Simple and Multiple)",
                                               tabPanel("Economy(GDP Per Capita)",
                                                        h4("Check for Normality, Linearity and Homoscedasticity"),
                                                        h5("Normality:"),
                                                        plotOutput("histHap"),
                                                        h5("Linearity"),
                                                        plotOutput("lineEcon"),
                                                        h5("Homoscedasticity"),
                                                        textOutput("homoEcon1"),
                                                        textOutput("homoEcon2"),
                                                        textOutput("homoEcon3"),
                                                        textOutput("homoEcon4"),
                                                        textOutput("homoEcon5"),
                                                        textOutput("homoEcon6"),
                                                        textOutput("homoEcon7")
                                               ),
                                               tabPanel("Family(Social Support)",
                                                        h4("Check for Normality, Linearity and Homoscedasticity"),
                                                        h5("Normality:"),
                                                        plotOutput("histHap"),
                                                        h5("Linearity"),
                                                        plotOutput("lineFamily"),
                                                        h5("Homoscedasticity"),
                                                        textOutput("homoFamily1"),
                                                        textOutput("homoFamily2"),
                                                        textOutput("homoFamily3"),
                                                        textOutput("homoFamily4"),
                                                        textOutput("homoFamily5"),
                                                        textOutput("homoFamily6"),
                                                        textOutput("homoFamily7")
                                               ),
                                               tabPanel("Health(Life Expectancy)",
                                                        h4("Check for Normality, Linearity and Homoscedasticity"),
                                                        h5("Normality:"),
                                                        plotOutput("histHap"),
                                                        h5("Linearity"),
                                                        plotOutput("lineHealth"),
                                                        h5("Homoscedasticity"),
                                                        textOutput("homoHealth1"),
                                                        textOutput("homoHealth2"),
                                                        textOutput("homoHealth3"),
                                                        textOutput("homoHealth4"),
                                                        textOutput("homoHealth5"),
                                                        textOutput("homoHealth6"),
                                                        textOutput("homoHealth7")
                                               ),
                                               tabPanel("All Factors",
                                                        h4("Check for Normality, Linearity and Homoscedasticity"),
                                                        h5("Normality:"),
                                                        plotOutput("histHap"),
                                                        h5("Linearity"),
                                                        plotOutput("lineHealth"),
                                                        h5("Homoscedasticity"),
                                                        textOutput("homoHealth1"),
                                                        textOutput("homoHealth2"),
                                                        textOutput("homoHealth3"),
                                                        textOutput("homoHealth4"),
                                                        textOutput("homoHealth5"),
                                                        textOutput("homoHealth6"),
                                                        textOutput("homoHealth7")
                                               )
                                    )
                           ),
                           tabPanel("Correlation",
                                    sidebarLayout(
                                      sidebarPanel(
                                        
                                      ),
                                      mainPanel(
                                        
                                      )
                                    )
                           ),
                           tabPanel("Graphs and Plots",
                                    
                           ),
                           tabPanel("Table Data",
                                    tableOutput("allTable")  
                           ),
                           
                )
)

# Define server 
server <- function(input, output) {
  output$histHap <- renderPlot({
    hist(gdp)
  })
  output$allTable <- renderTable(southEast, options = list(iDisplayLength = 10))
}

# Run the application 
shinyApp(ui = ui, server = server)
