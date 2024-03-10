library(shiny)
library(shinythemes)
library(shinyBS)
library(ggplot2)
library(maps)
library(tidyverse)
library(dplyr)


data <- read.csv("world_happiness_report_2015_2022_cleaned.csv")
southEast <- data[which(data$Region == "Southeastern Asia"),]
typo <- data[which(data$Region == "Southeast Asia"),]
tableS <- data[which(data$Region == "Southeast Asia" | data$Region == "Southeastern Asia"),]
happiness <- as.numeric(southEast$Happiness.Score)
gdp <- as.numeric(southEast$Economy)
social <- as.numeric(southEast$Family)
health <- as.numeric(southEast$Health)
year <- as.numeric(southEast$Year)
country <- southEast$Country

c2020 <- typo[which(typo$Year == "2020"),]
c2021 <- typo[which(typo$Year == "2021"),]
c2022 <- typo[which(typo$Year == "2022"),]

# Define UI 
ui <- fluidPage(theme = shinytheme("cosmo"),
                tags$head(
                  tags$style(HTML("hr {border-top: 1px solid #808080;},
                                  h4 {font-style: bold;}"))
                ),
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
                          CS227 - Data Analytics Learning Evidence 1. The dataset used, retrieved from Kaggle.com, 
                               is the World Happiness Report from years 2015-2022. This dataset is used to correlate, graph and 
                               use regression analysis on the factors that contribute to the happiness score of countries and people in 
                                Southeast Asia."
                               , style = "text-align: justify; text-justify: inter-word;")
                          )
                        )
               ),
               tabPanel("Regression",
                 navbarPage("Linear Regression(Simple and Multiple)",
                            tabPanel("Economy(GDP per Capita)",
                                     sidebarLayout(
                                       
                                       mainPanel(
                                         splitLayout(cellWidths = c("50%", "50%"), 
                                                     h3("Normality:",plotOutput("histEcon")),
                                                     h3("Linearity:",plotOutput("lineEcon"))
                                         ),
                                         h3("Homoscedasticity:"),
                                         h4("Residuals:"),
                                         splitLayout(cellWidths = c("20%", "20%", "20%", "20%", "20%"),
                                                     p("Min:", textOutput("min1")),
                                                     p("1Q:", textOutput("fQ1")),
                                                     p("Median:", textOutput("med1")),
                                                     p("3Q:", textOutput("tQ1")),
                                                     p("Max", textOutput("max1"))
                                         ),
                                         hr(),
                                         h4("Coefficients:"),
                                         splitLayout(cellWidths = c("20%", "20%", "20%", "20%", "20%"),
                                                     p("Predictors:", p("(Intercept)"), p("GDP")),
                                                     p("Estimate:" ,p(textOutput("inter1")), p(textOutput("gdp"))),
                                                     p("Std. Error", p(textOutput("error1")), p(textOutput("gdpE"))),
                                                     p("T-Value", p(textOutput("t1")), p(textOutput("gdpT"))),
                                                     p("Pr(>|t|)", p(textOutput("pr1")), p(textOutput("gdpPr")))
                                         ),
                                         hr(),
                                         h4("P-Value:"), 
                                         textOutput("pVal1"),
                                         hr()
                                       ),
                                       sidebarPanel(
                                         h3("Linear Regression Model (GDP and Happiness):"),
                                         h5("Formula: Happiness = intercept + slope(gdp) +/- e"),
                                         h5("Predicted Formula: 3.5492 + 1.524373(gdp) +/- 0.1728031"),
                                         
                                         textInput('input1', 'Enter a GDP per capita value:'),
                                         
                                         bsTooltip(id = "input1", title = "Input decimal value (Ex. 1.0 = $60000)", 
                                                   placement = "left", trigger = "hover"),
                                         h4("Predicted Happiness:"),
                                         textOutput("predGdp")
                                       ),
                                     )
                                    
                                     ),
                            
                                     
                            tabPanel("Family(Social Support)",
                                     mainPanel(
                                     splitLayout(cellWidths = c("50%", "50%"), 
                                        h4("Normality:",plotOutput("histFamily")),
                                        h4("Linearity:",plotOutput("lineFamily"))
                                        ),
                                     h3("Homoscedasticity:"),
                                     h4("Residuals:"),
                                     splitLayout(cellWidths = c("20%", "20%", "20%", "20%", "20%"),
                                                 p("Min:", textOutput("min2")),
                                                 p("1Q:", textOutput("fQ2")),
                                                 p("Median:", textOutput("med2")),
                                                 p("3Q:", textOutput("tQ2")),
                                                 p("Max", textOutput("max2"))
                                     ),
                                     hr(),
                                     h4("Coefficients:"),
                                     splitLayout(cellWidths = c("20%", "20%", "20%", "20%", "20%"),
                                                 p("Predictors:", p("(Intercept)"), p("Social")),
                                                 p("Estimate:" ,p(textOutput("inter2")), p(textOutput("fam"))),
                                                 p("Std. Error", p(textOutput("error2")), p(textOutput("famE"))),
                                                 p("T-Value", p(textOutput("t2")), p(textOutput("famT"))),
                                                 p("Pr(>|t|)", p(textOutput("pr2")), p(textOutput("famPr")))
                                     ),
                                     hr(),
                                     h4("P-Value:"), 
                                     textOutput("pVal2"),
                                     hr()
                            ),
                            sidebarPanel(
                              h3("Linear Regression Model (Social Support and Happiness):"),
                              h5("Formula: Happiness = intercept + slope(support) +/- e"),
                              h5("Predicted Formula: 3.722491 + 1.53568(support) +/- 0.3831837"),
                              
                              textInput('input2', 'Enter a Social Support value:'),
                              
                              bsTooltip(id = "input2", title = "Input decimal value (Ex. 1.35)", 
                                        placement = "left", trigger = "hover"),
                              h4("Predicted Happiness:"),
                              textOutput("predFam")
                            ),
                 
                         ),
                                        
                          
                            tabPanel("Health(Life Expectancy)",
                                     mainPanel(
                                       splitLayout(cellWidths = c("50%", "50%"), 
                                                   h4("Normality:",plotOutput("histHealth")),
                                                   h4("Linearity:",plotOutput("lineHealth"))
                                       ),
                                       h3("Homoscedasticity:"),
                                       h4("Residuals:"),
                                       splitLayout(cellWidths = c("20%", "20%", "20%", "20%", "20%"),
                                                   p("Min:", textOutput("min3")),
                                                   p("1Q:", textOutput("fQ3")),
                                                   p("Median:", textOutput("med3")),
                                                   p("3Q:", textOutput("tQ3")),
                                                   p("Max", textOutput("max3"))
                                       ),
                                       hr(),
                                       h4("Coefficients:"),
                                       splitLayout(cellWidths = c("20%", "20%", "20%", "20%", "20%"),
                                                   p("Predictors:", p("(Intercept)"), p("Health")),
                                                   p("Estimate:" ,p(textOutput("inter3")), p(textOutput("health"))),
                                                   p("Std. Error", p(textOutput("error3")), p(textOutput("healthE"))),
                                                   p("T-Value", p(textOutput("t3")), p(textOutput("healthT"))),
                                                   p("Pr(>|t|)", p(textOutput("pr3")), p(textOutput("healthPr")))
                                       ),
                                       hr(),
                                       h4("P-Value:"), 
                                       textOutput("pVal3"),
                                       hr()
                                     ),
                                     sidebarPanel(
                                       h3("Linear Regression Model (Health and Happiness):"),
                                       h5("Formula: Happiness = intercept + slope(health) +/- e"),
                                       h5("Predicted Formula: 3.376054 + 3.1474(health) +/- 0.4362989"),
                                       
                                       textInput('input3', 'Enter a Life Expectancy value:'),
                                       
                                       bsTooltip(id = "input3", title = "Input decimal value (Ex. 0.94 = 94 years old)", 
                                                 placement = "left", trigger = "hover"),
                                       h4("Predicted Happiness:"),
                                       textOutput("predHealth")
                                     ),
                                     
                                     ),
                            tabPanel("All Factors",
                                     mainPanel(
                                       splitLayout(cellWidths = c("50%", "50%"), 
                                                   h4("Normality:",plotOutput("histAll")),
                                                   h4("Linearity:",plotOutput("lineAll"))
                                       ),
                                       h3("Homoscedasticity:"),
                                       h4("Residuals:"),
                                       splitLayout(cellWidths = c("20%", "20%", "20%", "20%", "20%"),
                                                   p("Min:", textOutput("min4")),
                                                   p("1Q:", textOutput("fQ4")),
                                                   p("Median:", textOutput("med4")),
                                                   p("3Q:", textOutput("tQ4")),
                                                   p("Max", textOutput("max4"))
                                       ),
                                       hr(),
                                       h4("Coefficients:"),
                                       splitLayout(cellWidths = c("20%", "20%", "20%", "20%", "20%"),
                                                   p("Predictors:", p("(Intercept)"), p("GDP"), p("Social"), p("Health")),
                                                   p("Estimate:" ,textOutput("inter4"), p(textOutput("all")), p(textOutput("all2")), p(textOutput("all3"))),
                                                   p("Std. Error", textOutput("error4"), p(textOutput("allE")), p(textOutput("allE2")), p(textOutput("allE3"))),
                                                   p("T-Value", textOutput("t4"), p(textOutput("allT")), p(textOutput("allT2")), p(textOutput("allT3"))),
                                                   p("Pr(>|t|)", textOutput("pr4"), p(textOutput("allPr")), p(textOutput("allPr2")), p(textOutput("allPr3")))
                                       ),
                                       hr(),
                                       h4("P-Value:"), 
                                       textOutput("pVal4"),
                                       hr()
                                     ),
                                     sidebarPanel(
                                       h3("Linear Regression Model (GDP, Social, Health and Happiness):"),
                                       h5("Formula: Happiness = intercept + slope1(gdp) + 
                                          slope2(social) + slope3(health) +/- e"),
                                       h5("Predicted Formula: 2.856212 + 1.103911(gdp) + 0.6822363(social) + 1.101057(health) +/-
                                          0.4786595"),
                                       
                                       textInput('inputAll1', 'Enter a GDP per Capita value:'),
                                       
                                       bsTooltip(id = "inputAll1", title = "Input decimal value (Input decimal value (Ex. 1.0 = $60000))", 
                                                 placement = "left", trigger = "hover"),
                                       
                                       textInput('inputAll2', 'Enter a Social Support value:'),
                                       
                                       bsTooltip(id = "inputAll2", title = "Input decimal value (Input decimal value (Ex. 1.35))", 
                                                 placement = "left", trigger = "hover"),
                                       
                                       textInput('inputAll3', 'Enter a Life Expectancy value:'),
                                       
                                       bsTooltip(id = "inputAll3", title = "Input decimal value (Ex. 0.94 = 94 years old)", 
                                                 placement = "left", trigger = "hover"),
                                       
                                       h4("Predicted Happiness:"),
                                       textOutput("predAll1")
                                     ),
                            )
                   )
               ),
               tabPanel("Correlation",
                  navbarPage("Significance Testing",
                    tabPanel("Economy(GDP per Capita)",
                             sidebarLayout(
                               sidebarPanel(
                                 h3("Null Hypothesis:"),
                                 p("Factor GDP does not have any significance to the Happiness Score"),
                                 hr(),
                                 h3("Alternative Hypothesis:"),
                                 p("Factor GDP has significance to the Happiness Score")
                               ),
                               mainPanel(
                                 h3("Significance Testing (Pearson's Method of Correlation Coefficient)"),
                                 hr(),
                                 h4("T-Value:", textOutput("tValue1")),
                                 h4("Degree of Freedom:", textOutput("DoF1")),
                                 h4("P-Value:", textOutput("pValue1")),
                                 h4("95% Confidence interval:", textOutput("cInter1")),
                                 h4("Estimated Measure of Association (Correlation):", textOutput("esti1")),
                               )
                             )
                             ),
                    tabPanel("Family(Social Support)",
                             sidebarLayout(
                               sidebarPanel(
                                 h3("Null Hypothesis:"),
                                 p("Factor Family(Social Support) does not have any significance to the Happiness Score"),
                                 hr(),
                                 h3("Alternative Hypothesis:"),
                                 p("Factor Family(Social Support) has significance to the Happiness Score")
                               ),
                               mainPanel(
                                 h3("Significance Testing (Pearson's Method of Correlation Coefficient)"),
                                 hr(),
                                 h4("T-Value:", textOutput("tValue2")),
                                 h4("Degree of Freedom:", textOutput("DoF2")),
                                 h4("P-Value:", textOutput("pValue2")),
                                 h4("95% Confidence interval:", textOutput("cInter2")),
                                 h4("Estimated Measure of Association (Correlation):", textOutput("esti2")),
                               )
                             )
                             ),
                    tabPanel("Health(Life Expectancy)",
                             sidebarLayout(
                               sidebarPanel(
                                 h3("Null Hypothesis:"),
                                 p("Factor Health(Life Expectancy) does not have any significance to the Happiness Score"),
                                 hr(),
                                 h3("Alternative Hypothesis:"),
                                 p("Factor Health(Life Expectancy)  has significance to the Happiness Score")
                               ),
                               mainPanel(
                                 h3("Significance Testing (Pearson's Method of Correlation Coefficient)"),
                                 hr(),
                                 h4("T-Value:", textOutput("tValue3")),
                                 h4("Degree of Freedom:", textOutput("DoF3")),
                                 h4("P-Value:", textOutput("pValue3")),
                                 h4("95% Confidence interval:", textOutput("cInter3")),
                                 h4("Estimated Measure of Association (Correlation):", textOutput("esti3")),
                               )
                             )
                             )
                  )
               ),
               tabPanel("Graphs",
                        navbarPage("Line Graphs",
                          tabPanel("GDP per Capita",
                                   splitLayout(cellWidths = c("50%", "50%"),
                                               h3("2015", plotOutput("gdp2015"),align="center"),
                                               h3("2016", plotOutput("gdp2016"),align="center")
                                   ),
                                   splitLayout(cellWidths = c("50%", "50%"),
                                               h3("2017", plotOutput("gdp2017"),align="center"),
                                               h3("2018", plotOutput("gdp2018"),align="center")
                                   ),
                                   splitLayout(cellWidths = c("50%", "50%"),
                                               h3("2019", plotOutput("gdp2019"),align="center"),
                                               h3("2020", plotOutput("gdp2020"),align="center")
                                   ),
                                   splitLayout(cellWidths = c("50%", "50%"),
                                               h3("2021", plotOutput("gdp2021"),align="center"),
                                               h3("2022", plotOutput("gdp2022"),align="center")
                                   )
                          ),
                          tabPanel("Family (Social Support)",
                                   splitLayout(cellWidths = c("50%", "50%"),
                                               h3("2015", plotOutput("fam2015"),align="center"),
                                               h3("2016", plotOutput("fam2016"),align="center")
                                   ),
                                   splitLayout(cellWidths = c("50%", "50%"),
                                               h3("2017", plotOutput("fam2017"),align="center"),
                                               h3("2018", plotOutput("fam2018"),align="center")
                                   ),
                                   splitLayout(cellWidths = c("50%", "50%"),
                                               h3("2019", plotOutput("fam2019"),align="center"),
                                               h3("2020", plotOutput("fam2020"),align="center")
                                   ),
                                   splitLayout(cellWidths = c("50%", "50%"),
                                               h3("2021", plotOutput("fam2021"),align="center"),
                                               h3("2022", plotOutput("fam2022"),align="center")
                                   )
                          ),
                          tabPanel("Health (Life Expectancy)",
                                   splitLayout(cellWidths = c("50%", "50%"),
                                               h3("2015", plotOutput("health2015"),align="center"),
                                               h3("2016", plotOutput("health2016"),align="center")
                                   ),
                                   splitLayout(cellWidths = c("50%", "50%"),
                                               h3("2017", plotOutput("health2017"),align="center"),
                                               h3("2018", plotOutput("health2018"),align="center")
                                   ),
                                   splitLayout(cellWidths = c("50%", "50%"),
                                               h3("2019", plotOutput("health2019"),align="center"),
                                               h3("2020", plotOutput("health2020"),align="center")
                                   ),
                                   splitLayout(cellWidths = c("50%", "50%"),
                                               h3("2021", plotOutput("health2021"),align="center"),
                                               h3("2022", plotOutput("health2022"),align="center")
                                   )
                          )
                        )
                 
               ),
               tabPanel("Southeast Asia Happiness Map",
                        h3("Southeast Asia Happiness Score Map (2022)", align = "center"),
                        plotOutput("SAMap", width = "100%")
                        ),
               
               tabPanel("Table Data",
                    dataTableOutput("allTable")  
               )
    )
)

# Define server 
server <- function(input, output) {
  output$allTable <- renderDataTable(tableS, options = list(iDisplayLength = 10))
  
  output$histEcon <- renderPlot({
    ggplot() + geom_histogram(aes(happiness), bins = 5, colour="black", fill="#00BE67") +
    labs(x = "Happiness", y = "Frequency") + theme_light()
  })
  output$histFamily <- renderPlot({
    ggplot() + geom_histogram(aes(happiness), bins = 5, colour="black", fill="#00A9FF") +
      labs(x = "Happiness", y = "Frequency") + theme_light()
    
  })
  output$histHealth <- renderPlot({
    ggplot() + geom_histogram(aes(happiness), bins = 5, colour="black", fill="#F8766D") +
      labs(x = "Happiness", y = "Frequency") + theme_light()
    
  })
  output$histAll <- renderPlot({
    ggplot() + geom_histogram(aes(happiness), bins = 5, colour="black", fill="orange") +
      labs(x = "Happiness", y = "Frequency") + theme_light()
    
  })
  
  output$lineEcon <- renderPlot({
    data <- data.frame(gdp, happiness)
    ggplot(data, aes(x=gdp, y=happiness, group=1)) + geom_point() +
      geom_smooth(method="lm",se=FALSE, color="#00BE67") +
      labs(x = "GDP per Capita", y = "Happiness Score", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$lineFamily <- renderPlot({
    data <- data.frame(social, happiness)
    ggplot(data, aes(x=social, y=happiness, group=1)) + geom_point() +
      geom_smooth(method="lm",se=FALSE, color="#00A9FF") +
      labs(x = "Social Support", y = "Happiness Score", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$lineHealth <- renderPlot({
    data <- data.frame(health, happiness)
    ggplot(data, aes(x=health, y=happiness, group=1)) + geom_point() +
      geom_smooth(method="lm",se=FALSE, color="#F8766D") +
      labs(x = "Life Expectancy", y = "Happiness Score", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })

  output$lineAll <- renderPlot({
    data <- data.frame(gdp, social, health, happiness)
    ggplot(data, aes(x = "", y= happiness, group = 1)) + 
      geom_smooth(aes(x = gdp), method = "lm", se = FALSE, color = "#00BE67")+ 
      geom_smooth(aes(x = social), method = "lm", se = FALSE, color = "#00A9FF")+ 
      geom_smooth(aes(x = health), method = "lm", se = FALSE, color = "#F8766D")+
      theme_light()
  })
  
  # Start All
  
  happinessAll <- lm(happiness ~ gdp + social + health)
  allSum <- summary(happinessAll)
  
  output$min4 <- renderText({
    minimum <- min(allSum[["residuals"]]) 
  })
  output$fQ4 <- renderText({
    firstQ <- quantile(allSum[["residuals"]], 0.25)
  })
  output$med4 <- renderText({
    med <- median(allSum[["residuals"]])
  })
  output$tQ4 <- renderText({
    thirdQ <- quantile(allSum[["residuals"]], 0.75)
  })
  output$max4 <- renderText({
    maximum <- max(allSum[["residuals"]])
  })
  output$inter4 <- renderText({
    intercept <- allSum$coefficients["(Intercept)", "Estimate"]
  })
  output$all <- renderText({
    intercept <- allSum$coefficients["gdp", "Estimate"]
  })
  output$all2 <- renderText({
    intercept <- allSum$coefficients["social", "Estimate"]
  })
  output$all3 <- renderText({
    intercept <- allSum$coefficients["health", "Estimate"]
  })
  output$error4 <- renderText({
    error <- allSum$coefficients["(Intercept)", "Std. Error"]
  })
  output$allE <- renderText({
    error <- allSum$coefficients["gdp", "Std. Error"]
  })
  output$allE2 <- renderText({
    error <- allSum$coefficients["social", "Std. Error"]
  })
  output$allE3 <- renderText({
    error <- allSum$coefficients["health", "Std. Error"]
  })
  output$t4 <- renderText({
    t <- allSum$coefficients["(Intercept)", "t value"]
  })
  output$allT <- renderText({
    t <- allSum$coefficients["gdp", "t value"]
  })
  output$allT2 <- renderText({
    t <- allSum$coefficients["social", "t value"]
  })
  output$allT3 <- renderText({
    t <- allSum$coefficients["health", "t value"]
  })
  output$pr4 <- renderText({
    pr <- allSum$coefficients["(Intercept)", "Pr(>|t|)"]
  })
  output$allPr <- renderText({
    pr <- allSum$coefficients["gdp", "Pr(>|t|)"]
  })
  output$allPr2 <- renderText({
    pr <- allSum$coefficients["social", "Pr(>|t|)"]
  })
  output$allPr3 <- renderText({
    pr <- allSum$coefficients["health", "Pr(>|t|)"]
  })
  output$pVal4 <- renderText({
    pval <- lmp(happinessAll)
  })
  output$predAll1 <- renderText({
    gdp <- as.numeric(input$inputAll1)
    social <- as.numeric(input$inputAll2)
    health <- as.numeric(input$inputAll3)
    calc <- 2.856212 + 1.103911 *(gdp) + 0.6822363*(social) + 1.101057*(health) + 0.4786595
  })
  # End All
  
  
  #Start GDP
  
  happinessGdp <- lm(happiness ~ gdp)
  gdpSum <- summary(happinessGdp)
  
  output$min1 <- renderText({
      minimum <- min(gdpSum[["residuals"]]) 
  })
  output$fQ1 <- renderText({
      firstQ <- quantile(gdpSum[["residuals"]], 0.25)
  })
  output$med1 <- renderText({
    med <- median(gdpSum[["residuals"]])
  })
  output$tQ1 <- renderText({
    thirdQ <- quantile(gdpSum[["residuals"]], 0.75)
  })
  output$max1 <- renderText({
    maximum <- max(gdpSum[["residuals"]])
  })
  output$inter1 <- renderText({
    intercept <- gdpSum$coefficients["(Intercept)", "Estimate"]
  })
  output$gdp <- renderText({
    gdpSum$coefficients["gdp", "Estimate"]
  })
  output$error1 <- renderText({
    error <- gdpSum$coefficients["(Intercept)", "Std. Error"]
  })
  output$gdpE <- renderText({
    error <- gdpSum$coefficients["gdp", "Std. Error"]
  })
  output$t1 <- renderText({
    t <- gdpSum$coefficients["(Intercept)", "t value"]
  })
  output$gdpT <- renderText({
    t <- gdpSum$coefficients["gdp", "t value"]
  })
  output$pr1 <- renderText({
    pr <- gdpSum$coefficients["(Intercept)", "Pr(>|t|)"]
  })
  output$gdpPr <- renderText({
    pr <- gdpSum$coefficients["gdp", "Pr(>|t|)"]
  })
  output$pVal1 <- renderText({
    pval <- lmp(happinessGdp)
  })
  output$predGdp <- renderText({
      gdp <- as.numeric(input$input1)
      pred <- 3.5492 + 1.9992 * (gdp)  + 0.1772
  })
  # End GDP
  
  # Start Family(Social Support)
  
  happinessFam <- lm(happiness ~ social)
  famSum <- summary(happinessFam)
  
  output$min2 <- renderText({
    minimum <- min(famSum[["residuals"]]) 
  })
  output$fQ2 <- renderText({
    firstQ <- quantile(famSum[["residuals"]], 0.25)
  })
  output$med2 <- renderText({
    med <- median(famSum[["residuals"]])
  })
  output$tQ2 <- renderText({
    thirdQ <- quantile(famSum[["residuals"]], 0.75)
  })
  output$max2 <- renderText({
    maximum <- max(famSum[["residuals"]])
  })
  output$inter2 <- renderText({
    intercept <- famSum$coefficients["(Intercept)", "Estimate"]
  })
  output$fam <- renderText({
    famSum$coefficients["social", "Estimate"]
  })
  output$error2 <- renderText({
    error <- famSum$coefficients["(Intercept)", "Std. Error"]
  })
  output$famE <- renderText({
    error <- famSum$coefficients["social", "Std. Error"]
  })
  output$t2 <- renderText({
    t <- famSum$coefficients["(Intercept)", "t value"]
  })
  output$famT <- renderText({
    t <- famSum$coefficients["social", "t value"]
  })
  output$pr2 <- renderText({
    pr <- famSum$coefficients["(Intercept)", "Pr(>|t|)"]
  })
  output$famPr <- renderText({
    pr <- famSum$coefficients["social", "Pr(>|t|)"]
  })
  output$pVal2 <- renderText({
    pval <- lmp(happinessFam)
  })
  output$predFam <- renderText({
    fam <- as.numeric(input$input2)
    pred <- 3.5492 + 1.9992 * (fam)  + 0.1772
  })
  
  # End Family(Social Support)
  
  # Start Health
  
  happinessHealth <- lm(happiness ~ health)
  healthSum <- summary(happinessHealth)
  
  output$min3 <- renderText({
    minimum <- min(healthSum[["residuals"]]) 
  })
  output$fQ3 <- renderText({
    firstQ <- quantile(healthSum[["residuals"]], 0.25)
  })
  output$med3 <- renderText({
    med <- median(healthSum[["residuals"]])
  })
  output$tQ3 <- renderText({
    thirdQ <- quantile(healthSum[["residuals"]], 0.75)
  })
  output$max3 <- renderText({
    maximum <- max(healthSum[["residuals"]])
  })
  output$inter3 <- renderText({
    intercept <- healthSum$coefficients["(Intercept)", "Estimate"]
  })
  output$health <- renderText({
    healthSum$coefficients["health", "Estimate"]
  })
  output$error3 <- renderText({
    error <- healthSum$coefficients["(Intercept)", "Std. Error"]
  })
  output$healthE <- renderText({
    error <- healthSum$coefficients["health", "Std. Error"]
  })
  output$t3 <- renderText({
    t <- healthSum$coefficients["(Intercept)", "t value"]
  })
  output$healthT <- renderText({
    t <- healthSum$coefficients["health", "t value"]
  })
  output$pr3 <- renderText({
    pr <- healthSum$coefficients["(Intercept)", "Pr(>|t|)"]
  })
  output$healthPr <- renderText({
    pr <- healthSum$coefficients["health", "Pr(>|t|)"]
  })
  output$pVal3 <- renderText({
    pval <- lmp(happinessHealth)
  })
  output$predHealth <- renderText({
    health <- as.numeric(input$input3)
    pred <- 3.5492 + 1.9992 * (health)  + 0.1772
  })
  
  # End Health
  
  # Start GDP Correlation
  test <- cor.test(gdp, happiness)
  
  output$tValue1 <- renderText({
    print(test[["statistic"]])
  })
  output$DoF1 <- renderText({
    print(test[["parameter"]])
  })
  output$pValue1 <- renderText({
    print(test[["p.value"]])
  })
  output$cInter1 <- renderText({
    print(test[["conf.int"]])
  })
  output$esti1 <- renderText({   
    print(test[["estimate"]])
  })
  
  # End GDP Correlation
  
  # Start Social Support Correlation
  
  test2 <- cor.test(social, happiness)
  
  output$tValue2 <- renderText({
    print(test2[["statistic"]])
  })
  output$DoF2 <- renderText({
    print(test2[["parameter"]])
  })
  output$pValue2 <- renderText({
    print(test2[["p.value"]])
  })
  output$cInter2 <- renderText({
    print(test2[["conf.int"]])
  })
  output$esti2 <- renderText({   
    print(test2[["estimate"]])
  })
  
  # End Social Support Correlation
  
  # Start Health Correlation
  
  test3 <- cor.test(health, happiness)
  
  output$tValue3 <- renderText({
    print(test3[["statistic"]])
  })
  output$DoF3 <- renderText({
    print(test3[["parameter"]])
  })
  output$pValue3 <- renderText({
    print(test3[["p.value"]])
  })
  output$cInter3 <- renderText({
    print(test3[["conf.int"]])
  })
  output$esti3 <- renderText({   
    print(test3[["estimate"]])
  })
  
  # End Health Correlation
  
  # Start GDP Line Graphs 2015-2022
  
  output$gdp2015 <- renderPlot({
    data <- data.frame(gdp, country, year)
    data <- data[which(data$year == "2015"),]
    ggplot(data, aes(x=country, y=gdp, group=1)) + 
      geom_line(color="#00BE67", size = 1.2)+ geom_point() +
      labs(x = "Southeast Asian Countries", y = "GDP per Capita", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$gdp2016 <- renderPlot({
    data <- data.frame(gdp, country, year)
    data <- data[which(data$year == "2016"),]
    ggplot(data, aes(x=country, y=gdp, group=1)) + 
    geom_line(color="#00BE67", size = 1.2)+ geom_point() +
    labs(x = "Southeast Asian Countries", y = "GDP per Capita", title="") +
    theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$gdp2017 <- renderPlot({
    data <- data.frame(gdp, country, year)
    data <- data[which(data$year == "2017"),]
    ggplot(data, aes(x=country, y=gdp, group=1)) + 
      geom_line(color="#00BE67", size = 1.2)+ geom_point() +
      labs(x = "Southeast Asian Countries", y = "GDP per Capita", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$gdp2018 <- renderPlot({
    data <- data.frame(gdp, country, year)
    data <- data[which(data$year == "2018"),]
    ggplot(data, aes(x=country, y=gdp, group=1)) + 
      geom_line(color="#00BE67", size = 1.2)+ geom_point() +
      labs(x = "Southeast Asian Countries", y = "GDP per Capita", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  
  output$gdp2019 <- renderPlot({
    data <- data.frame(gdp, country, year)
    data <- data[which(data$year == "2019"),]
    ggplot(data, aes(x=country, y=gdp, group=1)) + 
      geom_line(color="#00BE67", size = 1.2)+ geom_point() +
      labs(x = "Southeast Asian Countries", y = "GDP per Capita", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$gdp2020 <- renderPlot({
    data <- data.frame(Economy = c2020$Economy, Country = c2020$Country, Year = c2020$Year)
    data <- data[which(data$Year == "2020"),]
    ggplot(data, aes(x=Country, y=Economy, group=1)) + 
      geom_line(color="#00BE67", size = 1.2)+ geom_point() +
      labs(x = "Southeast Asian Countries", y = "GDP per Capita", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$gdp2021 <- renderPlot({
    data <- data.frame(Economy = c2021$Economy, Country = c2021$Country, Year = c2021$Year)
    data <- data[which(data$Year == "2021"),]
    ggplot(data, aes(x=Country, y=Economy, group=1)) + 
      geom_line(color="#00BE67", size = 1.2)+ geom_point() +
      labs(x = "Southeast Asian Countries", y = "GDP per Capita", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$gdp2022 <- renderPlot({
    data <- data.frame(gdp, country, year)
    data <- data[which(data$year == "2022"),]
    ggplot(data, aes(x=country, y=gdp, group=1)) + 
      geom_line(color="#00BE67", size = 1.2)+ geom_point() +
      labs(x = "Southeast Asian Countries", y = "GDP per Capita", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  
  
  # End GDP Line Graphs 2015-2022
  
  # Start Family Line Graphs 2015-2022
  
  output$fam2015 <- renderPlot({
    data <- data.frame(social, country, year)
    data <- data[which(data$year == "2015"),]
    ggplot(data, aes(x=country, y=social, group=1)) + 
      geom_line(color = "#00A9FF", size = 1.2)+geom_point() +
      labs(x = "Southeast Asian Countries", y = "Social Support", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$fam2016 <- renderPlot({
    data <- data.frame(social, country, year)
    data <- data[which(data$year == "2016"),]
    ggplot(data, aes(x=country, y=social, group=1)) + 
      geom_line(color = "#00A9FF", size = 1.2)+geom_point() +
      labs(x = "Southeast Asian Countries", y = "Social Support", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$fam2017 <- renderPlot({
    data <- data.frame(social, country, year)
    data <- data[which(data$year == "2017"),]
    ggplot(data, aes(x=country, y=social, group=1)) + 
      geom_line(color = "#00A9FF", size = 1.2)+geom_point() +
      labs(x = "Southeast Asian Countries", y = "Social Support", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$fam2018 <- renderPlot({
    data <- data.frame(social, country, year)
    data <- data[which(data$year == "2018"),]
    ggplot(data, aes(x=country, y=social, group=1)) +
      geom_line(color = "#00A9FF", size = 1.2)+ geom_point() +
      labs(x = "Southeast Asian Countries", y = "Social Support", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  
  output$fam2019 <- renderPlot({
    data <- data.frame(social, country, year)
    data <- data[which(data$year == "2019"),]
    ggplot(data, aes(x=country, y=social, group=1)) + 
      geom_line(color = "#00A9FF", size = 1.2)+geom_point() +
      labs(x = "Southeast Asian Countries", y = "Social Support", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$fam2020 <- renderPlot({
    data <- data.frame(Social = c2020$Family, Country = c2020$Country, Year = c2020$Year)
    data <- data[which(data$Year == "2020"),]
    ggplot(data, aes(x=Country, y=Social, group=1)) + 
      geom_line(color = "#00A9FF", size = 1.2)+geom_point() +
      labs(x = "Southeast Asian Countries", y = "Social Support", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$fam2021 <- renderPlot({
    data <- data.frame(Social = c2021$Family, Country = c2021$Country, Year = c2021$Year)
    data <- data[which(data$Year == "2021"),]
    ggplot(data, aes(x=Country, y=Social, group=1)) + 
      geom_line(color = "#00A9FF", size = 1.2)+geom_point() +
      labs(x = "Southeast Asian Countries", y = "Social Support", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$fam2022 <- renderPlot({
    data <- data.frame(social, country, year)
    data <- data[which(data$year == "2022"),]
    ggplot(data, aes(x=country, y=social, group=1)) + 
      geom_line(color = "#00A9FF", size = 1.2)+geom_point() +
      labs(x = "Southeast Asian Countries", y = "Social Support", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  
  # End Family Line Graphs 2015-2022
  
  # Start Health Line Graphs 2015-2022
  
  output$health2015 <- renderPlot({
    data <- data.frame(health, country, year)
    data <- data[which(data$year == "2015"),]
    ggplot(data, aes(x=country, y=health, group=1)) + 
      geom_line(color = "#F8766D", size = 1.2)+geom_point() +
      labs(x = "Southeast Asian Countries", y = "Life Expectancy", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$health2016 <- renderPlot({
    data <- data.frame(health, country, year)
    data <- data[which(data$year == "2016"),]
    ggplot(data, aes(x=country, y=health, group=1)) + 
      geom_line(color = "#F8766D", size = 1.2)+geom_point() +
      labs(x = "Southeast Asian Countries", y = "Life Expectancy", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$health2017 <- renderPlot({
    data <- data.frame(health, country, year)
    data <- data[which(data$year == "2017"),]
    ggplot(data, aes(x=country, y=health, group=1)) + 
      geom_line(color = "#F8766D", size = 1.2)+geom_point() +
      labs(x = "Southeast Asian Countries", y = "Life Expectancy", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$health2018 <- renderPlot({
    data <- data.frame(health, country, year)
    data <- data[which(data$year == "2018"),]
    ggplot(data, aes(x=country, y=health, group=1)) + 
      geom_line(color = "#F8766D", size = 1.2)+geom_point() +
      labs(x = "Southeast Asian Countries", y = "Life Expectancy", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  
  output$health2019 <- renderPlot({
    data <- data.frame(health, country, year)
    data <- data[which(data$year == "2019"),]
    ggplot(data, aes(x=country, y=health, group=1)) + 
      geom_line(color = "#F8766D", size = 1.2)+geom_point() +
      labs(x = "Southeast Asian Countries", y = "Life Expectancy", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$health2020 <- renderPlot({
    data <- data.frame(Health = c2020$Health, Country = c2020$Country, Year = c2020$Year)
    data <- data[which(data$Year == "2020"),]
    ggplot(data, aes(x=Country, y=Health, group=1)) + 
      geom_line(color = "#F8766D", size = 1.2)+geom_point() +
      labs(x = "Southeast Asian Countries", y = "Life Expectancy", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$health2021 <- renderPlot({
    data <- data.frame(Health = c2021$Health, Country = c2021$Country, Year = c2021$Year)
    data <- data[which(data$Year == "2021"),]
    ggplot(data, aes(x=Country, y=Health, group=1)) + 
      geom_line(color = "#F8766D", size = 1.2)+geom_point() +
      labs(x = "Southeast Asian Countries", y = "Life Expectancy", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  output$health2022 <- renderPlot({
    data <- data.frame(health, country, year)
    data <- data[which(data$year == "2022"),]
    ggplot(data, aes(x=country, y=health, group=1)) + 
      geom_line(color = "#F8766D", size = 1.2)+geom_point() +
      labs(x = "Southeast Asian Countries", y = "Life Expectancy", title="") +
      theme(plot.title = element_text(hjust=0.5, size=20, face='bold')) + theme_light()
  })
  
  # End Health Line Graphs 2015-2022
  
  output$SAMap <- renderPlot({
    southEast <- data[which(data$Region == "Southeastern Asia"),]
    southEast <- southEast[which(southEast$Year == "2022"),]
    sData <- data.frame(happiness = southEast$Happiness.Score, region = southEast$Country)
    
    mapData <- map_data("world")
    
    mapData <- left_join(mapData, sData, by ="region")
    mapData1 <- mapData %>% filter(!is.na(mapData$happiness))
    view(mapData1)
    labs <- sData$region
    
    regions <- c("Cambodia", "Indonesia", "Singapore", "Thailand", "Myanmar", "Malaysia", "Vietnam", "Laos", "Philippines")
    longLabels <- c(105, 122, 103, 101, 95, 103, 106, 102, 123)
    latLabels <- c(13, -4, -1, 15, 22, 5, 10, 20, 15)
    group <- c(908, 692, 1349, 1404, 995, 1032, 1589, 946, 1125)
    labeldata <- data.frame(regions, longLabels, latLabels, group)

    map1 <- ggplot(mapData1, aes(x = long, y = lat, group = group)) + 
      geom_polygon(aes(fill = happiness), color = "black") + 
      geom_text(data = labeldata, aes(x = longLabels, y = latLabels, label = regions), color = "black")
    
    map1
  
  })
}

# Function to get the p-value of regression
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# Run the application 
shinyApp(ui, server)
