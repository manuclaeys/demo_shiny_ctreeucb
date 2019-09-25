library(DT)
library(gdata)
library(data.table)
library(leaflet)
library(ggplot2)
library(dplyr)
library(magrittr)
library(lubridate)
library(readr)
#https://thedatalab.shinyapps.io/TrafficAccidents/

options(shiny.maxRequestSize=10000000000*1024^2) #pour les data du grande taille


server <- shinyServer(function(input, output, session) {

  ##################### Importer la base ################
 
  library(bandit4abtest)
    dat <- abtest1
    
    
 
  output$cont <- renderDataTable({
    
    head(dat, n = input$obs)
  })
  
  ########### fonction Time #################
  
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("The current time is", Sys.time())
  })
  
  
  ##############INPUTS###############
  ####Cartography###
  output$mapVars = renderUI(
    awesomeCheckboxGroup(inputId = "mapVars", 
                         label = "Choose type of investigation", choices =  c("A", "B"), selected = "A", 
                         inline = TRUE, status = "warning")
  )
  
  output$Device = renderUI(
    selectInput('Country',
                'Choose a device',
                c(levels(as.factor(dat$device)),"All"),
                "All")
  )
  
  output$UserAgent = renderUI(
    selectInput('Country',
                'Choose a User Agent',
                c(levels(as.factor(dat$userAgent)),"All"),
                "All")
  )
  
  output$Name= renderUI(
    selectInput('Country',
                'Choose a O.S.',
                c(levels(as.factor(dat$name)),"All"),
                "All")
  )
  
  ############### modifier notre base selon "type of investigation" ###############
  miniBase<-reactive({
   
   # d<- subset(dat,dat[,Investigation.Type] %in% c(input$mapVars))
    d <- dat %>% filter(Investigation.Type == input$mapVars)
   
    return(d)
    
  })
  
  #############################data explorer#################
  
  abtest_data <- dat

  abtest_data$A <- as.factor(abtest_data$A)
  abtest_data$B <- as.factor(abtest_data$B)
  
  # Filter data, returning a data frame
  getabtest  <- reactive({
    f <- abtest_data
    
    if (input$purposeFlight != "All") {
      f <- f %>% filter(Purpose.of.Flight == input$purposeFlight)
    }
    if (input$aircraftDamage != "All") {
      f <- f %>% filter(Aircraft.Damage == input$aircraftDamage)
    }
    if (input$reportStatus != "All") {
      f <- f %>% filter(Report.Status == input$reportStatus)
    }
    if (input$Country!= "All" ) {
      f <- f %>% filter(Country == input$Country)
    }
    if (input$aircraftCategory != "All") {
      f <- f %>% filter(Aircraft.Category == input$aircraftCategory)
    }
    
    return(f)
  })
  
  output$plot1 <- renderPlot({
    #myabtest_data   <- getabtest()
    myabtest_data   <-  myabtest_data 
    
    require(gridExtra)
    
    pie1 <- ggplot(myabtest_data, aes(x = "", fill = factor(device))) + 
      geom_bar(width = 1) +
      theme(axis.line = element_blank(), 
            plot.title = element_text(hjust=0.5)) + 
      labs(fill="class", 
           x=NULL, 
           y=NULL, 
           title="Pie Chart of device", 
           caption="Source: mpg") +
      coord_polar(theta = "y", start=0)
    
    pie2 <- ggplot(myabtest_data, aes(x = "", fill = factor(name))) + 
      geom_bar(width = 1) +
      theme(axis.line = element_blank(), 
            plot.title = element_text(hjust=0.5)) + 
      labs(fill="class", 
           x=NULL, 
           y=NULL, 
           title="Pie Chart of O.S.", 
           caption="Source: mpg") +
      coord_polar(theta = "y", start=0)
    
    
    pie3 <- ggplot(myabtest_data, aes(x = "", fill = factor(userAgent))) + 
      geom_bar(width = 1) +
      theme(axis.line = element_blank(), 
            plot.title = element_text(hjust=0.5)) + 
      labs(fill="class", 
           x=NULL, 
           y=NULL, 
           title="Pie Chart of U.A.", 
           caption="Source: mpg") +
      coord_polar(theta = "y", start=0)
    
    
    grid.arrange(pie1, pie2, pie3, ncol=3)
    
    
  })

 
})