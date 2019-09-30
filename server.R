library(DT)
library(gdata)
library(data.table)
library(leaflet)
library(ggplot2)
library(dplyr)
library(magrittr)
library(lubridate)
library(readr)
library(bandit4abtest)
library(partykit)
#https://thedatalab.shinyapps.io/TrafficAccidents/

options(shiny.maxRequestSize=10000000000*1024^2) #pour les data du grande taille


server <- shinyServer(function(input, output, session) {

  ##################### Importer la base ################
 
  
    dat <- abtest2
    
    abtest_data <- dat
    
    col_prediction <- function(){
      data_ab <- abtest_data
      data_ab$variation <- NA
      data_ab[  is.na(data_ab$A) , "variation"   ] <- "B"
      data_ab[  is.na(data_ab$B) , "variation"   ] <- "A"
      data_ab[  is.na(data_ab$A) , "A"   ] <- 0
      data_ab[  is.na(data_ab$B) , "B"   ] <- 0
      data_ab[ , "variation"] <- as.factor(data_ab[ , "variation"] )
      data_ab  <-   data_ab[ is.na(data_ab$variation) ==FALSE,] 
      data_ab$action <- as.factor(as.integer(as.character(data_ab$A)) + as.integer(as.character(data_ab$B)))
      return(data_ab)
    }
    

 
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
                         label = "Choose type of investigation", choices =  c("1", "0"), selected = "1", 
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
   
    myabtest_data   <-  col_prediction()
    myabtest_data  <-   myabtest_data  %>% filter(myabtest_data$action == input$mapVars)
    
   # d <- subset(dat,dat[,Investigation.Type] %in% c(input$mapVars))
  #  d <- dat %>% filter(Investigation.Type == input$mapVars)
   
    return( myabtest_data)
    
  })
  
  
  ##############Map function##################
  
  output$mymap <- renderLeaflet({
    #base  <-  col_prediction()
    base<- miniBase()
    leaflet(data = base) %>% addTiles() %>%
    #  addMarkers(~longitude, ~latitude)
      addMarkers(~longitude, ~latitude, popup = ~as.character(countryID), label = ~as.character(countryID))
    leaflet(data = base) %>% addTiles() %>% addMarkers(
      clusterOptions = markerClusterOptions())
    
  })

  
  #############################data explorer#################
  

  
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
    #abtest_data <- col_prediction()
    
    #abtest_data$A <- as.factor(abtest_data$A)
    #abtest_data$B <- as.factor(abtest_data$B)
    
    
    myabtest_data   <-  col_prediction()
    
    require(gridExtra)
    library(wesanderson)
    
    pie1 <- ggplot(myabtest_data, aes(x = "", fill = factor(device))) + 
      geom_bar(width = 1) +
      theme(
        plot.margin = margin(2,.8,2,.8, "cm"),
        axis.line = element_blank(), 
            plot.title = element_text(hjust=0.5)) + 
      labs(fill="class", 
           x=NULL, 
           y=NULL, 
           title="Pie Chart of device", 
           caption="Source: mpg") +
      coord_polar(theta = "y", start=0)+
      scale_fill_brewer(palette = "Reds")
    
    pie2 <- ggplot(myabtest_data, aes(x = "", fill = factor(name)))  + 
      geom_bar(width = 1) +
      theme(
        plot.margin = margin(2,.8,2,.8, "cm"),
        axis.line = element_blank(), 
            plot.title = element_text(hjust=0.5)) + 
      labs(fill="class", 
           x=NULL, 
           y=NULL, 
           title="Pie Chart of O.S.")+ 
          # caption="Source: mpg") +
      coord_polar(theta = "y", start=0) +
      scale_fill_brewer(palette = "Blues")
     # scale_fill_manual(values=wes_palette(n=length(levels(as.factor(myabtest_data$name))), name="GrandBudapest1"))
    
    
    pie3 <- ggplot(myabtest_data, aes(x = "", fill = factor(userAgent)))  + 
      geom_bar(width = 1) +
      theme(
        plot.margin = margin(2,.8,2,.8, "cm"),
        axis.line = element_blank(), 
            plot.title = element_text(hjust=0.5)) + 
      labs(fill="class", 
           x=NULL, 
           y=NULL, 
           title="Pie Chart of U.A.", 
           caption="Source: mpg") +
      coord_polar(theta = "y", start=0)+
      scale_fill_brewer(palette = "Greens")
    
    
    grid.arrange(pie1, pie2, pie3, ncol=3)
    
    
  })
  
  output$plot2 <- renderPlot({

    plot(x=1:10,y=1:10)
    
  })

  
  
  
  
  output$plot4 <- renderPlot({
    
    library(bayesAB)
    data_for_tree <- col_prediction()
    x <- as.numeric(as.character(   data_for_tree[ data_for_tree$variation == "A" , "action"]))
    y <- as.numeric(as.character( data_for_tree[ data_for_tree$variation == "B" , "action"]))
    test1 <- bayesTest(x, y, distribution = "bernoulli", priors = c("alpha" = 10, "beta" = 10))
    #    print(test1)
     temp <- plot(test1)
      temp$posteriors
  #  plot(x=1:10,y=1:10)
    
  })
  
  
  
  
  
  
  output$plot3 <- renderPlot({

    # Variation #
    data_for_tree <- col_prediction()
    
    my_formula <- as.formula("action ~ name + device + userAgent + variation")
    
    ### classification
    my_tree  <- ctree(my_formula,data = data_for_tree)
    
    plot(my_tree  )
    
  })
  
  

  
 
})