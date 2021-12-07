# Sergio Martelo
# STA-230: Intro to Data Science
# Macro Daashboard

#Libraries
library(rsconnect)
library(treemap)
library(shinythemes)
library(shiny)
library(tidyverse)
library(sp)
library(leaflet)    
library(geojsonio)  
library(htmltools)  
#Data 
POP_GDP <- data.frame(read.csv("POP_GDP.csv"))

## jSON files
shapeurl <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
WorldCountry <- geojson_read(shapeurl, what = "sp")
WorldCountry@data <- POP_GDP

# Define UI for application 
ui <- navbarPage(theme = shinytheme("lumen"), 
    # Application title
    "Macro Dashboard",
    # World Panel
    tabPanel("World",
             fillPage(leafletOutput("mapt1")),
             absolutePanel( #Options panel
                 id = "controls", class = "panel panel-default",
                 style="padding: 12px",
                 bottom = 155, left = 25, width = 225, fixed=TRUE,
                 draggable = TRUE, height = 225,
                 radioButtons("radio", label = h3("Options"), #Radio Buttons
                             choices = list("Gross Domestic Product(GDP)" = "GDP_Value", 
                                            "Population" = "POP_Value",
                                            "Income Groups" = "Income Group",
                                            "Regions" = "Region"), selected = "GDP_Value"),
                 checkboxInput("log", label = "Log Scale", value = FALSE)) #Log Scale input
             ),
    #Individual Country Statistics 
     tabPanel("Countries",
              sidebarPanel( #Side Panel
                  selectInput("select1", label = h3("Select Country 1"), #Select Country 1
                               choices = WorldCountry@data$name),
                  selectInput("select2", label = h3("Select Country 2"), #Select Country 2
                              choices = WorldCountry@data$name),
                  selectInput("select3", label = h3("Distribution"), # Select for treemap distribution
                              choices = c("Top 1 Percent" = "top1per",
                                          "Top 10 Percent" = "top10per.x",
                                          "Middle 40 Percent" = "mid40per",
                                          "Bottom 50 Percent" = "bot50per"))
              ),
              mainPanel(fluidRow(# Output Panel
                  column(6,
                         plotOutput("compGraph1",height = "300px"), # GDP Bar plot
                         plotOutput("treemap1", height = "200px")), # Treemap for country 1 distribution
                  column(6,
                         plotOutput("compGraph2",height = "300px"), # Population Bar plot
                         plotOutput("treemap2", height = "200px")) # Treemap for country 2 distribution
              ))),

    
    #Customization inputs
    inverse = TRUE,
    fluid = TRUE
)

# server logic
server <- function(input, output) {
    
    ## Map Functions
    
    
    #Label
    myLabels <- paste("<strong>", WorldCountry@data$name, "</strong>", "<br/>",
                      "<em> Population </em>", WorldCountry@data$POP_Value, "<br/>",
                      "<em> Gross Domestic Product </em>", WorldCountry@data$GDP_Value)
    
    
    
    ## Map
    # Base map
    output$mapt1 <- renderLeaflet({
        
        leaflet(WorldCountry) %>% 
            addTiles(options = providerTileOptions(minZoom = 0.75, maxZoom = 10)) %>% 
            setMaxBounds(180,180,-180,-180)
        
    })
    
    #Reactive elements
    observe({
        
        ## Popup Management
        if(input$radio == "GDP_Value"){
            myPopups <- paste("Rank", WorldCountry@data$GDP_Ranking)
        } else if(input$radio == "POP_Value"){
            myPopups <- paste("Rank", WorldCountry@data$POP_Ranking)
        } else if (input$radio == "Region"){
            myPopups <- paste(WorldCountry@data$Region)
        } else if (input$radio == "Income Group"){
            myPopups <- paste(WorldCountry@data$Income_Group)
        }
        
        ## Color and title Management
        if(input$radio == "GDP_Value" & !input$log){
            pal <- colorNumeric(
                palette = "Blues",
                domain = WorldCountry@data$GDP_Value)
    
            palVal <- WorldCountry@data$GDP_Value
            title_ <- "GDP (in thousands)"
            
        } else if(input$radio == "POP_Value" & !input$log){
            pal <- colorNumeric(
                palette = "Reds",
                domain = WorldCountry@data$POP_Value)
            
            palVal <- WorldCountry@data$POP_Value
            title_ <- "Population (in thousands)"
            
        } else if(input$radio == "GDP_Value"){
            pal <- colorNumeric(
                palette = "Blues",
                domain = log10(WorldCountry@data$GDP_Value))
            
            palVal <- log10(WorldCountry@data$GDP_Value)
            title_ <- "Log Base 10 of GDP (in thousands)"
            
        } else if(input$radio == "POP_Value"){
            pal <- colorNumeric(
                palette = "Reds",
                domain = log10(WorldCountry@data$POP_Value))
            
            palVal <- log10(WorldCountry@data$POP_Value)
            title_ <- "Log Base 10 of Population (in thousands)"
            
        } else if(input$radio == "Region"){
            pal <- colorFactor(topo.colors(7), WorldCountry@data$Region)
            
            palVal <- WorldCountry@data$Region
            title_ <- input$radio
            
        } else if(input$radio == "Income Group"){
            pal <- colorFactor(topo.colors(4), WorldCountry@data$Income_Group)
    
            palVal <- WorldCountry@data$Income_Group
            title_ <- input$radio
            
        }
        
        
        ## Draws polygons, labels, and colors the map
        leafletProxy("mapt1", data = WorldCountry) %>% addPolygons(
            data = WorldCountry,
            fillColor = ~pal(palVal),
            weight = 2,
            opacity = 1,
            color = "white",
            fillOpacity = 0.7,
            highlight = highlightOptions(
                weight = 3,
                color = "grey",
                fillOpacity = 0.7,
                bringToFront = TRUE),
            label = lapply(myLabels, HTML),
            popup = myPopups) %>%
            clearControls() %>% 
            addLegend("bottomright", pal = pal,
                      values = ~palVal,
                      title = title_,
                      labFormat = labelFormat(prefix = " "),
                      opacity = 1)
            
        
    })
    
    ##Comparison graphs
    
    # Draws GDP graph
    output$compGraph1 <- renderPlot({
        # Defining country variables
        Country1 <- input$select1
        Country2 <- input$select2
        
        # Getting subset of data
        tempData <- POP_GDP %>% 
            filter(name == Country1 | name == Country2)
        
        #Draws plot
        ggplot(tempData, 
               aes(x= name, y = POP_Value)) + 
            geom_col(fill = "lightBlue") +
            labs(title ="2020 Population",
                 x = "",
                 y = "Population (in thousands)")
        
    }) 

    # Draw Population graph
    output$compGraph2 <- renderPlot({
        #Definind=g country variables
        Country1 <- input$select1
        Country2 <- input$select2
        
        #Getting subset of data
        tempData <- POP_GDP %>% 
            filter(name == Country1 | name == Country2)
        
        #Draws plot
        ggplot(tempData, 
               aes(x= name,y = GDP_Value)) + 
            geom_col(fill = "lightBlue") +
            labs(title = "2020 Gross Domestic Product",
                 x = "",
                 y = "Dollars (in thousands)")
        
    })

    
    
    # Treemap of distribution for country 1
    output$treemap1 <- renderPlot({
        #Defining country variable
        country <- input$select1
        
        #Getting subset of data
        tempData <- POP_GDP %>% 
            filter(name == country)
        
        ## Distribution Choice Management
        
        if(input$select3 == "top10per.x"){
            lab <- "Top 10 Percent"
            share <- as.numeric(tempData$top10per.x)
        } else if(input$select3 == "top1per"){
            lab <- "Top 1 Percent"
            share <- as.numeric(tempData$top1per)
        } else if(input$select3 == "bot50per"){
            lab <- "Bottom 50 Percent"
            share <- as.numeric(tempData$bot50per)
        } else if(input$select3 == "mid40per"){
            lab <- "Middle 40 Percent"
            share <- as.numeric(tempData$mid40per)
        }
        
        # Creating data for treemap
        group <- c(lab,"Rest of the Population")
        value <- c(share, (1 - share))
        data <- data.frame(group,value)
        title_ <- paste("Distribution of Wealth for", input$select1)
        
        # treemap
        if(!is.na(data$value[1])){
            treemap(data,
                    index="group",
                    vSize="value",
                    type="index",
                    palette = "Set1",
                    title = title_
            ) 
        } 
        
        
    })
    
    output$treemap2 <- renderPlot({
        #Defining country variable
        country <- input$select2
        
        #Getting subset of data
        tempData <- POP_GDP %>% 
            filter(name == country)
        
        ## Distribution Choice Management
        
        if(input$select3 == "top10per.x"){
            lab <- "Top 10 Percent"
            share <- as.numeric(tempData$top10per.x)
        } else if(input$select3 == "top1per"){
            lab <- "Top 1 Percent"
            share <- as.numeric(tempData$top1per)
        } else if(input$select3 == "bot50per"){
            lab <- "Bottom 50 Percent"
            share <- as.numeric(tempData$bot50per)
        } else if(input$select3 == "mid40per"){
            lab <- "Middle 40 Percent"
            share <- as.numeric(tempData$mid40per)
        }
        
        # Creating data for treemap
        group <- c(lab,"Rest of the Population")
        value <- c(share, (1 - share))
        data <- data.frame(group,value)
        title_ <- paste("Distribution of Wealth for", input$select2)
        
        # treemap
        if(!is.na(data$value[1])){
            treemap(data,
                    index="group",
                    vSize="value",
                    type="index",
                    palette = "Set1",
                    title = title_
            ) 
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
