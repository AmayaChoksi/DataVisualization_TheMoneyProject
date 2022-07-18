library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(DT)
library(plotly)
library(billboarder)

data <- read_csv("billionaires.csv")
all_indus = data %>%
  distinct(Industry) %>%
  pull(Industry)


ui <-
  dashboardPage(
    dashboardHeader(title = "The Money Project"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("About", tabName = "page1", icon = icon("briefcase")),
        menuItem(
          "Wealth of Top 10 Billionaires",
          tabName = "page2",
          icon = icon("bar-chart")
        ),
        menuItem(
          "Billionaires by Industry",
          tabName = "page3",
          icon = icon("industry")
        ),
        menuItem(
          "Age and Net-worth",
          tabName = "page4",
          icon = icon("dollar-sign")
        ),
        menuItem("Map", tabName = "page5", icon = icon("map-o")),
        menuItem("Data", tabName = "page6", icon = icon("table"))
      )
    ),
    skin = "green",
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "page1",
          fluidRow(
            box(
              title = "The Richest People in the World",
              solidHeader = TRUE,
              status = "success",
              width = 12,
              collapsible = TRUE,
              column(12,
                     tags$div(
                       tags$span(
                         "The world’s 2,600 or so billionaires covered in this dataset have more wealth than the
                                4.6 billion people who make up 60 percent of the planet’s population. In this project we will exlpore
                                in which industries they accumulated their wealth, how age plays a role in wealth accumulation and the geospatial
                                distribution of world's richest people.",
                         style = "font-size:14px"
                       ),
                     ))
            ),
            box(
              width = 4,
              img(
                src = "ElonMusk.png",
                width = "100%",
                height = "100%",
                tags$div("Elon Musk", align = "center")
              )
            ),
            box(
              width = 4,
              img(
                src = "JeffBezos.png",
                width = "100%",
                height = "100%",
                tags$div("Jeff Bezos", align = "center")
              )
            ),
            box(
              width = 4,
              img(
                src = "BernardArnault.png",
                width = "100%",
                height = "100%",
                tags$div("Bernard Arnault", align = "center")
              )
            ),
            box(
              width = 4,
              img(
                src = "BillGates.png",
                width = "100%",
                height = "100%",
                tags$div("Bill Gates", align = "center")
              )
            ),
            box(
              width = 4,
              img(
                src = "WarrenBuffet.png",
                width = "100%",
                height = "100%",
                tags$div("Warren Buffet", align = "center")
              )
            ),
            box(
              width = 4,
              img(
                src = "SergeyBrin.png",
                width = "100%",
                height = "100%",
                tags$div("Sergey Brin", align = "center")
              )
            ),
            box(
              width = 4,
              img(
                src = "LarryPage.png",
                width = "100%",
                height = "100%",
                tags$div("Larry Page", align = "center")
              )
            ),
            box(
              width = 4,
              img(
                src = "MukeshAmbani.png",
                width = "100%",
                height = "100%",
                tags$div("Mukesh Ambani", align = "center")
              )
            ),
            box(
              width = 4,
              img(
                src = "MikeBloomberg.png",
                width = "100%",
                height = "100%",
                tags$div("Michael Bloomberg", align = "center")
              )
            ),
          )
        ),
        
        tabItem(
          tabName = "page2",
          tags$h1("Top 10, Who?", align = "center"),
          plotOutput("plot1"),
          selectInput(
            inputId = "Industry",
            label = "Explore Top 10 by Industry",
            choices = all_indus
          ),
          plotOutput("plot99")
        ),
        
        tabItem(
          tabName = "page3",
          tags$h1("Billionaires by Industry", align = "center"),
          billboarderOutput("plot3", width = "100%", height = "400px"),
          fluidRow(
            box(
              title = "Instructions",
              solidHeader = TRUE,
              status = "success",
              width = 12,
              collapsible = TRUE,
              column(12,
                     tags$div(
                       tags$span(
                         "Hover over the chart to see the % of billionaires in each industry. Click on the
                    tabs in the legend to include or remove industries.",
                         style = "font-size:14px"
                       ),
                     ))
            ),
            box(
              width = 4,
              img(
                src = "Tesla.png",
                width = "100%",
                height = "100%",
                tags$div("Automotive", align = "center")
              )
            ),
            box(
              width = 4,
              img(
                src = "Lvmh.png",
                width = "100%",
                height = "100%",
                tags$div("Fashion and Retail", align = "center")
              )
            ),
            box(
              width = 4,
              img(
                src = "Mars.png",
                width = "100%",
                height = "100%",
                tags$div("Food and Beverage", align = "center")
              )
            ),
          )
          
        ),
        
        tabItem(
          tabName = "page4",
          sliderInput(
            "Age",
            "Age:",
            min = 18,
            max = 101,
            value = 1,
            step = 1,
            animate = animationOptions(interval = 500, loop = FALSE)
          ),
          plotOutput("plot7", height = 350)
        ),
        
        tabItem(
          tabName = "page5",
          tags$h1("Billionaires by Geography", align = "center"),
          fluidRow(
            box(
              width = 14,
              img(
                src = "photo.png",
                width = "100%",
                height = "100%",
              )
            ),
            box(
              title = "Guide",
              solidHeader = TRUE,
              status = "success",
              width = 12,
              collapsible = TRUE,
              column(12,
                     tags$div(
                       tags$span(
                         "Take a look at the clusters of billionaires around the world based on their geographic locations.",
                         style = "font-size:14px"
                       ),
                     ))
            )
          ),
          leafletOutput("myMap", width = "100%")
        ),
        
        
        tabItem(tabName = "page6",
                dataTableOutput("myTable"))
      )
    )
  )


server <- function(input, output, session) {
  output$plot7 = renderPlot({
    p <- data %>% 
      filter(Age <= as.numeric(input$Age)) %>%
      ggplot( aes(x = Age, y = Networth)) +
      xlim(0, 105) +
      ylim(0, 250) +
      labs(title = "Relationship Between Age and Networth") +
      xlab("Age") +
      ylab("Networth (in billions)") +
      geom_point(aes(color = Age)) +
      theme(axis.line = element_line(),
            panel.background = element_blank())
    p
  })
  
  output$plot3 = renderBillboarder({
    data$Industry <- as.factor(data$Industry)
    y = data %>%
      group_by(Industry) %>%
      summarise(CNT = n())
    billboarder() %>%
      bb_donutchart(y) %>%
      #bb_legend(position = 'right') %>%
      #bb_title("Billionaires by Industry") %>%
      bb_legend(position = "bottom")
    
  })
  
  output$plot1 = renderPlot({
    subset = data %>% slice(1:10)
    mycolors = RColorBrewer::brewer.pal(10, "Set3")
    
    p =
      subset %>%
      mutate(Name = fct_reorder(Name, -Networth)) %>%
      ggplot(aes(x = reorder(Name, -Networth), y = Networth)) +
      geom_bar(stat = "identity", aes(fill = Name)) +
      (scale_fill_manual(values = mycolors)) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)
      ) +
      geom_text(aes(label = paste0("$", signif(Networth))), nudge_y = 4) +
      labs(y = "Networth (in billions)") +
      ggtitle("Top 10 Billionaires Overall")
    
    p
    
  })
  
  output$plot99 = renderPlot({
    subset2 = data %>% filter(Industry == input$Industry) %>% slice(1:10)
    mycolors = RColorBrewer::brewer.pal(10, "PRGn")
    p2 =
      subset2 %>%
      mutate(Name = fct_reorder(Name, -Networth)) %>%
      ggplot(aes(x = reorder(Name, -Networth), y = Networth)) +
      geom_bar(stat = "identity", aes(fill = Name)) +
      (scale_fill_manual(values = mycolors)) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)
      ) +
      geom_text(aes(label = paste0("$", signif(Networth))), nudge_y = 4) +
      labs(y = "Networth (in billions)") +
      ggtitle(paste(
        "Top 10 Billionaires in the",
        input$Industry,
        "Industry",
        sep = " "
      ))
    
    p2
    
    
  })
  
  output$myMap = renderLeaflet({
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(
        lat = ~ Latitude,
        lng = ~ Longitude,
        clusterOptions = markerClusterOptions()
      )
  })
  
  
  output$myTable = renderDataTable({
    return(datatable(data, rownames = FALSE))
  })
  
}

shinyApp(ui = ui, server = server)
