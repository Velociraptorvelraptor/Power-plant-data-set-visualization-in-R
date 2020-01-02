library(shiny)
library(shinydashboard)



dashboardPage(
  dashboardHeader(title = "Plant data visualization"),
  dashboardSidebar(
    id = "Avaiable views",
    menuItem("Dashboard", tabName = "dashboard", icon = icon("fire")),
    menuItem("Map", icon = icon("industry"), tabName = "map", badgeLabel = "new fuels",
             badgeColor = "green"),
    menuItem("Overview", tabName = "overview", icon = icon("atom")),
    menuItem("Data sources", tabName = "sources", icon = icon("file")),
      checkboxInput(inputId = "show_outlier",
                    label = strong("Show outlier\n
                                   Baihetan Dam, 
                                   Three Gorges, 
                                   Dam Xiluodu (plot n. 1)"),
                    value = FALSE),
             selectInput("bins", "Number of bins (plot n. 5):", 
                         choices=c(10, 20, 30, 40, 50),
                         selected=30)
    ),
    

  dashboardBody(
    # Boxes need to be put in a row (or column)
    # Use a fluid Bootstrap layout
    tags$head( 
      tags$style(HTML(".main-sidebar { font-size: 20px; }")) #change the font size to 20
    ),
    tabItems(   
    tabItem(tabName = 'dashboard',
            fluidRow(
              
              
              column(12,
                     plotOutput("plot1")
              )), 
            
            fluidRow(
              
              column(12,div(style='height:50px;'),
                     plotOutput("plot4")
              )    
            ),
            
            fluidRow(
              
              column(12,div(style='height:50px;'),
                     plotOutput("plot8")
              )),
            
            fluidRow(
              
              column(12,div(style='height:50px;'),
                     plotOutput("plot2")
              )),
            
            
            
            fluidRow(
              
              column(12,div(style='height:50px;'),
                     plotOutput("plot3")
              )),
            
            fluidRow(
              
              column(12,div(style='height:50px;'),
                     plotOutput("plot10")
              )),
          
            
            ),
      
    tabItem(tabName = 'map',
      
      fluidRow(
        column(2,
               
               # Copy the line below to make a slider bar 
               selectInput("p_fuel", "Primary fuel:",
                           c("None", "Biomass", "Coal", "Cogeneration", "Gas", "Geothermal", "Hydro", "Nuclear", "Oil",
                             "Other","Petcoke", "Solar", "Storage", "Wave and Tidal", "Waste", "Wind")),

               selectInput("s_fuel", "Secondary fuel:",
                           c("None", "Biomass", "Coal", "Cogeneration", "Gas", "Geothermal", "Hydro", "Nuclear", "Oil",
                             "Other","Petcoke", "Solar", "Storage", "Wave and Tidal", "Waste", "Wind"))
               ),
        column(10,
               plotOutput("plot7")
               )
               ),

           
    ),
      
    tabItem(tabName = 'overview',
            
      fluidRow(
        
        column(12,
               plotOutput("plot9")
        ))
    ),
      
 

    tabItem(tabName = 'sources',
          mainPanel(h1(strong("Developer: Maks Sedzielski, 102207"), align="center"), 
                    h4("http://datasets.wri.org/dataset/globalpowerplantdatabase", align="center"),
                    h4("https://datahub.io/JohnSnowLabs/country-and-continent-codes-list/datapackage.json", align= "center"))
    )            
            )
      )
)


