library(ggplot2)
library(jsonlite)
library(RColorBrewer)
library(mapproj)
library(treemapify)
library(tidyverse)

##################################################################

json_file <- 'https://datahub.io/JohnSnowLabs/country-and-continent-codes-list/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# get list of all resources:
#print(json_data$resources$name)

# print all tabular data(if exists any)
for(i in 1:length(json_data$resources$datahub$type)){
  if(json_data$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_data$resources$path[i]
    data_countries_codes <- read.csv(url(path_to_file))
    #print(data_countries_codes)
  }
}

##################################################################
wd <-getwd()

data_read=read.csv(paste(wd,"/global_power_plant_database.csv", sep=""), header = TRUE, sep = ",")
data <-as.data.frame(data_read)

data$continent_name <- data_countries_codes$Continent_Name[match(data$country,data_countries_codes$Three_Letter_Country_Code)]
data$continent_code <- data_countries_codes$Continent_Code[match(data$country,data_countries_codes$Three_Letter_Country_Code)]

data <- data[!(is.na(data$country) | data$country == ""), ]
data <- data[!(is.na(data$continent_name) | data$continent_name == ""), ]

data$estimated_generation_gwh_cleaned <- str_sub(data$estimated_generation_gwh, end=-4)

###################################################################

function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(data)+geom_boxplot(aes(data$continent_name, data$capacity_mw), width = 0.5, outlier.alpha = 0.6,fill = "gold")+
      coord_flip()+
      labs(title="Installed capacity [MW] across the world", y="Capacity [MW]", x = "Continent")+
      theme(plot.title = element_text(size=28,face="bold"),
            axis.title=element_text(size=20),
            axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=14))+
    if (input$show_outlier==FALSE) {
      scale_y_continuous(limits=c(0, 10000))
    }
    
  },height = 400)
  # Fill in the spot we created for a plot
  output$plot2 <- renderPlot({
    ggplot(data)+ geom_histogram(aes(x=data$commissioning_year), bins=input$bins, binwidth=5,
                       color="black", fill="lightblue",
                       linetype="dashed", alpha=0.5)+
      labs(title="Commissioning year histogram",x="Year of commissioning", y = "Quantity")+
      theme(plot.title = element_text(size=28,face="bold"),
            axis.title=element_text(size=20))+
      geom_vline(aes(xintercept=mean(data$capacity_mw)),
                 color="blue", linetype="dashed", size=1)+
      scale_x_continuous(limits=c(1880, 2019))
    
  },height = 400)
  
  output$plot3 <- renderPlot({ 
  ggplot(data, aes(x=primary_fuel, y=capacity_mw)) + 
    geom_bar(stat="identity", width=.8, fill="tomato3") + 
    labs(title="Installed capacity by primary fuel [MW]",
         subtitle="Whole world taken into account",
         x="Primary fuel", 
         y="Installed capacity [MW]")+ 
    theme(axis.text.x = element_text(angle=65, vjust=0.6, size=18), 
          plot.title = element_text(size=28,face="bold"),
          axis.title=element_text(size=20))
  },height = 400)
  
    output$plot4 <- renderPlot({  
      ggplot(data, aes(x=data$continent_name))+
        geom_bar(aes(fill=data$primary_fuel), width = 0.7) +
        scale_fill_manual(values=colorRampPalette(brewer.pal(8, "Spectral"))(16))+
      theme(axis.text.x = element_text(angle=65, vjust=0.6, size=16),
            plot.title = element_text(size=28,face="bold"),
            axis.title=element_text(size=20)) + 
        labs(title="Primary fuel supply across the world",
             x="Primary fuel", 
             y="Quantity",
             subtitle="Quantity of power plants across the world by primary fuel supply",
             fill="Primary fuel") 

  },height = 400)


    
    
    output$plot7 <- renderPlot({ 
      world_map <- map_data("world")
      ggplot()+geom_map(data = world_map, map = world_map, aes(x = long, y = lat, group = group, map_id=region),
               fill="lightgray", colour = "white", size=0.5)+
      guides(fill=FALSE)+
        geom_point(data=data[data$primary_fuel==input$p_fuel,],
                   aes(x=data[data$primary_fuel==input$p_fuel,]$longitude,
                       y=data[data$primary_fuel==input$p_fuel,]$latitude), color='red')+
        geom_point(data=data[data$other_fuel1==input$s_fuel,],
                   aes(x=data[data$other_fuel1==input$s_fuel,]$longitude,
                       y=data[data$other_fuel1==input$s_fuel,]$latitude), color='blue')+
        
        labs(title=paste("World map with plants using:", input$p_fuel), 
             subtitle=paste("and eventually as a second fuel:", input$s_fuel))+
        theme(plot.title = element_text(size=28,face="bold"),
              plot.subtitle = element_text(size=18,face="bold"),
              axis.title=element_text(size=20),
              axis.title.x = element_blank(),
              axis.title.y = element_blank())
    },height=800)
    

    output$plot8 <- renderPlot({
      ggplot(data, aes(x=data$continent_name, y=data$commissioning_year)) + 
        geom_point(aes(size=data$capacity_mw), col="tomato2", alpha=0.5) +   # Draw points
        geom_segment(aes(x=data$continent_name, 
                         xend=data$continent_name, 
                         y=min(data$commissioning_year), 
                         yend=max(data$commissioning_yeare)), 
                     linetype="dashed", 
                     size=0.1)+coord_flip()+scale_size(range = c(.5,24))+
        labs(title="Power plant commissioning by continents", 
             x="Continent", 
             y="Commissioning year", 
             size="Capacity [MW]")+
        theme(plot.title = element_text(size=28,face="bold"),
              axis.title=element_text(size=20), 
              axis.text.x = element_text( size=18), 
              axis.text.y = element_text( size=18))
    },height = 400)


    
    output$plot9 <- renderPlot({
      ggplot(data, aes(x=data$primary_fuel, y=data$commissioning_year)) + 
        geom_point(aes(size=data$capacity_mw, color=data$continent_name), alpha=0.3) +   # Draw points
        geom_segment(aes(x=data$primary_fuel, 
                         xend=data$primary_fuel, 
                         y=min(data$commissioning_year), 
                         yend=max(data$commissioning_year)), 
                     linetype="dashed", 
                     size=0.1)+coord_flip()+scale_size(range = c(.5,24))+
        labs(title="Power plant complete info overview",
             subtitle="Plot containts power plant info of commissioning year, primary fuel as well as capacity and continent",
             x="Primary fuel", 
             y="Commissioning year", 
             size="Capacity [MW]",
             color="Continent")+
        theme(plot.title = element_text(size=28,face="bold"),
              axis.title=element_text(size=20), 
              axis.text.x = element_text( size=18), 
              axis.text.y = element_text( size=18))
    },height = 500)
  
  
    output$plot10 <- renderPlot({
     ggplot(data=data, aes(x=data$primary_fuel, y=data$estimated_generation_gwh))+
      geom_boxplot(aes(fill=factor(data$continent_name))) + 
        theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
        labs(title="Box plot of estimated energy generation [GWh] by primary fuel and continents",
             x="Primary fuel",
             y="Estimated energy generation [GWh]",
             fill="Continent")+
        scale_y_continuous(limits=c(0, as.numeric(input$scale)))+
        theme(plot.title = element_text(size=28,face="bold"),
              axis.title=element_text(size=20), 
              axis.text.x = element_text( size=14), 
              axis.text.y = element_text( size=18))

    },height = 500)
}
