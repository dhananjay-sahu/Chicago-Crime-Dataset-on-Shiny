list.of.packages <- c("shiny","ggmap","tidyverse","stringr","tidyr","tidyverse","lubridate") 
#Specify the list of packages required for your project
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]  
#Check for the packages that are NOT installed in the system
if(length(new.packages)) install.packages(new.packages)   
#Install the packages NOT installed in the system

library(ggmap)
library(stringr)
library(tidyverse)
library(shiny)
library(tidyr)
library(shinyWidgets)
library(chron)
library(leaflet)
library(lubridate)


df = read.csv("Crimes_-_2018.csv", nrows = 1000)
head(df)

sapply(df, function(x) sum(is.na(x)))# to count missing values of all columns

#Split Date column into two columns- Crime_Date and Crime_Time

df <- tidyr::separate(data=df,
                      col= Date,
                      into=c("Crime_Date", "Crime_Time"),
                      sep=" ",
                      remove=FALSE)
#Convert Crime_Date and Crime_Time from string to date and time

str(df$Crime_Date)
table(is.na(df$Crime_Date))
df$Crime_Date <- as.Date(df$Crime_Date, format = "%m/%d/%Y")

str(df$Crime_Time)
table(is.na(df$Crime_Time))
df$Crime_Time <- strptime(df$Crime_Time, "%H:%M")
df$Crime_Time <- times(format(df$Crime_Time, "%H:%M:%S"))

df$Date<-as.POSIXct(df$Date, format="%m/%d/%Y %I:%M:%S %p", tz="UTC")
df$Hour<- format(as.POSIXct(df$Date,format="%m/%d/%Y %H:%M:%S"),'%H')


#register_google(key = 'AIzaSyA-2GIxXlK17Rj5ANHZJ8RgtjuMZ2mCmvU')

# df$Block <- paste(df$Block, ", Chicago", sep = "") #to control ambiguity
# 
# for(i in 1:nrow(df))
# {
#   # Print("Working...")
#   result <- geocode(df$Block[i], output = "latlona", source = "google")
#   df$lon[i] <- as.numeric(result[1])
#   df$lat[i] <- as.numeric(result[2])
#   df$geoAddress[i] <- as.character(result[3])
# }
# Write a CSV file containing df to the working directory
#write.csv(df, "geocoded_crime.csv", row.names=FALSE)
#tried using geocode for the missing values using df$block to find longitude and latitude but google
#is restricting me for some unknown reasons after some 


#droping incomplete cases by using 
df <- df[complete.cases(df), ]
sapply(df, function(x) sum(is.na(x))) #checking for missing values

count<-data.frame(table(df$Hour, df$Primary.Type))
count$Var1<- as.numeric(count$Var1)


##creating shiny app

ui <- fluidPage(
  navbarPage(title = "Chiacgo Crime Report",
             tabPanel(title = "Frequency of Crime type by month",
                      sidebarLayout(
                        sidebarPanel(
                          #Dropdown for Crime
                          selectInput(
                            inputId = "crimetype1", label = "Crime Type",
                            multiple = TRUE,
                            choices = sort(unique(df$Primary.Type)))
                        ),
                        
                        mainPanel(
                          h3("Histogram for Crimes"),
                          plotOutput(outputId = "hist")
                          
                        )
                      )
             ),             
             #Crime location leaflet
             tabPanel(title = "Crime Location by date",
                                    sidebarLayout(
                                      sidebarPanel(
                                        fluidRow(
                                          #select date range
                                          dateRangeInput(inputId = "FilterDate",
                                                         label = "Select the date range",
                                                         start = min(df$Crime_Date),
                                                         end = min(df$Crime_Date),
                                                         min = min(df$Crime_Date),
                                                         max = max(df$Crime_Date),
                                                         format = "mm/dd/yyyy",
                                                         separator = "-")
                                        )
                                      ),
                                      
                                      mainPanel(
                                        h3("Crime Locations"),
                                        # Create a leaflet output for map plot
                                        leafletOutput("mymap",width=1230, height = 400)
                                        
                                        
                                      )
                                    )
             ),
             #Heatmap
             tabPanel(title = "Heat Map",
                        mainPanel(
                        h4("Location of crimes by date"),
                        plotOutput("heatmap",height = "800")
                      ))
  ))


server <- function(input,output, session) {
  get_frequency_crime <- reactive(
    {
      crime_type <- input$crimetype1
      crime2 <<- input$crimetype1
      subdata <- df[(df$Primary.Type == crime_type), ]
      subdata["Month"] <-months(subdata$Crime_Date, abbreviate = T)
      subdata["Month_num"] <- format(subdata$Crime_Date, "%m")
      histdata <- subdata %>% group_by(Month, Month_num) %>% summarise(Count = n())
      histdata$Month <- factor(x = histdata$Month,levels = histdata$Month[order(histdata$Month_num)])
      return(histdata[order(histdata$Month_num), ])
    }
  )
  output$hist <- renderPlot(
    {
      frequency_crime <- get_frequency_crime()
      #returning the Histogram
      histogram <- ggplot(data = frequency_crime, aes(x= Month, y=Count)) +geom_bar(stat="identity")+
        geom_text(aes(label=Count), vjust=2, color="white", size=5)
      return(histogram)
    }
  )
  
  #map
  data <- reactive({
    
    df= subset(df, df$Crime_Date>= input$FilterDate[1] & df$Crime_Date<= input$FilterDate[2])
    return(df)
  })
  output$mymap <- renderLeaflet({
    df <- data()
    
    m <- leaflet(data = df) %>%
      addTiles() %>%
      addAwesomeMarkers(lng = ~Longitude,
                        lat = ~Latitude,
                        popup = paste("Crime Date:", df$Crime_Date, "<br>",
                                      "Crime Type:", df$Primary.Type))
    m
    
  })
  #heatmap
    output$heatmap <- renderPlot({
      ggplot(count, aes(x = count$Var1, y = count$Var2)) + geom_tile(aes(fill = count$Freq))+scale_fill_gradient(name = "Total Crimes", low = "red",high = "black")+
        ggtitle("Heatmap for Crime type and Crime Time") + labs(x="Hour",y="Crime type")
  })
}

shinyApp(ui = ui, server = server)

#normalize data for heatmap
