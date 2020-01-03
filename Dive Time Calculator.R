#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggmap) # allows for geocoding
library(readxl) # lets you read in an excel file (CURRENTLY DOES NOT WORK)
library(maps) # allows a filter by state
library(leaflet) # Will use to demo maps that can be created
library(tidyverse)

{
  key =  #Enter Google API Key here
  
  register_google(key = key)
  
} 

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Drivetime Calculator"),
   
   # Sidebar  
   sidebarLayout(
      sidebarPanel(
         p("Drive Time Calculator that Uses the Geocoder to create drive-time between buildings please run this through a geocoded file output with Geocoder.R"),
         textInput("Anchor", "Choose a Building I.D"), #Identifies a building that is used to as a center for the drive time calculations
         selectInput("Mode", "What Type of Transit Should be Used", choices =  c("walking", "driving", "transit"), selected = NULL), # Toggles the type of transit run through the geocoder
         textInput("Radius", "What is the maximum radius you would like to include from this building", value = ""), #Indicates acceptable range for travel time
         fileInput("Geocode_File", "Choose CSV File", accept = c("text/csv", "text/comma-seperated-values,text/plain",".csv")) # formats the .csv input
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        downloadButton("download_data", "Download Addresses"), # creates a button for downloading information from the internet
        downloadButton("download_na", "Download NA Values"), # Creates a button to download NA values that could not be geocoded
        tableOutput("contents"), #creates table
        
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  mydata = eventReactive(input$Geocode_File, {
  
    df = read.csv(input$Geocode_File$datapath) # read the csv file into the geocoder
  
    anchor = input$Anchor # Set a central anchor point to map drive time
    
    anchor2= df$ADDRESS[df$Building.Number..or.Identifier. == anchor] # Returns the address of the anchor
    
    anchor2 = as.character(anchor2) #ensures the address is a character string
    
    
    method = input$Mode # Selection for the mode of travel
    
    radius = as.numeric(input$Radius) # parses the radius for the include, exclude
    
    
    if("address" %in% colnames(df)) { 
      df = subset(df, !is.na(df$address)) } # If there is an address column subset the values that do not have an address
    
    else {
      
    }
    
    if( "address2" %in% colnames(df)) {
      df = subset(df, !is.na(df$address2))
    } else {
      
    } 
    
    df$`Lon/Lat` = paste(df$lat, df$lon, sep = ",") # Join the Lat Lon columns together
    
    
    
    test2 = function(address) {
    mapdist(anchor2, as.character(address), mode = method, output = "simple")
  } #Helper code to help the direction converter to code accurately set to accept driving on default
    
    data_frame_2 = lapply(df$`Lon/Lat`, test2) #applies test 2 to geocoder
    
    output$Here = renderText("Stop")
    
    data_frame_3 = data.frame(matrix(unlist(data_frame_2),nrow= length(data_frame_2), byrow=9)) #create a new dataframe that contains ditance time
    
    output$Here = renderText("Stop2")
    
    names(data_frame_3) <- c("Building Point", "Anchor", "Meters", "Kilometers", "Miles", "Seconds", "Minutes", "Hours", "Method") #Names the new data frame
    
    
    
    cols = c("Meters", "Kilometers", "Miles", "Seconds", "Minutes", "Hours") #Name the columms that need to become number
    
    test_3 = function(num) {
    as.numeric(as.character(num))
  } # Helper function to turn factor columns into number columns
    
    output$Here = renderText("Stop3")
    
    data_frame_3[cols] = lapply(data_frame_3[cols], test_3) #Turn factor columns into a number columns
    
    output$Here = renderText("Stop4")
    
    df$Minutes = data_frame_3$Minutes #Concatenate drive time in minutes
    
    output$Here = renderText("Stop5")
    
    df$`Include/Exclude` = ifelse(df$Minutes >= radius, "Exclude", "Include") # Create Include, Exclude based on driving minutes

    
    
    output$Here = renderText("Stop6")
    
    return(df)
    
    
  
 } )
  
  na_data = eventReactive(input$Geocode_File, {
    
    df = read.csv(input$Geocode_File$datapath)
    
    newframe = df[is.na(df$address2), ]
    
    return(newframe)
    
    
  })
  

  
  output$download_na = downloadHandler(
    "NA Address Values.csv", content = function(file) {
      write.csv(na_data(), file)
    } #writes the files that could not be geocoded into a 
  )

  output$download_data = downloadHandler(
    "Geocoded Addresses.csv", content = function(file) { # writes the files as a csv that can be exported by the program
      
      write.csv(mydata(), file)
    } # Formats the download button
  )
  
  output$contents = renderTable(mydata())
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
