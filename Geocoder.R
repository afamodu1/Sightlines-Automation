#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny) #allows for the app to be run
library(ggmap) # allows for geocoding
library(readxl) # lets you read in an excel file (CURRENTLY DOES NOT WORK)
library(maps) # allows a filter by state
library(leaflet) # Will use to demo maps that can be created
library(tidyverse)

{
key = 

register_google(key = key)

} #Contains my API Key Please Do Not Take!!!

holder = c()

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Address Converter"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        p("Please enter in a state, town, then the building inventory pulled out from Athena. This tool will try to convert the building inventory to a list of addresses then create what a map of what the building inventory should look like in a Power BI Map at the bottom. Please wait for the table to be displayed before attempting a download."),
        p("This tool attempts to clean our building name by removing everything within parentheses and replacing the abbreviation bldg to building." ),
        p("Once the data is downloaded check the returned addresses to ensure that they are not just the town and state, this is an error and means that the program could not find the address by asking google"),
        textInput("Town", "Choose a Town", value = ""),
        textInput("State", "Choose a State", value = ""),
        #textInput("Anchor", "Choose a Building I.D"),
        #selectInput("Mode", "What Type of Transit Should be Used", choices =  c("walking", "driving", "transit"), selected = NULL),
        #textInput("Radius", "What is the maximum radius you would like to include from this building", value = ""),
         fileInput("Geocode_File", "Choose CSV File", accept = c("text/csv", "text/comma-seperated-values,text/plain",".csv")) # defines a file input as well as the expectations for it
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        downloadButton("download_data", "Download Addresses"), # creates a button for downloading information from the internet
        downloadButton("download_na", "Download NA Values"), # Creates a button to download NA values that could not be geocoded
        tableOutput("contents"),
        leafletOutput("Mymap"),
        textOutput("Here")
      )
   )
)


# Define server logic geocode addresses
server <- function(input, output) {
  
  
  

  
  
  mydata = eventReactive(input$Geocode_File, {
    df  = read.csv(input$Geocode_File$datapath) # turns the csv into a dataframe
    df$ADDRESS = as.character(df$`Building.Name`) #turns the address string into a character string that can be used by the program
    #df$ADDRESS = gsub("\\(Aux)", "", df$Building.Name)
    df$ADDRESS = gsub("Addition","", df$ADDRESS)
    df$ADDRESS = gsub("Bldg","Building", df$ADDRESS)
    #df$ADDRESS = gsub("Build","", df$ADDRESS)
    df$ADDRESS = gsub( "*\\(.*?\\) *","", df$ADDRESS)
    df$ADDRESS = paste(df$ADDRESS,", ", input$Town, ", ", input$State)
    df = mutate_geocode(df, ADDRESS, output = "latlona")
     
      #anchor = input$Anchor # Set a central anchor point to map drive time
     
     

      #method = input$Mode # Selection for the mode of travel
     
     
     
     #radius = as.numeric(input$Radius)
     
     
     #anchor2= df$ADDRESS[df$Building.Number..or.Identifier. == anchor]
     
     
     
     #test2 = function(address) {
      # mapdist(anchor2, address, mode = method, output = "simple")
     #} #Helper code to help the direction converter to code accurately set to accept driving on default
     
     #df_2 = df[!is.na(df$address2), ]
     
     #data_frame_2 = lapply(df$ADDRESS, test2) # maps driving distance
     
     
     
     #data_frame_3 = data.frame(matrix(unlist(data_frame_2),nrow= length(data_frame_2), byrow=9)) #create a new dataframe that contains ditance time 
     
     #names(data_frame_3) <- c("Building Point", "Anchor", "Meters", "Kilometers", "Miles", "Seconds", "Minutes", "Hours", "Method") #Names the new data frame
     
     #cols = c("Meters", "Kilometers", "Miles", "Seconds", "Minutes", "Hours") #Name the columms that need to become number
     
     #test_3 = function(num) {
       #as.numeric(as.character(num))
     #} # Helper function to turn factor columns into number columns
     
     #data_frame_3[cols] = lapply(data_frame_3[cols], test_3) #Turn factor columns into a number columns
     
     #df$Minutes = data_frame_3$Minutes #Concatenate drive time in minutes
     
     #df$`Include/Exclude` = ifelse(df$Minutes >= radius, "Exclude", "Include") # Create Include, Exclude based on driving minutes
     
     output$Mymap = renderLeaflet(leaflet(data = df) %>% addTiles() %>% addMarkers(~lon, ~lat, label = ~as.character(`Building.Name`))) #Output the map
     

    return(df) # return the dataframe, finish the function, the dataframe that has been formatted can be called using mydata()
  }
  
  
    
  )
  
  output$contents = renderTable({
    mydata() #displays the results of geocoding
  }) # Displays the new table

  output$download_data = downloadHandler(
    "Geocoded Addresses.csv", content = function(file) { # writes the files as a csv that can be exported by the program
      
      write.csv(mydata(),file)
    } # Formats the download button
  )
  
  output$download_na = downloadHandler(
    "NA Address Values.csv", content = function(file) {
      write.csv(newframe(),file)
    } #writes the files that could not be geocoded into a 
  )
}
 

# Run the application 
shinyApp(ui = ui, server = server)
