#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Easy Prediction"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      p("This is a template that I created to process and export prediction data please add the following colums before using this tool"),
      p("Please add the GSF, Reno. Age Category, and Sightlines Function Tab to the column list in Athena"),
      p("Afterwords drag and drop the csv output into this application"),
      fileInput("Predict_me", "Choose CSV File"),
      
      downloadButton('download_total_age', 'Download Total Need By Age'),
      
      br(),
      br(),
      downloadButton('download_total_system', 'Download Total Need By System'),
      
      br(),
      br(),
      downloadButton('download_current_system', 'Download Current Need By System'),
      
      br(),
      br(),
      downloadButton('download_total_function', 'Download Total Need By Function'),
      
      
      br(),
      br(),
      downloadButton('download_total_by_fiscal_year', 'Download Renewal Need By Fiscal Year'),
      
      br(),
      br(),
      downloadButton('download_system_by_fiscal_year', 'Download System Need By Fiscal Year')
      
    ),
    
    
    mainPanel(
      
      
      
      plotlyOutput("plot3"),
      br(),
      br(),
      
      #plotlyOutput("plot"),
      
      tableOutput("distPlot"),
        
      
      plotlyOutput("plot4"),
      
      #br(),
      #br(),
      
      #plotlyOutput("plot2"),
      
      br(),
      br(),
      
      plotlyOutput("plot5"),
      
      br(),
      br(),
      
      plotlyOutput("plot6"),
      
      br(),
      br(),
      
      plotlyOutput("plot7"),
      
      br(),
      br()
      
      
    )
  )
)

server <- function(input, output) {
  

  
  fill_missing_age = function(mydata) {
    if (is.element(10, mydata$Group.1) == FALSE) {
      mydata = mydata %>% add_row(Group.1 = 10, x = 0)
    } else if (is.element(25, mydata$Group.1) == FALSE) {
      mydata = mydata %>% add_row(Group.1 = 10, x = 0)
    } else if (is.element(50, mydata$Group.1) == FALSE) {
      mydata = mydata %>% add_row(Group.1 = 10, x = 0)
    } else if (is.element(100, mydata$Group.1) == FALSE) {
      add_row(Group.1 = 10, x = 0)
    } else {
      mydata = mydata
    }
    
    mydata = mydata %>% arrange(Group.1)
    
    return(mydata)
  }
  
  create_campus_gsf = function(full.frame) {
    
    beta = aggregate(full.frame$GSF, list(full.frame$Reno..Age.Category, full.frame$Building.Name), FUN=mean) #pivots the GSF by reno age category and building name
    raw.data = aggregate(beta$x, list(beta$Group.1), FUN=sum) #pivots the GSF by reno age
    raw.data = fill_missing_age(raw.data) #makes sure no reno age is blank for GSF
    

    return(raw.data)
    
  }
  
  create_need = function(cut.frame) {
    
    
    gamma = aggregate(cut.frame$Total.System.Cost, list(cut.frame$`Reno..Age.Category`), FUN=sum)
    
    gamma = fill_missing_age(gamma)
    
    return(gamma)
    
  }
  
  
  
  create_current_need = eventReactive(input$Predict_me, {
    
    df = read.csv(input$Predict_me$datapath) # reads csv from input
    
    df$`Reno..Age.Category` = as.factor(df$`Reno..Age.Category`) # turns age into a factor and keeps the charts looking nice
    
    campus_gsf = create_campus_gsf(df) #pulls in the above function to create gsf table
    
    backlog_need = filter(df, df$Backlog == TRUE) #filters on backlog needby pulling in above equation
    
    backlog_need = create_need(backlog_need) #create backlog need 
    
    backlog_need = merge(campus_gsf, backlog_need, "Group.1") # creates a table with gsf and backlog
    
    names(backlog_need) = c("Age Category", "GSF", "Backlog Need") #renames table
    
    backlog_need$`Backlog Need Per GSF` = backlog_need$`Backlog Need`/backlog_need$GSF # makes backlog need per GSF
    
    return(backlog_need)
    
    
  })
  
  create_renewal_need = eventReactive(input$Predict_me, {
    
    df = read.csv(input$Predict_me$datapath) # allows the csv to be read by the program/ copies it into another table
    
    df$`Reno..Age.Category` = as.factor(df$`Reno..Age.Category`) # turns age into a factor and keeps the charts looking nice
    
    campus_gsf = create_campus_gsf(df)
    
    renewal_need = filter(df, df$`Model.Predicted.Next` <= 2029) # seperates renewal need and copies it into a new table
    
    renewal_need = create_need(renewal_need) # uses above function to aggregate need
    
    #renewal_need = fill_missing_age(renewal_need) # makes sure no need is blank in the age category column
    
    renewal_need = merge(campus_gsf, renewal_need, "Group.1") # merges GSF and renewal need together
    
    names(renewal_need) = c("Age Category", "GSF", "Renewal Need") #renames table
    
    renewal_need$`Renewal Need Per GSF` = renewal_need$`Renewal Need`/renewal_need$GSF #creates need per GSF
    
    return(renewal_need) # returns table used
    
  })
  
  create_total_need = eventReactive(input$Predict_me, {
    
     backlog = create_current_need() # creates backlog chart
    
     renewal = create_renewal_need() # creates renewal need chart
     
     total_need =  merge(backlog, renewal, "Age Category") # merges the two together to recieve the total
     
     return(total_need) # returns the total chart
    
  })
  
  create_total_system_need = eventReactive(input$Predict_me, {
    
    df = read.csv(input$Predict_me$datapath) # reads csv from input
    
    df$Reno..Age.Category = as.factor(df$Reno..Age.Category) #turns age into a factor
    
    campus_gsf = sum(filter(df, (System == 'Building Exteriors' | System == 'SMALL Building Renovation'))$GSF) # creates total campus GSF
    
    total_data = filter(df, Model.Predicted.Next <= 2029 | is.na(Model.Predicted.Next)) # creates total data field
    
    aggregate_total_need = aggregate(total_data$Total.System.Cost, list(total_data$System), sum) #aggregate by systems cost
    
    aggregate_total_need$x = aggregate_total_need$x/campus_gsf #creates need/GSF by system
    
    names(aggregate_total_need) = c("System", "Total Need By System") #Renames table columns
    
    return(aggregate_total_need)
    
  })
  
  create_current_system_need = eventReactive(input$Predict_me, {
    
    df = read.csv(input$Predict_me$datapath)
    
    df$Reno..Age.Category = as.factor(df$Reno..Age.Category)
    
    backlog_data = filter(df, Backlog  == TRUE)
    
    campus_gsf = sum(filter(df, (System == 'Building Exteriors' | System == 'SMALL Building Renovation'))$GSF)
    
    aggregate_backlog = aggregate(backlog_data$Total.System.Cost, list(backlog_data$System), sum)
    
    aggregate_backlog$x = aggregate_backlog$x/campus_gsf
    
    names(aggregate_backlog) = c("System", "Backlog_Need_by_System")
      
    
    
    return(aggregate_backlog)
    
    
  })
  
  
  create_total_function_need = eventReactive(input$Predict_me, {
    
    df = read.csv(input$Predict_me$datapath)
    
    df$Reno..Age.Category = as.factor(df$Reno..Age.Category)
    
    beta = aggregate(df$GSF, list(df$Sightlines.Function, df$Building.Name), FUN=mean) #pivots the GSF by function and building name
    
    raw.data = aggregate(beta$x, list(beta$Group.1), FUN=sum) #pivots the GSF by building name
    
    campus_gsf = sum(filter(df, (System == 'Building Exteriors' | System == 'SMALL Building Renovation'))$GSF)
    
    total_data = filter(df, Model.Predicted.Next <= 2029 | is.na(Model.Predicted.Next))
    
    aggregate_total_need = aggregate(total_data$Total.System.Cost, list(total_data$Sightlines.Function), sum)
    
    aggregate_total_need$x = aggregate_total_need$x/raw.data$x
    
    names(aggregate_total_need) = c("Function", "Total_Need_By_Function")
    
    return(aggregate_total_need)
    
  })
  
  
  create_renewal_need_by_year = eventReactive(input$Predict_me, {
    
    df = read.csv(input$Predict_me$datapath)
    
    df$Reno..Age.Category = as.factor(df$Reno..Age.Category)
    
    campus_gsf = sum(filter(df, (System == 'Building Exteriors' | System == 'SMALL Building Renovation'))$GSF)
    
    backlog_data = filter(df, Backlog  == TRUE)
    
    backlog_need = sum(backlog_data$Total.System.Cost)
    
    renewal_data = filter(df, Model.Predicted.Next <= 2029 | Backlog == TRUE )
    
    renewal_data$Model.Predicted.Next[is.na(renewal_data$Model.Predicted.Next)] <- "Current Need"
    
    renewal_data$Model.Predicted.Next = as.factor(renewal_data$Model.Predicted.Next)
    
    
    
    
    
    renewal_need = sum(renewal_data$Total.System.Cost)
    
    modernization_need = ((backlog_need+renewal_need)*.35)/10
    
    aggregate_renewal_need = aggregate(renewal_data$Total.System.Cost, list(renewal_data$Model.Predicted.Next), sum)
    
    aggregate_renewal_need$modernization = modernization_need
    
    
    {if (is.element(10, aggregate_renewal_need$Group.1) == FALSE) {
      aggregate_renewal_need = aggregate_renewal_need %>% add_row(Group.1 = 10, x = 0)
    } else if (is.element(25, aggregate_renewal_need$Group.1) == FALSE) {
      aggregate_renewal_need = aggregate_renewal_need %>% add_row(Group.1 = 25, x = 0)
    } else if (is.element(50, aggregate_renewal_need$Group.1) == FALSE) {
      aggregate_renewal_need = aggregate_renewal_need %>% add_row(Group.1 = 50, x = 0)
    } else if (is.element(100, aggregate_renewal_need$Group.1) == FALSE) {
      aggregate_renewal_need = aggregate_renewal_need %>% add_row(Group.1 = 100, x = 0)
    } else {
      aggregate_renewal_need = aggregate_renewal_need
    }
      aggregate_renewal_need = aggregate_renewal_need %>% arrange(Group.1)
      
      names(aggregate_renewal_need) = c("Fiscal_Year", "Renewal Need per Year", "modernization")
      
    }
    
    
    
    return(aggregate_renewal_need)
    
  })
  

  create_total_system_need_per_year = eventReactive(input$Predict_me, {
    
    df = read.csv(input$Predict_me$datapath)
    
    df$Reno..Age.Category = as.factor(df$Reno..Age.Category)
    
    campus_gsf = sum(filter(df, (System == 'Building Exteriors' | System == 'SMALL Building Renovation'))$GSF)
    
    total_data = filter(df, Model.Predicted.Next <= 2029 | is.na(Model.Predicted.Next))
    
    #Removing the below comment will bring in current need
    
    total_data$Model.Predicted.Next[is.na(total_data$Model.Predicted.Next)] <- "Current Need"
    
    total_data$Model.Predicted.Next = as.factor(total_data$Model.Predicted.Next)
    
    aggregate_total_need = aggregate(total_data$Total.System.Cost, list(total_data$Model.Predicted.Next, total_data$System), sum)
    
    aggregate_total_need$x = aggregate_total_need$x/campus_gsf
    
    names(aggregate_total_need) = c("Fiscal_Year", "System", "Need")
    
    return(aggregate_total_need)
    
  })
  
  
  
  
  
  
  {
  

  output$download_total_age = downloadHandler(
     "total need by age.csv", content = function(file) {
      write.csv(create_total_need(), file)
    }
  )
  

  
  output$download_total_system = downloadHandler(
    "total need by system.csv", content = function(file) { 
      write.csv(create_total_system_need(), file) 
      }
  )
  
  output$download_total_function = downloadHandler(
    "current need by system.csv", content = function(file) { 
      write.csv(create_current_system_need(), file) 
    }
  )
  
  
  output$download_total_by_fiscal_year = downloadHandler(
    "Renewal Need By Fiscal Year.csv", content = function(file) { 
      write.csv(create_renewal_need_by_year(), file) 
    }
  )
  
  output$download_system_by_fiscal_year = downloadHandler(
    "Renewal Need By System By Fiscal Year.csv", content = function(file) { 
      write.csv(create_total_system_need_per_year(), file) 
    }
  )
  
  
  } # Download Managers
  
  
  
  
  
  
    #output$distPlot <- renderTable({ create_renewal_need_by_year() })
  
    #output$plot = renderPlotly({ 
     # plot_ly(create_total_need(), x = ~Age, y = ~Renewal_Need_per_GSF) })
  
    #output$plot2 = renderPlotly({ 
     # plot_ly(create_current_system_need(), x = ~System, y = ~Backlog_Need_by_System, type = 'bar', name = 'Backlog Need') %>%
      #  layout(yaxis = list(title = 'Need/GSF'))
      #})
    
    output$plot3 = renderPlotly({ plot_ly(create_total_need(), x = ~`Age Category`, y = ~`Backlog Need Per GSF`, type ='bar', name = 'Backlog Need') %>% 
        add_trace(y = ~`Renewal Need Per GSF`, name = 'Renewal Need') %>%
        layout(yaxis = list(title = 'Need/GSF'), barmode = 'stack') %>% layout(title = list( text = 'Total Need By Age', x = .45))
      })
    
    output$plot4 = renderPlotly({
      plot_ly(create_total_system_need(), labels = ~System, values = ~`Total Need By System`, type = 'pie') %>%
      layout(title = list( text = 'Total Need By System', x = .45))
    })
    
    output$plot5 = renderPlotly(({
      plot_ly(create_total_function_need(), x = ~Function, y = ~Total_Need_By_Function, type = 'bar', name = 'Need By Function') %>% 
        layout(title = list( text = 'Total Need by Function', x = .45)) %>%
        layout(yaxis = list(title = 'Need/GSF'))
    }))
    
    output$plot6 = renderPlotly(({
      plot_ly(create_renewal_need_by_year(), x = ~Fiscal_Year, y = ~`Renewal Need per Year`, type = 'bar', name = 'Renewal Need') %>% 
        add_trace(y = ~modernization, name = 'Predicted Modernization Need') %>%
        layout(title = list( text = 'Renewal & Modernization Need By Year', x = .45)) %>%
        layout(yaxis = list(title = 'Total Need'), barmode = 'stack')
    }))
  
    output$plot7 = renderPlotly(({
      plot_ly(create_total_system_need_per_year(), color = ~System, x = ~Fiscal_Year, y = ~Need, type = 'bar' ) %>% 
        layout(title = list( text = 'Renewal & Modernization Need By Year', x = .45)) %>%
        layout(yaxis = list(title = 'Total Need/GSF'), barmode = 'stack')
      
    }))
    
    
    
    
    
    
    
    
    
    
}







  

# Run the application 
shinyApp(ui = ui, server = server)
