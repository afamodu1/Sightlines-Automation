#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#


library(shiny)
library(tidyverse)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Easy Prediction"),
  
  # Sidebar panel that manages downloads
  sidebarLayout(
    sidebarPanel(
      p("This is a template that I used to process Prediction Data please add the following colums before using this tool"),
      p("Please add the GSF, Reno. Age Category, and Sightlines Function Tab"),
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
      
      br(),
      br(),
      
      plotlyOutput("plot2"),
      
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
      mydata = mydata %>% add_row(Group.1 = 25, x = 0)
    } else if (is.element(50, mydata$Group.1) == FALSE) {
      mydata = mydata %>% add_row(Group.1 = 50, x = 0)
    } else if (is.element(100, mydata$Group.1) == FALSE) {
      add_row(Group.1 = 100, x = 0)
    } else {
      mydata = mydata
    }
    
    mydata = mydata %>% arrange(Group.1)
    
    return(mydata)
  }
  
  create_need = eventReactive(input$Predict_me, {
    
    df = read.csv(input$Predict_me$datapath)
    
    df$Sightlines.Function = as.factor(df$Sightlines.Function)
    
    campus_gsf = sum(filter(df, (System == 'Building Exteriors' | System == 'SMALL Building Renovation'))$GSF)
    
    total_data = filter(df, Model.Predicted.Next <= 2028 | is.na(Model.Predicted.Next))
    
    aggregate_total_need = aggregate(total_data$Total.System.Cost, list(total_data$Model.Predicted.Next), sum)
    
    return(aggregate_total_need)
  })
  
  
  create_current_need = eventReactive(input$Predict_me, {
    
    df = read.csv(input$Predict_me$datapath)
    
    df$Reno..Age.Category = as.factor(df$Reno..Age.Category)
    
    backlog_data = filter(df, Backlog  == 'true')
    
    campus_gsf = sum(filter(df, (System == 'Building Exteriors' | System == 'SMALL Building Renovation'))$GSF)
    
    aggregate_backlog = aggregate(backlog_data$Total.System.Cost, list(backlog_data$Reno..Age.Category), sum)
    
    aggregate_backlog$x = aggregate_backlog$x/campus_gsf
    
    {if (is.element(10, aggregate_backlog$Group.1) == FALSE) {
      aggregate_backlog = aggregate_backlog %>% add_row(Group.1 = 10, x = 0)
    } else if (is.element(25, aggregate_backlog$Group.1) == FALSE) {
      aggregate_backlog = aggregate_backlog %>% add_row(Group.1 = 25, x = 0)
    } else if (is.element(50, aggregate_backlog$Group.1) == FALSE) {
      aggregate_backlog = aggregate_backlog %>% add_row(Group.1 = 50, x = 0)
    } else if (is.element(100, aggregate_backlog$Group.1) == FALSE) {
      add_row(Group.1 = 100, x = 0)
    } else {
      aggregate_backlog = aggregate_backlog
    }
    
    aggregate_backlog = aggregate_backlog %>% arrange(Group.1)
    
    names(aggregate_backlog) = c("Age", "Backlog_Need_per_GSF")
    
    }
    
    return(aggregate_backlog)
    
    
  })
  
  create_renewal_need = eventReactive(input$Predict_me, {
    
    df = read.csv(input$Predict_me$datapath)
    
    df$Reno..Age.Category = as.factor(df$Reno..Age.Category)
    
    campus_gsf = sum(filter(df, (System == 'Building Exteriors' | System == 'SMALL Building Renovation'))$GSF)
    
    renewal_data = filter(drop_na(df, Model.Predicted.Next), Model.Predicted.Next <= 2028)
    
    renewal_need = sum(renewal_data$Total.System.Cost)
    
    aggregate_renewal_need = aggregate(renewal_data$Total.System.Cost, list(renewal_data$Reno..Age.Category), sum)
    
    aggregate_renewal_need$x = aggregate_renewal_need$x/campus_gsf
    
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
      
      names(aggregate_renewal_need) = c("Age", "Renewal_Need_per_GSF")
    
    }

    
    
    return(aggregate_renewal_need)
    
  })
  
  create_total_need = eventReactive(input$Predict_me, {
    
     backlog = create_current_need()
    
     renewal = create_renewal_need()
     
    #dframe = data.frame(backlog$Age, backlog$Backlog_Need_per_GSF)
     
     total_need =  merge(backlog, renewal, "Age")
     
     return(total_need)
    
  })
  
  create_total_system_need = eventReactive(input$Predict_me, {
    
    df = read.csv(input$Predict_me$datapath)
    
    df$Reno..Age.Category = as.factor(df$Reno..Age.Category)
    
    campus_gsf = sum(filter(df, (System == 'Building Exteriors' | System == 'SMALL Building Renovation'))$GSF)
    
    total_data = filter(df, Model.Predicted.Next <= 2028 | is.na(Model.Predicted.Next))
    
    aggregate_total_need = aggregate(total_data$Total.System.Cost, list(total_data$System), sum)
    
    aggregate_total_need$x = aggregate_total_need$x/campus_gsf
    
    names(aggregate_total_need) = c("System", "Total_Need_By_System")
    
    return(aggregate_total_need)
    
  })
  
  create_current_system_need = eventReactive(input$Predict_me, {
    
    df = read.csv(input$Predict_me$datapath)
    
    df$Reno..Age.Category = as.factor(df$Reno..Age.Category)
    
    backlog_data = filter(df, Backlog  == 'true')
    
    campus_gsf = sum(filter(df, (System == 'Building Exteriors' | System == 'SMALL Building Renovation'))$GSF)
    
    aggregate_backlog = aggregate(backlog_data$Total.System.Cost, list(backlog_data$System), sum)
    
    aggregate_backlog$x = aggregate_backlog$x/campus_gsf
    
    names(aggregate_backlog) = c("System", "Backlog_Need_by_System")
      
    
    
    return(aggregate_backlog)
    
    
  })
  
  
  create_total_function_need = eventReactive(input$Predict_me, {
    
    df = read.csv(input$Predict_me$datapath)
    
    df$Reno..Age.Category = as.factor(df$Reno..Age.Category)
    
    campus_gsf = sum(filter(df, (System == 'Building Exteriors' | System == 'SMALL Building Renovation'))$GSF)
    
    total_data = filter(df, Model.Predicted.Next <= 2028 | is.na(Model.Predicted.Next))
    
    aggregate_total_need = aggregate(total_data$Total.System.Cost, list(total_data$Sightlines.Function), sum)
    
    aggregate_total_need$x = aggregate_total_need$x/campus_gsf
    
    names(aggregate_total_need) = c("Function", "Total_Need_By_Function")
    
    return(aggregate_total_need)
    
  })
  
  
  create_renewal_need_by_year = eventReactive(input$Predict_me, {
    
    df = read.csv(input$Predict_me$datapath)
    
    df$Reno..Age.Category = as.factor(df$Reno..Age.Category)
    
    campus_gsf = sum(filter(df, (System == 'Building Exteriors' | System == 'SMALL Building Renovation'))$GSF)
    
    backlog_data = filter(df, Backlog  == 'true')
    
    backlog_need = sum(backlog_data$Total.System.Cost)
    
    renewal_data = filter(drop_na(df, Model.Predicted.Next), Model.Predicted.Next <= 2028)
    
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
      
      names(aggregate_renewal_need) = c("Fiscal_Year", "Renewal_Need_per_Year", "modernization")
      
    }
    
    
    
    return(aggregate_renewal_need)
    
  })
  

  create_total_system_need_per_year = eventReactive(input$Predict_me, {
    
    df = read.csv(input$Predict_me$datapath)
    
    df$Reno..Age.Category = as.factor(df$Reno..Age.Category)
    
    campus_gsf = sum(filter(df, (System == 'Building Exteriors' | System == 'SMALL Building Renovation'))$GSF)
    
    total_data = filter(df, Model.Predicted.Next <= 2028 | is.na(Model.Predicted.Next))
    
    #Removing the below comment will bring in current need
    
    #total_data$Model.Predicted.Next[is.na(total_data$Model.Predicted.Next)] <- "Current Need"
    
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
  
  
  
  
  
  
    #output$distPlot <- renderTable({ create_total_system_need_per_year() })
  
    output$plot = renderPlotly({ 
      plot_ly(create_current_need(), x = ~Age, y = ~Backlog_Need_per_GSF) })
  
    output$plot2 = renderPlotly({ 
      plot_ly(create_current_system_need(), x = ~System, y = ~Backlog_Need_by_System, type = 'bar', name = 'Backlog Need') %>%
        layout(yaxis = list(title = 'Need/GSF'))
      })
    
    output$plot3 = renderPlotly({ plot_ly(create_total_need(), x = ~Age, y = ~Backlog_Need_per_GSF, type ='bar', name = 'Backlog Need') %>% 
        add_trace(y = ~Renewal_Need_per_GSF, name = 'Renewal Need') %>%
        layout(yaxis = list(title = 'Need/GSF'), barmode = 'stack') %>% layout(title = list( text = 'Total Need By Age', x = .45))
      })
    
    output$plot4 = renderPlotly({
      plot_ly(create_total_system_need(), labels = ~System, values = ~Total_Need_By_System, type = 'pie') %>%
      layout(title = list( text = 'Total Need By System', x = .45))
    })
    
    output$plot5 = renderPlotly(({
      plot_ly(create_total_function_need(), x = ~Function, y = ~Total_Need_By_Function, type = 'bar', name = 'Need By Function') %>% 
        layout(title = list( text = 'Total Need by Function', x = .45)) %>%
        layout(yaxis = list(title = 'Need/GSF'))
    }))
    
    output$plot6 = renderPlotly(({
      plot_ly(create_renewal_need_by_year(), x = ~Fiscal_Year, y = ~Renewal_Need_per_Year, type = 'bar', name = 'Renewal Need') %>% 
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



