data=read.csv("D:/Masters/Urban netwrks/kdd cup/KDDData/KDDData/mi/training_20min_avg_volume_weather_wd.csv")
data1=read.csv("D:/Masters/Urban netwrks/kdd cup/KDDData/KDDData/dataSets (1)/dataSets/training/trajectories(table 5)_training11.csv")
library(shiny)
require(data.table)
library(ggplot2)
library(plotly)
library(shinydashboard)

unique=as.list(unique(data$tollgate_id))
uniqueweek=as.list(data$dayofweek)

ui <- dashboardPage(
  #iOutput('sized_plot'),
  dashboardHeader(title = "Basic dashboard"),
  
  dashboardSidebar(
    sidebarMenu(id = "tab",
                menuItem(strong("Allergies"),tabName = "Allergies",icon = icon("th")),
                conditionalPanel("input.tab == 'Allergies'",    
                                 selectInput("TollGate", label = strong("Select Toll Gate"), 
                                             choices=unique, 
                                             selected = NULL,multiple=TRUE),
                                 selectInput("Direction", label = strong("Select Entry or Exit"), 
                                             c(0,1), 
                                             selected = NULL,multiple=TRUE),
                                 selectInput("dayofweek", label = strong("Select day of week"), 
                                             choices = uniqueweek, 
                                             selected = NULL,multiple=TRUE)
                                 
                                 
                )
                
    )),
  
  
  dashboardBody(
    
    tabItems(tabItem("Allergies",
                     box(title = "Volume Summary",solidHeader = TRUE,background = "navy",plotlyOutput("dischargeBar"),width = "100%",height=550),
                     box(title = "Trajectories Summary",solidHeader = TRUE,background = "navy",plotlyOutput("traject"),width = "100%",height=550)
    )
    
    )
    
    
  ))


server <- function(input, output){
  
  
  
  
  
  output$dischargeBar <- renderPlotly({

    if(!is.null((input$TollGate)))
    {
      data=data[(data$tollgate_id==input$TollGate),]
    }
    
    if(!is.null((input$Direction)))
    {
  
      data=data[(data$direction==input$Direction),]
    }
    
    if(!is.null((input$dayofweek)))
    {
      
      data=data[(data$dayofweek==input$dayofweek),]
    }
    data$time_window=strptime(x = as.character(data$time_window),format = "%m/%d/%Y %H:%M")
    plot_ly(x = ~as.POSIXlt(data$time_window) , y =data$volume,type="bar")%>%
      layout(autosize=F,height=480,width=1800,margin = list(b = 200))
    
  })
  
  output$traject <- renderPlotly({
    
    if(!is.null((input$TollGate)))
    {
      data1=data1[(data1$tollgate_id==input$TollGate),]
    }
    
    
    data1$starting_time=strptime(x = as.character(data1$starting_time),format = "%m/%d/%Y %H:%M")
    plot_ly(x = ~as.POSIXlt(data1$starting_time) , y =data1$travel_time,type="bar")%>%
      layout(autosize=F,height=480,width=1800,margin = list(b = 200))
    
  })
  
}
shinyApp(ui = ui, server = server)