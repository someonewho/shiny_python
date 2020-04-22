library(shiny)
library(plotly)
library(tidyverse)
library(fpp2)
source('C:/Users/82103/Desktop/purdue project/shiny/My_App/KalmanFilter.R')
#setting directory with Absolute path
setwd('C:/Users/82103/Desktop/purdue project/shiny/My_App')
#read data with .csv file

#Test without Oxygen
propTest <- read.csv("Data/PropTesting.csv", stringsAsFactors=F)
propTest<-data.frame(propTest)
predpropTest<-propTest
Small <- read.csv("Data/small/small1_02_16.csv", stringsAsFactors=F)
Small<-data.frame(Small)
Median <- read.csv("Data/middle/middle4_02_16.csv", stringsAsFactors=F)
Median<-data.frame(Median)
Big <- read.csv("Data/big/big2_02_16.csv", stringsAsFactors=F)
Big<-data.frame(Big)

#real test 
SmallwO <- read.csv("Data/small/small1_02_16.csv", stringsAsFactors=F)
SmallwO <- data.frame(SmallwO)
MedianwO <- read.csv("Data/middle/middle4_02_16.csv", stringsAsFactors=F)
MedianwO <- data.frame(MedianwO)
BigwO <- read.csv("Data/CoolTerm Capture 2020-02-23 16-10-41_big05.csv", stringsAsFactors=F)
BigwO <- data.frame(BigwO)

#propane Test
Test0<- read.csv("Data/sensorTesting/PropTesting0.csv")
Test0<-data.frame(Test0)
Test1<- read.csv("Data/sensorTesting/PropTesting1.csv")
Test1<-data.frame(Test1)
Test2<- read.csv("Data/sensorTesting/PropTesting2.csv")
Test2<-data.frame(Test2)

ui = fluidPage(
  titlePanel("Digital Blast Dashboard"),
  sidebarLayout(
    sidebarPanel(
      #parameters for select list
      #id, description, choice list, several checking is available, list limit, default value
      radioButtons("source", "Switch Plot",
                   choices = c("Field Test without Oxygen", "Field Test", "Sensor Test"), #, "Field Test"
                   selected = "Field Test without Oxygen"),
      radioButtons("data","Switch Plot",
                   choices = c("Propane", "CO"),
                   selected = "Propane")
    ),
    mainPanel(
      conditionalPanel(
        condition = ("input.data == 'Propane'&& input.source == 'Field Test without Oxygen'"), plotlyOutput("plot1", width = "1000px", height = "800px")),
      conditionalPanel(
        condition = ("input.data == 'CO' && input.source == 'Field Test without Oxygen'"), plotlyOutput("plot2", width = "1000px", height = "800px")),
      conditionalPanel(
        condition = ("input.data == 'Propane' && input.source == 'Sensor Test'"),  plotlyOutput("plot3", width = "1000px", height = "800px")),
      conditionalPanel(
        condition = ("input.data == 'CO' && input.source == 'Sensor Test'"),  plotlyOutput("plot4", width = "1000px", height = "800px")),
      conditionalPanel(
        condition = ("input.data == 'Propane'&& input.source == 'Field Test'"), plotlyOutput("plot5", width = "1000px", height = "800px")),
      conditionalPanel(
        condition = ("input.data == 'CO' && input.source == 'Field Test'"), plotlyOutput("plot6", width = "1000px", height = "800px"))
      
    )
  ) 
)


server = function(input, output) {
  
  output$plot1 = renderPlotly({
    # Plot
    #%>% is for parameter ggplot(data, aes()) == don %>% ggplot( aes())
    PropTest <- plot_ly(propTest, x = ~sequence, y = ~prop0, name = 'propTest propane: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~prop1, name = 'propTest propane: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~prop2, name = 'propTest propane: Sensor 2', mode = 'lines', color = 'green') %>%
      layout(
        scene = list(
          xaxis = list(title = "Propane Test"),
          yaxis = list(title = "ppm")
        ))
    
    small <- plot_ly(Small, x = ~sequence, y = ~prop0, name = '5.56 propane: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~prop1, name = '5.56 propane: Sensor 1', mode = 'lines', color ='blue') %>%
      add_trace(y = ~prop2, name = '5.56 propane: Sensor 2', mode = 'lines', color ='green')%>%
      layout(
        scene = list(
          xaxis = list(title = "5.56"),
          yaxis = list(title = "ppm")
        ))
    
    median <- plot_ly(Median, x = ~sequence, y = ~prop0, name = '7.62 propane: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~prop1, name = '7.62 propane: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~prop2, name = '7.62 propane: Sensor 2', mode = 'lines', color = 'green')%>%
      layout(
        scene = list(
          xaxis = list(title = "7.62"),
          yaxis = list(title = "ppm")
        ))
    
    big <- plot_ly(Big, x = ~sequence, y = ~prop0, name = '.50CAL propane: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~prop1, name = '.50CAL propane: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~prop2, name = '.50CAL propane: Sensor 2', mode = 'lines', color = 'green')%>%
      layout(
        scene = list(
          xaxis = list(title = ".50CAL"),
          yaxis = list(title = "ppm")
        ))
    
    Prop<-subplot(PropTest, small, median, big, nrows = 2, heights = c(0.4, 0.4)) %>% 
      layout(annotations = list(
      list(x = 0.2 , y = 0.90, text = "propTest", showarrow = F, xref='paper', yref='paper'),
      list(x = 0.8 , y = 0.90, text = "5.56", showarrow = F, xref='paper', yref='paper'),
      list(x = 0.2 , y = 0.45, text = "7.62", showarrow = F, xref='paper', yref='paper'),
      list(x = 0.8 , y = 0.45, text = ".50CAL", showarrow = F, xref='paper', yref='paper')
      )
    )
    Prop
  })
  
  output$plot2<-renderPlotly({
    Test <- plot_ly(propTest, x = ~sequence, y = ~CO0, name = 'propTest CO: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~CO1, name = 'propTest CO: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~CO2, name = 'propTest CO: Sensor 2', mode = 'lines', color = 'green')%>%
      layout(
        scene = list(
          xaxis = list(title = "Propane Test"),
          yaxis = list(title = "ppm")
        ))
    
    small <- plot_ly(Small, x = ~sequence, y = ~CO0, name = '5.56 CO: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~CO1, name = '5.56 CO: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~CO2, name = '5.56 CO: Sensor 2', mode = 'lines', color = 'green')%>%
      layout(
        scene = list(
          xaxis = list(title = "5.56"),
          yaxis = list(title = "ppm")
        ))
    
    median <- plot_ly(Median, x = ~sequence, y = ~CO0, name = '7.62 CO: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~CO1, name = '7.62 CO: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~CO2, name = '7.62 CO: Sensor 2', mode = 'lines', color = 'green')%>%
      layout(
        scene = list(
          xaxis = list(title = "7.62"),
          yaxis = list(title = "ppm")
        ))
    
    big <- plot_ly(Big, x = ~sequence, y = ~CO0, name = '.50CAL CO: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~CO1, name = '.50CAL CO: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~CO2, name = '.50CAL CO: Sensor 2', mode = 'lines', color = 'green')%>%
      layout(
        scene = list(
          xaxis = list(title = ".50CAL"),
          yaxis = list(title = "ppm")
        ))
    
    CO<-subplot(Test, small, median, big, nrows = 2, heights = c(0.4, 0.4))%>% 
      layout(annotations = list(
        list(x = 0.2 , y = 0.90, text = "propTest", showarrow = F, xref='paper', yref='paper'),
        list(x = 0.8 , y = 0.90, text = "5.56", showarrow = F, xref='paper', yref='paper'),
        list(x = 0.2 , y = 0.45, text = "7.62", showarrow = F, xref='paper', yref='paper'),
        list(x = 0.8 , y = 0.45, text = ".50CAL", showarrow = F, xref='paper', yref='paper')
      )
      )
    CO
    
  })
  
  output$plot3<-renderPlotly({
    #%>% is for parameter ggplot(data, aes()) == don %>% ggplot( aes())
    PropTest <- plot_ly(Test0, x = ~sequence, y = ~prop0, name = 'Propane Test0: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~prop1, name = 'Propane Test0: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~prop2, name = 'Propane Test0: Sensor 2', mode = 'lines', color = 'green')%>%
      layout(
        scene = list(
          xaxis = list(title = "Propane Test: Sensor 0"),
          yaxis = list(title = "ppm")
        ))
    
    small <- plot_ly(Test1, x = ~sequence, y = ~prop0, name = 'Propane Test1: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~prop1, name = 'Propane Test1: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~prop2, name = 'Propane Test1: Sensor 2', mode = 'lines', color = 'green')%>%
      layout(
        scene = list(
          xaxis = list(title = "Propane Test: Sensor 1"),
          yaxis = list(title = "ppm")
        ))
    
    
    median <- plot_ly(Test2, x = ~sequence, y = ~prop0, name = 'Propane Test2: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~prop1, name = 'Propane Test2: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~prop2, name = 'Propane Test2: Sensor 2', mode = 'lines', color = 'green')%>%
      layout(
        scene = list(
          xaxis = list(title = "Propane Test: Sensor 2"),
          yaxis = list(title = "ppm")
        ))
    
    Prop<-subplot(PropTest, small, median, nrows = 2, heights = c(0.4, 0.4))%>% 
      layout(annotations = list(
        list(x = 0.2 , y = 0.90, text = "Sensor0", showarrow = F, xref='paper', yref='paper'),
        list(x = 0.8 , y = 0.90, text = "Sensor1", showarrow = F, xref='paper', yref='paper'),
        list(x = 0.2 , y = 0.45, text = "Sensor2", showarrow = F, xref='paper', yref='paper')
      ))
    Prop
  })
  
  output$plot4<-renderPlotly({
    Test <- plot_ly(Test0, x = ~sequence, y = ~CO0, name = 'CO Test0: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~CO1, name = 'CO Test0: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~CO2, name = 'CO Test0: Sensor 2', mode = 'lines', color = 'green')%>%
      layout(
        scene = list(
          xaxis = list(title = "Propane Test: Sensor 0"),
          yaxis = list(title = "ppm")
        ))
    
    small <- plot_ly(Test1, x = ~sequence, y = ~CO0, name = 'CO Test1: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~CO1, name = 'CO Test1: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~CO2, name = 'CO Test1: Sensor 2', mode = 'lines', color = 'green')%>%
      layout(
        scene = list(
          xaxis = list(title = "Propane Test: Sensor 1"),
          yaxis = list(title = "ppm")
        ))
    
    median <- plot_ly(Test2, x = ~sequence, y = ~CO0, name = 'CO Test2: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~CO1, name = 'CO Test2: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~CO2, name = 'CO Test2: Sensor 2', mode = 'lines', color = 'green')%>%
      layout(
        scene = list(
          xaxis = list(title = "Propane Test: Sensor 2"),
          yaxis = list(title = "ppm")
        ))
    
    CO <- subplot(Test, small, median, nrows = 2, heights = c(0.4, 0.4))%>% 
      layout(annotations = list(
        list(x = 0.2 , y = 0.90, text = "Sensor0", showarrow = F, xref='paper', yref='paper'),
        list(x = 0.8 , y = 0.90, text = "Sensor1", showarrow = F, xref='paper', yref='paper'),
        list(x = 0.2 , y = 0.45, text = "Sensor2", showarrow = F, xref='paper', yref='paper')
      ))
    CO
    
  })
  
  output$plot5 = renderPlotly({
    # Plot
    #%>% is for parameter ggplot(data, aes()) == don %>% ggplot( aes())
    PropTest <- plot_ly(propTest, x = ~sequence, y = ~prop0, name = 'propTest propane: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~prop1, name = 'propTest propane: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~prop2, name = 'propTest propane: Sensor 2', mode = 'lines', color = 'green') %>%
      layout(
        scene = list(
          xaxis = list(title = "Propane Test"),
          yaxis = list(title = "ppm")
        ))
    
    small <- plot_ly(Small, x = ~sequence, y = ~prop0, name = '5.56 propane: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~prop1, name = '5.56 propane: Sensor 1', mode = 'lines', color ='blue') %>%
      add_trace(y = ~prop2, name = '5.56 propane: Sensor 2', mode = 'lines', color ='green')%>%
      layout(
        scene = list(
          xaxis = list(title = "5.56"),
          yaxis = list(title = "ppm")
        ))
    
    median <- plot_ly(Median, x = ~sequence, y = ~prop0, name = '7.62 propane: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~prop1, name = '7.62 propane: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~prop2, name = '7.62 propane: Sensor 2', mode = 'lines', color = 'green')%>%
      layout(
        scene = list(
          xaxis = list(title = "7.62"),
          yaxis = list(title = "ppm")
        ))
    
    big <- plot_ly(BigwO, x = ~sequence, y = ~prop0, name = '.50CAL propane: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~prop1, name = '.50CAL propane: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~prop2, name = '.50CAL propane: Sensor 2', mode = 'lines', color = 'green')%>%
      layout(
        scene = list(
          xaxis = list(title = ".50CAL"),
          yaxis = list(title = "ppm")
        ))
    big
    Prop<-subplot(PropTest, small, median, big, nrows = 2, heights = c(0.4, 0.4)) %>% 
      layout(annotations = list(
        list(x = 0.2 , y = 0.90, text = "propTest", showarrow = F, xref='paper', yref='paper'),
        list(x = 0.8 , y = 0.90, text = "5.56", showarrow = F, xref='paper', yref='paper'),
        list(x = 0.2 , y = 0.45, text = "7.62", showarrow = F, xref='paper', yref='paper'),
        list(x = 0.8 , y = 0.45, text = ".50CAL", showarrow = F, xref='paper', yref='paper')
      ))
    Prop
  })
  
  output$plot6<-renderPlotly({
    Test <- plot_ly(propTest, x = ~sequence, y = ~CO0, name = 'propTest CO: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~CO1, name = 'propTest CO: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~CO2, name = 'propTest CO: Sensor 2', mode = 'lines', color = 'green')%>%
      layout(
        scene = list(
          xaxis = list(title = "Propane Test"),
          yaxis = list(title = "ppm")
        ))
    
    small <- plot_ly(Small, x = ~sequence, y = ~CO0, name = '5.56 CO: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~CO1, name = '5.56 CO: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~CO2, name = '5.56 CO: Sensor 2', mode = 'lines', color = 'green')%>%
      layout(
        scene = list(
          xaxis = list(title = "5.56"),
          yaxis = list(title = "ppm")
        ))
    
    median <- plot_ly(Median, x = ~sequence, y = ~CO0, name = '7.62 CO: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~CO1, name = '7.62 CO: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~CO2, name = '7.62 CO: Sensor 2', mode = 'lines', color = 'green')%>%
      layout(
        scene = list(
          xaxis = list(title = "7.62"),
          yaxis = list(title = "ppm")
        ))
    
    big <- plot_ly(BigwO, x = ~sequence, y = ~CO0, name = '.50CAL CO: Sensor 0', type = 'scatter', mode = 'lines', color = 'red') %>%
      add_trace(y = ~CO1, name = '.50CAL CO: Sensor 1', mode = 'lines', color = 'blue') %>%
      add_trace(y = ~CO2, name = '.50CAL CO: Sensor 2', mode = 'lines', color = 'green')%>%
      layout(
        scene = list(
          xaxis = list(title = ".50CAL"),
          yaxis = list(title = "ppm")
        ))
    big
    CO<-subplot(Test, small, median, big, nrows = 2, heights = c(0.4, 0.4))%>% 
      layout(annotations = list(
        list(x = 0.2 , y = 0.90, text = "propTest", showarrow = F, xref='paper', yref='paper'),
        list(x = 0.8 , y = 0.90, text = "5.56", showarrow = F, xref='paper', yref='paper'),
        list(x = 0.2 , y = 0.45, text = "7.62", showarrow = F, xref='paper', yref='paper'),
        list(x = 0.8 , y = 0.45, text = ".50CAL", showarrow = F, xref='paper', yref='paper')
      ))
    CO
    
  })
  
  output$textOutput <- renderText({})
  
}

shinyApp(ui = ui, server = server)