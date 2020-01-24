library(plotly)
library(dplyr)
library(babynames)
library(reticulate)
setwd('C:/Users/82103/Desktop/purdue project/shiny/My_App')
#Ideal_Point_Data <- read.csv("Data/UN_IdealPoints.csv", stringsAsFactors=F)
ui = fluidPage(
  titlePanel("Digital Blast Dashboard"),
  sidebarLayout(
    sidebarPanel(
      #parameters for select list
      #id, description, choice list, several checking is available, list limit, default value
      selectizeInput("name",
                     label = "detection data",
                     choices = c("propane", "oxygen", "pressure"),
                     multiple = T,
                     options = list(maxItems = 3,
                                    placeholder = 'Select a name'),
                     selected = "propane"
                     
      ),
      helpText("Data: Bailey, Michael, Anton  Strezhnev and Erik Voeten. Forthcoming. stimating Dynamic State Preferences from United Nations Voting Data. Journal of Conflict Resolution. ")
      
    ),
    mainPanel(
      plotOutput("plot"),
      textOutput("textOutput")
    )
  ) 
)

source_python('practice.py')

server = function(input, output) {
  
  output$plot = renderPlot({
    
    na <- defineName(input$name)
    #making a data with selected name and sex
    don <- babynames %>% 
      filter(name %in% na) %>%
      filter(sex=="F")
    
    # Plot
    #%>% is for parameter ggplot(data, aes()) == don %>% ggplot( aes())
    don %>%
      ggplot( aes(x=year, y=n, group=name, color=name)) +
      geom_line()
    #geom_line is for line graph
  })
  
  output$textOutput <- renderText({
    value = input$name
    returnedText = testMethod(value)
  })
  
}

shinyApp(ui = ui, server = server)