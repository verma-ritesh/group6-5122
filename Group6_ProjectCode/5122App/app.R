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
library(scales)
library(ggspatial) # make sure to install if you don't have it
library(viridis)
#install.packages("devtools")
#devtools::install_github("dgrtwo/gganimate")
library(gganimate)
# install.packages('Rcpp')
library(Rcpp)
library(plotly)
library(shinythemes)
iD <- read_csv("data/indexData.csv")
iI <- read_csv("data/indexInfo.csv")
iP <- read_csv("data/indexProcessed.csv")
joined_df <- merge(iI, iP, by = "Index")
Indexes <- as.character(iI$Index)


# Define UI for application 
ui <- fluidPage(
  theme = shinytheme("journal"),
    # Application title
    titlePanel("Stock Exchange Data EDA and Price Prediction"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("chkIndex", "Select Stock Exchange?", Indexes,selected = "GDAXI",multiple=TRUE),
            
            dateRangeInput("daterange1", "Date range:",
                           start = "2001-01-01",
                           end   = "2010-12-31",
                           min    = "1990-01-01",
                           max    = "2020-12-31",
                           format = "yyyy/mm/dd",
                           separator = "-"),
            hr(),
            h3("ARIMA Price Prediction"),
            selectInput("arimaIndex", "Predict for?", Indexes,selected = "J203.JO")
        ),

        # Show a plot of the generated distribution
        mainPanel(

           tabsetPanel(
             tabPanel("High price trend", plotOutput("plot",height = "600px",width = "900px")),
             tabPanel("Volume Scatter plot", plotOutput("plot2",height = "600px",width = "800px")),
             tabPanel("Candlestick", plotlyOutput("candle",height = "600px",width = "900px")),
             tabPanel("Price Prediction", plotOutput("arima",height = "600px",width = "1200px")),
             tabPanel("Animated bubble plot over Years", imageOutput("plot3",height = "600px",width = "900px"))
           )
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


      output$plot <- renderPlot({
        print(input$daterange1)
        joined_df %>%
          filter(Index == input$chkIndex & Date > input$daterange1[1] & Date < input$daterange1[2]) %>%
          ggplot(aes(x=Date,y=High, color=Index)) + geom_line() +
          labs(title = "Stock Exchange Trend",
               subtitle = "For High price",
               caption = " Dataset Source: Kaggle") +
          xlab("Year") + ylab("High") +
          scale_color_manual(values = c("#003B70", "#000000", "#cd1409"))
      }) 
        
      output$plot2 <- renderPlot({
        joined_df %>%
          filter(Index == input$chkIndex & Date > input$daterange1[1] & Date < input$daterange1[2]) %>%
          ggplot(aes(x=Date, y=Volume, colour=Index)) +
          geom_point(shape=1) + geom_smooth(method = 'loess') +
          theme(axis.text.x = element_text(angle = -30, hjust = 0.1)) +
          labs(title = "Stock Exchange Volume",
               x = "Year",
               y = "Volume",
               caption = " Dataset Source: Kaggle")
        
      })  

      #Animation plot is taking 10 mins to generate for the first time, you can look at outfile.gif in the folder to see generated plot saved.
      
      output$plot3 <- renderImage({
        ## Convert the dates into character in order to split the coloumn into "Y" "m" "dd"" columns
        joined_df$Date<-as.character(joined_df$Date)

        ## Split the date and create a list for the same
        list<-strsplit(joined_df$Date,"-")

        ## Convert the list into dataframe
        library(plyr)
        joined_df1<-ldply(list)
        colnames(joined_df1)<-c("Year","Month","Day")
        print(summary(joined_df1))
        ## Column bind with the main dataframe
        Master_Data<-cbind(joined_df,joined_df1)
        names(Master_Data)

        ## Convert the Date to as.Date()
        Master_Data$Date<-as.Date(Master_Data$Date)

        # transform(Master_Data, Year = as.numeric(Year))
        Master_Data["Year"] <- lapply(Master_Data["Year"], as.integer)
        print(typeof(Master_Data["Year"]))

        Master_Data %>% group_by(Year)
        print(summary(Master_Data))

       animPlot <- Master_Data %>% ggplot(aes(Index , Close, size = Close, colour = Index )) +
          geom_point(alpha = 0.7, show.legend = FALSE) +
          #scale_colour_manual(values = country_colors) +
          scale_size(range = c(2, 12)) +
          #scale_x_log10() +
          facet_wrap(~Index) +
          # Here comes the gganimate specific bits
          labs(title = 'Year: {frame_time}', x = '', y = 'Closing Price',
               caption = " Dataset Source: Kaggle") +
          transition_time(Year) +
          ease_aes('linear')

       anim <- animPlot +
        # transition_time(Year) +
         shadow_mark() +
         enter_grow() +
         enter_fade()

       anim_save("outfile.gif", animate(anim, height = 800, width =900)) # New
       
       # Return a list containing the filename
       list(src = "outfile.gif", contentType = "image/gif")

      }) 
      
      output$candle <- renderPlotly({ 
        joined_df %>%
          filter(Index == input$chkIndex & Date > input$daterange1[1] & Date < input$daterange1[2]) %>%
          plot_ly(x = ~ Date,
                  type = "candlestick", 
                  open = ~Open, 
                  close = ~Close, 
                  high = ~High,
                  low = ~Low,
                  name = "price") %>%
          layout(
            xaxis = list(
              rangeselector = list(
                buttons = list(
                  list(
                    count = 1,
                    label = "1 mo",
                    step = "week",
                    stepmode = "backward"),
                  list(
                    count = 3,
                    label = "3 mo",
                    step = "month",
                    stepmode = "backward"),
                  list(
                    count = 6,
                    label = "6 mo",
                    step = "month",
                    stepmode = "backward"),
                  list(
                    count = 1,
                    label = "1 yr",
                    step = "year",
                    stepmode = "backward"),
                  list(
                    count = 3,
                    label = "3 yr",
                    step = "year",
                    stepmode = "backward"),
                  list(step = "all"))),
              rangeslider = list(visible = FALSE)),
            yaxis = list(title = "Price ($)",
                         showgrid = TRUE,
                         showticklabels = TRUE))
        
      })

      output$arima <- renderPlot({
        library('quantmod')
        library('ggplot2')
        library('forecast')
        library('tseries')
        joined_df1<- joined_df %>%
          filter(Index == input$arimaIndex)
      fit <- auto.arima(joined_df1$Close,ic="bic")
      fit.forecast <- forecast(fit)
      plot(fit.forecast,  main= input$arimaIndex)
      fit.forecast
      })

    #})
}

# Run the application 
shinyApp(ui = ui, server = server)
