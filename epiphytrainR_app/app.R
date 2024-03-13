library(shiny)
library(shinythemes)
library(epiphytoolR)
library(epicrop)
library(dplyr)
library(ggplot2)
load("data/g_weather.rda")
# g_weather <- get_wth(lonlat = c(152.33, 27.55),
#                      dates = c("2010-01-01", "2023-12-31"))
# save(g_weather, file = "epiphytrainR_app/data/g_weather.rda")



# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("darkly"),
                tabsetPanel(
                   tabPanel("Bacterial Blight",
                            sidebarLayout(
                               sidebarPanel(
                                  radioButtons("disease", "Select Disease",
                                               choices = c("Bacterial Blight" = "bb")),
                                  dateInput("startdate", "Date of emergence",
                                            value = "2023-03-19",
                                  min = "2010-01-01",max = "2023-10-30")),
                               # Main panel
                               mainPanel(plotOutput("weather_plot"),
                                         plotOutput("disease_plot"))
                               )

                            ))
                )

# Define server logic required to draw a histogram
server <- function(input, output) {
    epicrop_out <-
       reactive({
        if (input$disease == "bb") {
           cat("Bacterial Blight")
            return(predict_bacterial_blight(g_weather,
                                     emergence = input$startdate))
        }
          if (input$disease == "bs"){
             cat("Brown Spot")
             return(predict_brown_spot(g_weather,
                                emergence = input$startdate))
           }
    })

    output$disease_plot <- renderPlot({

       ggplot(data = epicrop_out(),
              aes(x = dates,
                  y = intensity)) +
          labs(y = "Intensity",
               x = "Date") +
          geom_line() +
          geom_point() +
          theme_classic()
    })

    output$weather_plot <- renderPlot({
       w_dat <- g_weather |>
          filter(YYYYMMDD >= input$startdate,
                 YYYYMMDD <= input$startdate + 120)

          ggplot(w_dat,
                 aes(x = YYYYMMDD,
                     y = TEMP)) +
          labs(y = "Temperature (C)",
               x = "Date") +
          geom_line(color = "red") +
          geom_line(data = w_dat,
                    aes(x = YYYYMMDD,
                        y = RHUM),color = "blue") +
           theme_classic()
    })


}

# Run the application
shinyApp(ui = ui, server = server)
