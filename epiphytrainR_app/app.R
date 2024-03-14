library(shiny)
library(shinythemes)
library(htmltools)
library(epiphytoolR)
library(epicrop)
library(tidyr)
library(dplyr)
library(DT)
library(ggplot2)
library(leaflet)
library(gridExtra)
load("data/g_weather.rda")
# g_weather <- get_wth(lonlat = c(152.33, 27.55),
#                      dates = c("2010-01-01", "2023-12-31"))
# save(g_weather, file = "epiphytrainR_app/data/g_weather.rda")



# Define UI for application that draws a histogram
ui <-
   fluidPage(
      theme = shinytheme("darkly"),
      # --------------------  Tab 1 --------------------------
      tabsetPanel(
         tabPanel(
            "Start page",
            h1("epiphytrainR"),
            h2("About"),
            p(
               "This is a R shiny app to assist in teaching the principles of
            plant disease epidemiology.",
               HTML("You can find the source code for this app on github,",
                    a("https://github.com/PaulMelloy/epiphytrainR/",
                      href = "https://github.com/PaulMelloy/epiphytrainR/")
               )
            ),
            h2("Acknowledgements"),
            p(
               HTML(
                  "This app is developed by Dr Paul Melloy from the University of
         Queensland using packages such as:",
                  a("epicrop", href = "http://adamhsparks.github.io/epicrop/index.html"),
                  " developed by",
                  a("Prof. Adam Sparks", href = "https://adamhsparks.netlify.app/")
               )
            )
         ),
      # --------------------  Tab 2 --------------------------
      tabPanel("Epicrop",
               p("Epicrop is a package for predicting the intensity of rice diseases using weather data."),
               sidebarLayout(
                  sidebarPanel(
                     radioButtons("disease",
                                  "Select Disease",
                                  choices = c(
                                     "Bacterial Blight" = "bb",
                                     "Brown Spot" = "bs",
                                     "Leaf Blast" = "lb",
                                     "Sheath Blight" = "sb",
                                     "Rice Tungro Spherical Virus" = "tu"
                                     )),
                     dateInput(
                        "startdate",
                        "Date of emergence",
                        value = "2023-03-19",
                        min = "2010-01-01",
                        max = "2023-10-30"),
                     p(),
                     h3("Acknowledgement"),
                     p(
                        HTML(
                           "This app is developed by Dr Paul Melloy
                                         using the epicrop package authored by",
                           a("Prof. Adam Sparks", href = "https://adamhsparks.netlify.app/"),
                           "find the epicrop package on",
                           a("github", href = "https://github.com/adamhsparks/epicrop"),
                           "or",
                           a("codeburg", href = "https://codeberg.org/adamhsparks/epicrop"),
                        )
                     ),
                  ),
                  # Main panel
                  mainPanel(
                     h2("Weather plot"),
                     plotOutput("weather_plot"),
                     p(),
                     h2("Disease Intensity"),
                     plotOutput("disease_plot")
                  )
               )))
      )

# Define server logic required to draw a histogram
server <- function(input, output) {
   epicrop_out <-
      reactive({
         if (input$disease == "bb") {
            #cat("Bacterial Blight")
            return(predict_bacterial_blight(g_weather,
                                            emergence = input$startdate))
         }
         if (input$disease == "bs") {
            #cat("Brown Spot")
            return(predict_brown_spot(g_weather,
                                      emergence = input$startdate))
         }
         if (input$disease == "lb") {
            #cat("Leaf Blast")
            return(predict_leaf_blast(g_weather,
                                      emergence = input$startdate))
         }
         if (input$disease == "sb") {
            #cat("Sheath Blight")
            return(predict_sheath_blight(g_weather,
                                         emergence = input$startdate))
         }
         if (input$disease == "tu") {
            #cat("Tungro")
            return(predict_tungro(g_weather,
                                  emergence = input$startdate))
         }
      })

   output$disease_plot <- renderPlot({
      Intensity <-
         ggplot(data = epicrop_out(),
                aes(x = dates,
                    y = intensity)) +
         labs(y = "Intensity",
              x = "Date") +
         geom_line() +
         geom_point() +
         theme_classic()

      dat <- pivot_longer(
         epicrop_out(),
         cols = c("diseased", "removed", "latent", "infectious"),
         names_to = "site",
         values_to = "value"
      )

      sier_plot <-
         ggplot(data = dat,
                aes(
                   x = dates,
                   y = value,
                   shape = site,
                   linetype = site
                )) +
         labs(y = "Sites",
              x = "Date") +
         geom_line(aes(group = site, colour = site)) +
         geom_point(aes(colour = site)) +
         theme_classic()

      grid.arrange(Intensity, sier_plot, ncol = 2)
   })

   output$weather_plot <- renderPlot({
      w_dat <- g_weather |>
         filter(YYYYMMDD >= input$startdate,
                YYYYMMDD <= input$startdate + 120)

      Tm <- ggplot(w_dat,
                   aes(x = YYYYMMDD,
                       y = TEMP)) +
         labs(y = "Temperature (C)",
              x = "Date") +
         geom_line(color = "red") +
         theme_classic()

      RH <- ggplot(w_dat,
                   aes(x = YYYYMMDD,
                       y = RHUM)) +
         labs(y = "Relative Humidity (%)",
              x = "Date") +
         geom_line(color = "blue") +
         theme_classic()
      rain <- ggplot(w_dat,
                     aes(x = YYYYMMDD,
                         y = RAIN)) +
         labs(y = "Rainfall (mm)",
              x = "Date") +
         geom_col(color = "skyblue") +
         theme_classic()

      grid.arrange(Tm, RH, rain, ncol = 2)

   })


}

# Run the application
shinyApp(ui = ui, server = server)
