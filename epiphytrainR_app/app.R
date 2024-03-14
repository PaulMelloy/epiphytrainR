library(shiny)
library(shinythemes)
#library(htmltools)
library(shinycssloaders)
library(epiphytoolR)
library(epicrop)
library(tidyr)
library(dplyr)
#library(DT)
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
            "This is a R shiny app to assist in teaching the principles of
            plant disease epidemiology. You can find the source code for this app on ",
            a("github,",
              "https://github.com/PaulMelloy/epiphytrainR/",
              href = "https://github.com/PaulMelloy/epiphytrainR/"),
            h2("Choose weather location"),
            leafletOutput("worldmap"),
            "Global weather data is sourced from the",
            a("NASA POWER project", href = "https://power.larc.nasa.gov/"),
            "via the R api", a("nasapower", href = "https://docs.ropensci.org/nasapower/"),
            h2("Acknowledgements"),
            "This app is developed by ", a("Dr Paul Melloy",href = "https://paulmelloy.github.io/"),
            "from",a("The University of Queensland", href = "https://www.uq.edu.au/"),
            "using packages such as:", a("epicrop", href = "http://adamhsparks.github.io/epicrop/index.html"),
            " developed by", a("Prof. Adam Sparks", href = "https://adamhsparks.netlify.app/")
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
                                     "Rice Tungro Spherical Virus" = "tu",
                                     "Custom" = "custom"
                                     )),
                     uiOutput("custom_disease"),
                     dateInput(
                        "startdate",
                        "Date of rice crop emergence",
                        value = "2023-03-19",
                        min = "2010-01-01",
                        max = "2023-10-30"),
                     p(),
                     h3("Acknowledgement"),
                     "epicrop model was authored by",
                     a("Prof. Adam Sparks", href = "https://adamhsparks.netlify.app/"),
                     "find the epicrop package on",
                     a("github", href = "https://github.com/adamhsparks/epicrop"),
                     "or", a("codeburg", href = "https://codeberg.org/adamhsparks/epicrop")
                  ),
                  # Main panel
                  mainPanel(
                     h2("Weather plot"),
                     plotOutput("weather_plot"),
                     p(),
                     h2("Disease progress"),
                     plotOutput("disease_plot")
                  )
               )))
      )

# Define server logic required to draw a histogram
server <- function(input, output) {

   # Render map for farm address input
   output$worldmap <- renderLeaflet({
      leaflet() %>%
         addProviderTiles("Esri.WorldTopoMap") %>%
         # Centre the map in in the destination
         setView(
            lng = 152.33,
            lat = -27.55,
            zoom = 4
         ) %>%
         addMarkers(lng = 152.33,
                    lat = -27.55,
                    layerId = "gatton_start")
   })

   # Update the weather location map if either the map is clicked or, update
   #   Delivery button is clicked
   observeEvent(
      input$worldmap_click, {
         cat(input$worldmap_click[["lat"]]," ", input$worldmap_click[["lng"]])
      leafletProxy("worldmap") %>%
            removeMarker(layerId = "gatton_start") %>%
            addMarkers(lng = input$worldmap_click[["lng"]],
                       lat = input$worldmap_click[["lat"]],
                       layerId = "weather_location")
      })

   weather_dat <- reactive({
      if (is.null(input$worldmap_click)) {
         return(g_weather)
      }else{
         showPageSpinner(type = 1,
                         size = 3,
                         caption = "Please wait while the weather data is downloaded ..." )
      got_wth <- get_wth(
         lonlat = c(input$worldmap_click[["lng"]],
                    input$worldmap_click[["lat"]]),
         dates = c("2010-01-01", "2023-12-31"))
      hidePageSpinner()
      return(got_wth)}
      })


   epicrop_out <-
      reactive({
         if (input$disease == "bb") {
            #cat("Bacterial Blight")
            epi_out<- predict_bacterial_blight(weather_dat(),
                                            emergence = input$startdate)
         }
         if (input$disease == "bs") {
            #cat("Brown Spot")
            epi_out<- predict_brown_spot(weather_dat(),
                                      emergence = input$startdate)
         }
         if (input$disease == "lb") {
            #cat("Leaf Blast")
            epi_out<- predict_leaf_blast(weather_dat(),
                                      emergence = input$startdate)
         }
         if (input$disease == "sb") {
            #cat("Sheath Blight")
            epi_out<- predict_sheath_blight(weather_dat(),
                                         emergence = input$startdate)
         }
         if (input$disease == "tu") {
            #cat("Tungro")
            epi_out<- predict_tungro(weather_dat(),
                                  emergence = input$startdate)
         }
         if (input$disease == "custom") {
            #cat("Custom")
            epi_out<- SIER(wth = weather_dat(),
                           emergence = input$startdate,
                           onset = 30
                           )
         }
         epi_out <- epi_out |>
            mutate(susceptible = sites - infectious - removed - latent)
         return(epi_out)
      })



   # Disease plots ----------------------------------------------
   output$disease_plot <- renderPlot({

      coloured_lines <- c(Healthy = "darkgreen",
                          Senesced = "gold",
                          Diseased = "darkred")

      Intensity <-
         ggplot(data = epicrop_out(),
                aes(x = dates,
                    y = susceptible,
                    colour = "Healthy")) +
         labs(y = "Sites",
              x = "Date",
              colour = "Site") +
         geom_line() +
         geom_line(data = epicrop_out(),
                   aes(x = dates,
                       y = senesced,
                       colour = "Senesced")) +
         geom_line(data = epicrop_out(),
                   aes(x = dates,
                       y = (latent + infectious + removed),
                       colour = "Diseased")) +
         theme_classic()+
         ggtitle("Heathy, Senesced and Diseased sites")+
         theme(legend.position = "bottom")+
         scale_color_manual(values = coloured_lines)

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
         theme_classic()+
         theme(legend.position = "bottom")+
         ggtitle("Diseased Plant Sites: Exposed, Infectious and Removed")


      grid.arrange(Intensity, sier_plot, ncol = 2)
   })

   output$weather_plot <- renderPlot({
      w_dat <- weather_dat() |>
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
