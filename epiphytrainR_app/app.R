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
               epi_out <- SEIR(
                  wth = weather_dat(),
                  emergence = input$startdate,
                  onset = input$onset,
                  duration = 120L,
                  rhlim = 90L,
                  rainlim = 5L,
                  H0 = input$H0,
                  I0 = input$I0,
                  RcA = cbind(c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L, 100L, 110L, 120L),
                              c(0.84,0.84,0.84,0.84,0.84,0.84,0.84,0.88,0.88,1.0,1.0,1.0,1.0)),
                  RcT = cbind(
                     c(12L, 16L, 20L, 24L, 28L, 32L, 36L, 40L),
                     c(0, 0.42, 0.94, 0.94, 1.0, 0.85, 0.64, 0)),
                  RcOpt = input$RcOpt,
                  p = input$latent_period,
                  i = input$infectious_period,
                  Sx = 1000L,
                  a = input$aggregation,
                  RRS = input$RRS,
                  RRG = input$RRG)}

         epi_out <- epi_out |>
            mutate(susceptible = sites - infectious - removed - latent)
         return(epi_out)
      })

   output$custom_disease <- renderUI({
      if (input$disease == "custom") {
         tagList(
         actionButton(inputId = "run_custom",
                      label = "Custom Disease",
                      status = "off",
                      on = "Yes",
                      off = "No"),
            sliderInput("onset",
                     "Onset of disease (days)",
                     min = 1,
                     max = 120,
                     value = 30),
         sliderInput("H0",
                     "Initial Heathy sites",
                     min = 1,
                     max = 1000,
                     value = 100),
         sliderInput("I0",
                     "Initial infectious sites",
                     min = 1,
                     max = 120,
                     value = 1),
         sliderInput("Tm_optim",
                     "Optimum temperature for disease (C)",
                     min = 0,
                     max = 45,
                     value = 24),
         sliderInput("RH_optim",
                     "Optimum Relative humidity for disease %",
                     min = 30,
                     max = 100,
                     value = 90),
         sliderInput("RcOpt",
                     "Infection rate, sites per day",
                     min = 0,
                     max = 1,
                     value = 0.5,
                     step = 0.01),
         sliderInput("latent_period",
                     "Latent period (days)",
                     min = 2,
                     max = 30,
                     value = 5),
         sliderInput("infectious_period",
                     "Infectious period (days)",
                     min = 0,
                     max = 100,
                     value = 30),
         sliderInput("aggregation",
                     "Aggregation coeficient",
                     min = 1,
                     max = 20,
                     value = 2.5,
                     step = 0.1),
         sliderInput("RRS",
                     "Relative rate of senecence",
                     min = 0,
                     max = 0.2,
                     value = 0.005,
                     step = 0.001),
         sliderInput("RRG",
                     "Relative rate of Growth",
                     min = 0,
                     max = 1,
                     value = 0.2,
                     step = 0.01))

      }else{
         # it would be nuce here to return the default values for each of the
         #   hard coded diseases options in epicrop
         p("stuff goes here")
      }
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
