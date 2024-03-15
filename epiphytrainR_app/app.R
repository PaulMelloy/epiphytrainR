library(shiny)
library(shinythemes)
#library(htmltools)
library(shinycssloaders)
if(isFALSE("showPageSpinner" %in% ls("package:shinycssloaders"))){
   # showPageSpinner is not in the CRAN package version but on github
   detach("package:shinycssloaders", unload = TRUE)
   remotes::install_github("daattali/shinycssloaders", dependencies = TRUE)
   library(shinycssloaders)
}
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
      tabsetPanel(id = "tabs",
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
            " developed by", a("Prof. Adam Sparks", href = "https://adamhsparks.netlify.app/"),
            hr(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            # ------------------------------------------------- SESSION INFO ----------------------------------
            h3("Session info"),
            textOutput("workdir"),
            textOutput("datafiles"),
            br(),
            tableOutput("loaded_pkgs")
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
                           plotOutput("weather_plot1",height = "450px"),
                           p(),
                           h2("Disease progress"),
                           plotOutput("disease_plot1",height = "550px")
                        ) # end mainPanel
                     )# end sidebarLayout
            ), # end tabPanel
            #
            #
            # # --------------------  Tab 3 --------------------------
            tabPanel("SEIR", id = "SEIR",
                     p("SEIR is a disease model which simulates Susceptible, Exposed,
                 Infectious and Removed plant sites during a plant disease epidemic."),
                 sidebarLayout(
                    sidebarPanel(
                       dateInput(
                          "startdate",
                          "Date of rice crop emergence",
                          value = "2023-03-19",
                          min = "2010-01-01",
                          max = "2023-10-30"),
                       hr(),
                       sliderInput("onset",
                                   "Onset of disease (days after emergence)",
                                   min = 1,
                                   max = 120,
                                   value = 30),
                       hr(),
                       sliderInput("H0",
                                   "Initial Heathy sites",
                                   min = 1,
                                   max = 1000,
                                   value = 100),
                       hr(),
                       sliderInput("I0",
                                   "Initial infectious sites",
                                   min = 1,
                                   max = 120,
                                   value = 1),
                       hr(),
                       sliderInput("Tm_optim",
                                   "Optimum temperature for disease (C)",
                                   min = 0,
                                   max = 45,
                                   value = 24),
                       hr(),
                       sliderInput("RH_optim",
                                   "Optimum Relative humidity for disease %",
                                   min = 30,
                                   max = 100,
                                   value = 90),
                       hr(),
                       sliderInput("RcOpt",
                                   "Infection rate, sites per day",
                                   min = 0,
                                   max = 1,
                                   value = 0.5,
                                   step = 0.01),
                       hr(),
                       sliderInput("latent_period",
                                   "Latent period (days)",
                                   min = 2,
                                   max = 30,
                                   value = 5),
                       hr(),
                       sliderInput("infectious_period",
                                   "Infectious period (days)",
                                   min = 0,
                                   max = 100,
                                   value = 30),
                       hr(),
                       sliderInput("aggregation",
                                   "Aggregation coeficient",
                                   min = 1,
                                   max = 20,
                                   value = 2.5,
                                   step = 0.1),
                       hr(),
                       sliderInput("RRS",
                                   "Relative rate of senecence",
                                   min = 0,
                                   max = 0.2,
                                   value = 0.005,
                                   step = 0.001),
                       hr(),
                       sliderInput("RRG",
                                   "Relative rate of Growth",
                                   min = 0,
                                   max = 1,
                                   value = 0.2,
                                   step = 0.01),
                       hr(),
                       p(),
                       h3("Acknowledgement"),
                       "SEIR R model was authored by",
                       a("Prof. Adam Sparks", href = "https://adamhsparks.netlify.app/"),
                       "find the SEIR model in the epicrop package on",
                       a("github", href = "https://github.com/adamhsparks/epicrop"),
                       "or", a("codeburg", href = "https://codeberg.org/adamhsparks/epicrop"),
                       uiOutput("testing")
                    ), # end sidebarPanel
                    mainPanel(
                       h2("Weather plot"),
                       plotOutput("weather_plot2",height = "450px"),
                       p(),
                       h2("Disease progress"),
                       plotOutput("disease_plot2",height = "450px")
                    ) # end mainPanel
                 ) # end sidebarLayout
            ) # end tabPanel
      ) # end tabsetPanel
   ) # end fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {

   # sessino info
   output$loaded_pkgs <-
      renderTable({
         dat1 <- as.data.frame(devtools::loaded_packages())
         dat1[order(dat1$package),]
      })
   output$workdir <- renderText(getwd())
   output$datafiles <- renderText(list.files("data/"))

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

   # Update the weather location map if either the map is clicked
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
         if(input$tabs == "SEIR"){
            epi_out <-
               SEIR(
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
                  RRG = input$RRG)
         }else{

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
            }}
         epi_out <- epi_out |>
            mutate(susceptible = sites - infectious - removed - latent)
         return(epi_out)
      })

   observeEvent(input$SEIR, {
      cat(input$SEIR)
      cat("Custom")

      epicrop_out() <- SEIR(
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
         RRG = input$RRG)})

   # Disease plots ----------------------------------------------
   output$disease_plot1 <-
      output$disease_plot2 <-renderPlot({

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
            geom_line(linewidth = 2) +
            geom_line(data = epicrop_out(),
                      aes(x = dates,
                          y = senesced,
                          colour = "Senesced"),
                      linewidth = 2) +
            geom_line(data = epicrop_out(),
                      aes(x = dates,
                          y = (latent + infectious + removed),
                          colour = "Diseased"),
                      linewidth = 2) +
            theme_classic()+
            ggtitle("Heathy, Senesced\nand Diseased sites")+
            theme(legend.position = "bottom",
                  text = element_text(size = 15),
                  plot.margin = margin(t=30,l=10, r = 30))+
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
            geom_line(aes(group = site, colour = site),linewidth = 2) +
            geom_point(aes(colour = site)) +
            theme_classic()+
            theme(legend.position = "bottom",
                  text = element_text(size = 15),
                  plot.margin = margin(t=30,l=10, r = 30))+
            ggtitle("Diseased Plant Sites: Exposed, Infectious\nand Removed")


         grid.arrange(Intensity, sier_plot, ncol = 2)
      })

   # Weather plots ----------------------------------------------
   output$weather_plot1 <-
      output$weather_plot2 <- renderPlot({
         w_dat <- weather_dat() |>
            filter(YYYYMMDD >= input$startdate,
                   YYYYMMDD <= input$startdate + 120)

         Tm <- ggplot(w_dat,
                      aes(x = YYYYMMDD,
                          y = TEMP)) +
            geom_line(color = "red") +
            theme_classic()+
            theme(text = element_text(size = 15),
                  plot.margin = margin(t=30,l=10, r = 30))+
            labs(y = "Temperature (C)",
                 x = "Date")

         RH <- ggplot(w_dat,
                      aes(x = YYYYMMDD,
                          y = RHUM)) +
            labs(y = "Relative Humidity (%)",
                 x = "Date") +
            geom_line(color = "blue") +
            theme_classic()+
            theme(text = element_text(size = 15),
                  plot.margin = margin(t=30,l=10, r = 30))

         rain <- ggplot(w_dat,
                        aes(x = YYYYMMDD,
                            y = RAIN)) +
            labs(y = "Rainfall (mm)",
                 x = "Date") +
            geom_col(color = "skyblue") +
            theme_classic()+
            theme(text = element_text(size = 15),
                  plot.margin = margin(t=30,l=10, r = 30))

         grid.arrange(Tm, RH, rain, ncol = 2)

      })


}

# Run the application
shinyApp(ui = ui, server = server)
