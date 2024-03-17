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
source("R/Tm_range.R")
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
                                 "or", a("codeburg", href = "https://codeberg.org/adamhsparks/epicrop"),
                                 "This model is based on the published manuscript by Savary et al. (2012)",
                                 a("Savary, S., Nelson A., Willocquet, L., Pangga, I., Aunario, J. (2012).
                                   Modeling and mapping potential epidemics of rice diseases globally, 34(2012), 6-17.",
                                   href = "https://doi.org/10.1016/j.cropro.2011.11.009")
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
                                 style = "overflow-y:scroll;position:relative;max-height:1000px;max-width:400px;",
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
                                 p("Temperature influence on disease progress"),
                                 numericInput("temp_opti",
                                              HTML("Optimum temperature (C) for <br /> disease growth"),
                                              min = 5,max = 30,step = 0.1,value = 21),
                                 splitLayout(
                                    numericInput("temp_min",
                                                 HTML("Minimum temperature (C) for <br /> disease growth"),
                                                 min = -5,max = 28,step = 0.1,value = 8),
                                    numericInput("temp_max",
                                                 HTML("Maximum temperature (C) for <br /> disease growth"),
                                                 min = 10,max = 50,step = 0.1,value = 35,)),
                                 plotOutput("R_Tm_plot", height = "250px"),
                                 hr(),
                                 numericInput("rh_lim",
                                              HTML("RH (%) threshold for wet leaves<br/>where infection is most likely"),
                                                 min = 5,max = 99,step = 1,value = 90),
                                 hr(),
                                 radioButtons("R_Age",
                                              expression("Plant age effect on pathogen R"[0]),
                                              choices = c("Slight increase (Sheath blight)" = "Sheathblight",
                                                          "Strong decrease (Leaf Blast)" = "LeafBlast",
                                                          "Decrease (Bacterial Blight)" = "BacterialBlight",
                                                          "Strong increase (Brown Spot)" = "BrownSpot",
                                                          "Increase then decrease (Tungro)" = "Tungro")),
                                 plotOutput("R_age_plot", height = "250px"),
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
                                 h3("Acknowledgements"),
                                 "SEIR R model was authored by",
                                 a("Prof. Adam Sparks", href = "https://adamhsparks.netlify.app/"),
                                 "find the SEIR model in the epicrop package on",
                                 a("github", href = "https://github.com/adamhsparks/epicrop"),
                                 "or", a("codeburg", href = "https://codeberg.org/adamhsparks/epicrop"),
                                 "This model is based on the published manuscript by Savary et al. (2012)",
                                 a("Savary, S., Nelson A., Willocquet, L., Pangga, I., Aunario, J. (2012).
                                   Modeling and mapping potential epidemics of rice diseases globally, 34(2012), 6-17.",
                                   href = "https://doi.org/10.1016/j.cropro.2011.11.009")
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
                  rhlim = input$rh_lim,
                  rainlim = 5L,
                  H0 = input$H0,
                  I0 = input$I0,
                  RcA = r_age(),
                  RcT = cbind(
                     seq(1,50,by = 1),
                     Tm_range(seq(1,50,by = 1),
                              Tm_opt = input$temp_opti,
                              Tm_max = input$temp_max,
                              Tm_min = input$temp_min,
                              stdev = 10)),
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

   # observeEvent(input$SEIR, {
   #    cat(input$SEIR)
   #    cat("Custom")
   #
   #    epicrop_out() <- SEIR(
   #       wth = weather_dat(),
   #       emergence = input$startdate,
   #       onset = input$onset,
   #       duration = 120L,
   #       rhlim = 90L,
   #       rainlim = 5L,
   #       H0 = input$H0,
   #       I0 = input$I0,
   #       RcA = r_age(),
   #       RcT = cbind(
   #          c(12L, 16L, 20L, 24L, 28L, 32L, 36L, 40L),
   #          c(0, 0.42, 0.94, 0.94, 1.0, 0.85, 0.64, 0)),
   #       RcOpt = input$RcOpt,
   #       p = input$latent_period,
   #       i = input$infectious_period,
   #       Sx = 1000L,
   #       a = input$aggregation,
   #       RRS = input$RRS,
   #       RRG = input$RRG)})

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

         date5 <-epicrop_out()$dates[5]

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
            geom_text(aes(x = date5,
                          y = max(value, na.rm = TRUE),
                          label = paste0("AUDPC: ",
                                         round(epicrop_out()$AUDPC[1],3))),
                      vjust = -0.5, hjust = -0.5, size = 5) +
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

         weather_summary <- data.frame(
            x = 1,
            y = 1:7,
            text_summary = c(" ",
                             paste0("Mean seasonal Temp: ",
                                    round(mean(w_dat$TEMP),1), " (C)"),
                             paste0("Mean seasonal RH: ",
                                    round(mean(w_dat$RHUM)), " %"),
                             paste0("Total rainfall: ",
                                    round(sum(w_dat$RAIN),1), " mm"),
                             paste0("Total rainfall days: ",sum(w_dat$RAIN > 0.2)),
                             " "," ")
            )

         w_summary <-
            ggplot(aes(x = x, y = y), data = weather_summary) +
            geom_text(aes(label = text_summary), size = 5) +
            theme_void()+
            theme(text = element_text(size = 15),
                  plot.margin = margin(t=30,l=30, r = 30))

         grid.arrange(Tm, RH, rain, w_summary,ncol = 2)

      })

   # Reproductive modifiers
   r_age <- reactive({
      if(input$R_Age == "Sheathblight"){
         # Sheath Blight
         # Slight increase with plant age
         return(cbind(c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L, 100L, 110L, 120L),
               c(0.84,0.84,0.84,0.84,0.84,0.84,0.84,0.88,0.88,1.0,1.0,1.0,1.0)))}

      if(input$R_Age == "LeafBlast"){
         # Leaf Blast
         # strong decrease in R with plant age
         return(
            cbind(c(0L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L,
                 55L, 60L, 65L, 70L, 75L, 80L, 85L, 90L, 95L, 100L, 105L, 110L, 115L, 120L),
               c(1,1,1, 0.9, 0.8, 0.7, 0.64, 0.59, 0.53,  0.43, 0.32, 0.22,
                 0.16,0.09, 0.03, 0.02, 0.02, 0.02, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01)))}
      if(input$R_Age == "BacterialBlight"){
         # Bacterial blight
         # Decrease with plant age
         return(cbind(
            c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L, 100L, 110L, 120L),
            c(1, 1, 1, 0.9, 0.62, 0.43, 0.41, 0.41, 0.41, 0.41, 0.41, 0.41, 0.41)))}

      if(input$R_Age == "BrownSpot"){
         # Brown Spot
         # Strong increase with plant age
         return(cbind(c(0L, 20L, 40L, 60L, 80L, 100L, 120L),
               c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0)))}

      if(input$R_Age == "Tungro"){
         # Tungro
         # increase with plant age then strong decrease
            return(cbind(c(9,10,13.1111,16.2222,19.3333,22.4444,25.5555,28.6666,31.7777,34.8888,37.9999,40),
                           c(0, 0.13, 0.65, 0.75, 0.83, 0.89, 0.93, 0.97, 1.0, 0.96, 0.93, 0)))}

         grid.arrange(Tm, RH, rain, ncol = 2)

      })

   output$R_age_plot <- renderPlot({plot(x = r_age()[,1],
                                         y = r_age()[,2],
                                        type = "l", lwd = 2,main = expression("Plant age effect on pathogen R"[0]),
                                        xlab = "Plant age (days)", ylab = "R",
                                        xlim = c(0,120),
                                        ylim = c(0,1))})

   output$R_Tm_plot <- renderPlot({plot(x = 1:40,
                                         y = Tm_range(seq(1,40,by = 1),
                                                      Tm_opt = input$temp_opti,
                                                      Tm_max = input$temp_max,
                                                      Tm_min = input$temp_min),
                                         type = "l", lwd = 2,
                                         main = expression("Temperature effect on pathogen R"[0]),
                                         xlab = "Temperature", ylab = "R",
                                         ylim = c(0,1))})


}

# Run the application
shinyApp(ui = ui, server = server)


 #
# get_age_effect <- function(trend, length = 13){
#
#
#
# }
