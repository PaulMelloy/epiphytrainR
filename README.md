# epiphytrainR
A shiny app to help visualise and teach epiphytotics and plant disease epidemiology

To run this on your local machine, you will need to have R installed. You can download R from [here](https://cran.r-project.org/). You will also need to have RStudio installed. You can download RStudio from [here](https://www.rstudio.com/products/rstudio/download/).

Install the R packages `remotes` and `shiny`

```R
install.packages("remotes", "shiny")
```

Then run the shiny app using the following code:

```R
shiny::runGitHub(repo = "epiphytrainR", 
                 username = "PaulMelloy",
                 subdir = "epiphytrainR_app")
```
