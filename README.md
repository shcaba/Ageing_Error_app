# Ageing Error Application to develop ageing error matrices for Stock Synthesis and other uses
Ageing error shiny app using the TMB-based ageing error code from Andre Punt (https://pfmc-assessments.github.io/AgeingError/articles/getting_started.html) 

# Installing libraries 
```R

Use the below code to check to make sure you have the needed libraries:

packages<-c("shiny","shinyFiles","ggplot2","DT","bslib",
"data.table","shinybusy","plotly","remotes","shinyWidgets")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], dependencies = TRUE)
}

For the ageing error package, use one of the following installation commands:

install.packages("AgeingError", repos = c("https://noaa-fisheries-integrated-toolbox.r-universe.dev", "https://cloud.r-project.org"))

remotes::install_github("pfmc-assessments/AgeingError")

```

# Running the Ageing Error App

Running the tool can be accomplished in the following way:

1. Access the repository [Ageing_Error_app](https://github.com/shcaba/Ageing_Error_app), click the `green code button` and choose `Download zip` option. 


2. Extract the downloaded folder **Ageing_Error_app-main** and open the `server.r` or `ui.r` file in RStudio. 

**Obs.** Before running the first time, open both `server.r` and `ui.r` files to make sure all packages are installed. If any package is not installed, RStudio will signal..

in RStudio and push the "`Run App`" button 

I recommend using the "`Run External`" option within the "`Run App`" button 
(see small arrow in button to change options)


3. The application will open and you can begin using it.


