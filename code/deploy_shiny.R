install.packages(c("rsconnect", "shiny", "shinyhelper", "remotes", "plotly", "httr", "magrittr",
                   "shinybusy"), dependencies = NA)

#library("remotes")
#remotes::install_version("rsconnect", "0.8.29")

# install.packages("devtools")
# devtools::install_github("rstudio/rsconnect")

library("shiny")
library("shinyhelper")
library("rsconnect")
library("plotly")
#library("zoo")
library("httr")
library("magrittr")
library("shinybusy")
#library("DT")

# install.packages(c("rsconnect"), dependencies = NA)
# library("rsconnect")

SHINYAPPS_NAME = Sys.getenv("SHINYAPPS_NAME")
SHINYAPPS_TOKEN = Sys.getenv("SHINYAPPS_TOKEN")
SHINYAPPS_SECRET = Sys.getenv("SHINYAPPS_SECRET")

setAccountInfo(name=SHINYAPPS_NAME, token=SHINYAPPS_TOKEN, secret=SHINYAPPS_SECRET)
deployApp(appDir = "./respinow_viz", appName = "respinow_viz", forceUpdate = TRUE)
