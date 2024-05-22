install.packages(c("shiny", "shinyhelper", "remotes", "plotly", "zoo", "httr", "magrittr",
                   "shinybusy", "DT"), dependencies = TRUE)

library("remotes")
remotes::install_version("rsconnect", "0.8.29")

library("shiny")
library("shinyhelper")
library("rsconnect")
library("plotly")
library("zoo")
library("httr")
library("magrittr")
library("shinybusy")
library("DT")


SHINYAPPS_NAME = Sys.getenv("SHINYAPPS_NAME")
SHINYAPPS_TOKEN = Sys.getenv("SHINYAPPS_TOKEN")
SHINYAPPS_SECRET = Sys.getenv("SHINYAPPS_SECRET")

setAccountInfo(name=SHINYAPPS_NAME, token=SHINYAPPS_TOKEN, secret=SHINYAPPS_SECRET)
deployApp(appDir = "./respinow_viz", appName = "respinow_viz")
