#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
dat_example <- read.csv("../data/truth/truth_seasonal_influenza.csv", colClasses = c("date" = "Date"))
locations <- unique(dat_example$location)
age_groups <- unique(dat_example$age_group)
data_versions <- sort(unique(dat_example$date), decreasing = TRUE)
data_versions <- as.character(data_versions[data_versions <= Sys.Date()])


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Explore RESPINOW data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("select_disease", label = "Select disease",
                        choices = c("Pneumococcal disease" = "pneumococcal_disease",
                                    "RSV infection" =  "rsv_infection",
                                    "Seasonal influenza" = "seasonal_influenza")),
            selectizeInput("select_max_data_version", label = "Show data versions up to",
                           choices = data_versions),
            selectInput("select_location", label = "Select location",
                           choices = locations),
            selectInput("select_age_group", label = "Select age group",
                        choices = age_groups)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
