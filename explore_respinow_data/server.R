#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


setwd("/home/johannes/Documents/Projects/RESPINOW-Hub/explore_respinow_data")
dat_truth <- list()
dat_truth$seasonal_influenza <- read.csv("../data/truth/truth_seasonal_influenza.csv", colClasses = c("date" = "Date"))
dat_truth$rsv_infection <- read.csv("../data/truth/truth_rsv_infection.csv", colClasses = c("date" = "Date"))
dat_truth$pneumococcal_disease <- read.csv("../data/truth/truth_pneumococcal_disease.csv", colClasses = c("date" = "Date"))

# function to compute truth data as of a certain time:
truth_as_of <- function(dat_truth, age_group = "00+", location = "DE", date){
    if(is.null(date)){
        date <- max(dat_truth$date)
    }
    date <- as.Date(date)
    subs <- dat_truth[dat_truth$age_group == age_group &
                          dat_truth$location == location &
                          dat_truth$date <= date, ]
    matr <- subs[, grepl("value_", colnames(subs))]
    matr_dates <- matrix(subs$date, nrow = nrow(matr), ncol = ncol(matr))
    matr_delays <- matrix((1:ncol(matr_dates)) - 1, byrow = TRUE,
                          nrow = nrow(matr), ncol = ncol(matr))
    matr_reporting_date <- matr_dates + 7*matr_delays
    matr[matr_reporting_date > date] <- 0
    data.frame(date = subs$date,
               value = rowSums(matr))
}

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({
        
        dates <- sort(unique(dat_truth$seasonal_influenza$date))
        dates_to_show <- dates
        #dates <- dates[dates < Sys.Date()]
        
        dat <- truth_as_of(dat_truth = dat_truth[[input$select_disease]],
                           age_group = input$select_age_group,
                           location = input$select_location, 
                           date = max(dates_to_show))
        dat <- dat[dat$date <= Sys.Date(), ]
        
        plot(dat$date, dat$value, type = "l", ylim = c(0, max(dat$value)), col = "black",
             xlab = "date", ylab = "weekly new cases")
        legend("topleft", legend = c("current data version",
                                     paste("data version", input$select_max_data_version),
                                     "other data versions"),
               col = c("black", "red", "lightgrey"), lwd = c(3, 2, 1), bty = "n")
        
        for(i in seq_along(dates_to_show)){
            dat <- truth_as_of(dat_truth = dat_truth[[input$select_disease]],
                                age_group = input$select_age_group,
                                location = input$select_location, 
                                date = dates[i])
            dat <- dat[dat$date <= Sys.Date(), ]
            
            col <- "red"
            if(dates[i] < input$select_max_data_version) col <- "lightgrey"
            if(dates[i] > input$select_max_data_version) col <- "darkgrey"
            lines(dat$date, dat$value, col = col)
        }
        
        
        dat <- truth_as_of(dat_truth = dat_truth[[input$select_disease]],
                           age_group = input$select_age_group,
                           location = input$select_location, 
                           date = max(dates_to_show))
        dat <- dat[dat$date <= Sys.Date(), ]
        
        lines(dat$date, dat$value, type = "l", col = "black", lwd = 3)
        
        
        dat <- truth_as_of(dat_truth = dat_truth[[input$select_disease]],
                           age_group = input$select_age_group,
                           location = input$select_location, 
                           date = input$select_max_data_version)
        dat <- dat[dat$date <= Sys.Date(), ]
        
        lines(dat$date, dat$value, col = "red", lwd = 2)

    })

})


