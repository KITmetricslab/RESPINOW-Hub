# setwd("/home/johannes/Documents/RESPINOW/RESPINOW-Hub/respinow_viz")

library(shiny)
library(plotly)
library(shinyhelper)
library(magrittr)
library(shinybusy)
library(DT)
library(MASS)

# get auxiliary functions:
source("functions.R")

local <- TRUE
if(local){
    # get vector of model names:
    dat_models <- read.csv("plot_data/other/list_models.csv")
    # available versions of truth_data:
    available_data_versions <- sort(read.csv("plot_data/other/list_data_versions.csv", colClasses = c("date" = "Date"))$date)
    # available plot_data with nowcasts:
    available_nowcast_dates <- date_from_filename(sort(read.csv("plot_data/other/list_plot_data.csv")$file))
    
}else{
    # available versions of truth_data:
    # available_dates <- sort(read.csv("https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/main/nowcast_viz_de/plot_data/other/list_dates.csv", colClasses = c("date" = "Date"))$date)
    # available plot_data with nowcasts:
    available_nowcast_dates <- date_from_filename(sort(read.csv("https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/main/nowcast_viz_de/plot_data/other/list_plot_data.csv")$file))
    # get vector of model names:
    dat_models <- read.csv("https://raw.githubusercontent.com/KITmetricslab/hospitalization-nowcast-hub/main/nowcast_viz_de/plot_data/other/list_models.csv")
}

# map between codes for federal states and their human-readable names
list_locations <- read.csv("plot_data/other/list_locations_survstat.csv")
locations <- list_locations$location
names(locations) <- list_locations$location_long

style_explanation <- "font-size:13px;"

# check whether a disclaimer for a missing nowcast is needed:
update_available <- ((Sys.Date()) %in% available_nowcast_dates)
time <- as.POSIXct(Sys.time(), tz = "CET")
disclaimer_necessary <- ifelse((!update_available) & (format(time, format = "%H") >= 15), "true", "false")
# (a string to be added in a JS command)

# Define UI for application
shinyUI(fluidPage(
    
    # Application title
    conditionalPanel("input.select_language == 'DE'", 
                     titlePanel("RespiHub: Nowcasting respiratorischer Erreger in Deutschland (Beta)")),
    conditionalPanel("input.select_language == 'EN'", 
                     titlePanel("RespiHub: Nowcasting of respiratory pathogens in Germany (Beta)")),
    
    br(),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            radioButtons("select_language", label = "Sprache / language",
                         choices = c("Deutsch" = "DE", "English" = "EN"), inline = TRUE),
            conditionalPanel("input.select_language == 'DE'", strong("Datenstand")),
            conditionalPanel("input.select_language == 'EN'", strong("Data version")),
            div(style="display: inline-block;vertical-align:top;", actionButton("skip_backward", "<")),
            div(style="display: inline-block;vertical-align:top;width:200px", 
                dateInput("select_date", label = NULL, value = max(available_nowcast_dates),
                          min = min(available_data_versions), max = max(available_data_versions),
                          daysofweekdisabled = 1:6)), # only Sundays can be selected
            div(style="display: inline-block;vertical-align:top;", actionButton("skip_forward", ">")),
            selectizeInput("select_pathogen",
                           label = "Krankheit / Indikator",
                           choices = c("Saisonale Influenza (SurvStat)" = "survstat-influenza",
                                       "RSV (SurvStat)" = "survstat-rsv",
                                       "Pneumokokken (SurvStat)" = "survstat-pneumococcal",
                                       "Saisonale Influenza (NRZ)" = "nrz-influenza",
                                       # "Saisonale Influenza Tests (NRZ)" = "nrz-influenza-tests",
                                       "RSV (NRZ)" = "nrz-rsv",
                                       # "RSV Tests (NRZ)" = "nrz-rsv-tests",
                                       "SARI (ICOSARI)" = "icosari-sari"),
                           width = "300px"),
            conditionalPanel("input.select_language == 'DE'",
                             p("Nowcasts werden täglich gegen 13:00 aktualisiert, können aber verspätet sein falls Daten des RKI verzögert veröffentlicht werden. Falls ein Nowcast für das gewählte Datum nicht vorliegt wird der aktuellste Nowcast der letzten 7 Tage gezeigt.",
                               style = "font-size:11px;")),
            conditionalPanel("input.select_language == 'EN'",
                             p("Nowcasts are updated on daily at around 1pm, but may be delayed if input data from RKI are published later than usually. If a nowcast is not available for the chosen date, the most current nowcast from the last 7 days is shown.",
                               style = "font-size:11px;")),
            radioButtons("select_stratification", label = "Stratifizierung",
                         choices = c("Bundesland" = "state", "Altersgruppe" = "age"), inline = TRUE),
            conditionalPanel("input.select_stratification == 'age'",
                             conditionalPanel("input.select_language == 'DE'", strong("Altersgruppe")),
                             conditionalPanel("input.select_language == 'EN'", strong("Age group")),
                             selectizeInput("select_age",
                                            label = NULL,
                                            choices = c("0+" = "00+",
                                                        "0 - 4" = "00-04",
                                                        "5 - 14" = "05-14",
                                                        "15 - 34" = "15-34",
                                                        "35 - 59" = "35-59",
                                                        "60 - 79" = "60-79",
                                                        "80+" = "80+"), width = "200px")),
            conditionalPanel("input.select_stratification == 'state'",
                             selectizeInput("select_state",
                                            label = "Bundesland",
                                            choices = locations, width = "200px")),
            conditionalPanel("input.select_language == 'DE'",
                             p("Beachten Sie beim Vergleich der Altersgruppen bzw. der Bundesländer die unterschiedlichen Skalen in der Grafik.",
                               style = "font-size:11px;")),
            conditionalPanel("input.select_language == 'EN'",
                             p("When comparing age groups or Bundesländer please note that the scales in the figure differ.",
                               style = "font-size:11px;")),
            radioButtons("select_plot_type", label = "Grafische Darstellung:", 
                         choices = c("Interaktiv für mehrere Modelle" = "interactive",
                                     "Überblick für ein Modell" = "overview"), inline = TRUE),
            
            checkboxInput("show_truth_frozen", label = "Zeitreihe eingefrorener Werte", 
                          value = FALSE),
            conditionalPanel("input.select_language == 'DE'", strong("Weitere Optionen")),
            conditionalPanel("input.select_language == 'EN'", strong("More options")),
            strong(checkboxInput("show_additional_controls", label = "Zeige weitere Optionen", 
                                 value = FALSE)),
            
            conditionalPanel("input.show_additional_controls",
                             radioButtons("select_max_lag", label = "Maximaler Meldeverzug",
                                          choices = c("4" = "4", "beliebig" = "any"),
                                          selected = "4", inline = TRUE),
                             radioButtons("select_scale", label = "Anzeige", 
                                          choices = c("pro 100.000" = "per 100.000",
                                                      "absolute Zahlen" = "absolute counts"),
                                          selected = "per 100.000", inline = TRUE),
                             radioButtons("select_log", label = NULL, 
                                          choices = c("natürliche Skala" = "natural scale",
                                                      "log-Skala"  ="log scale"), 
                                          selected = "natural scale", inline = TRUE),
                             radioButtons("select_point_estimate", label = "Punktschätzer:", 
                                          choices = c("Median" = "median", "Erwartungswert" = "mean"),
                                          selected = "median", inline = TRUE),
                             radioButtons("select_interval", label = "Unsicherheitsintervall", 
                                          choices = c("95%" = "95%", "50%" = "50%", "keines" = "none"), selected = "95%", inline = TRUE),
                             
                             conditionalPanel("input.select_language == 'DE'", strong("Weitere Anzeigeoptionen")),
                             conditionalPanel("input.select_language == 'EN'", strong("Further display options")),
                             
                             checkboxInput("show_table", label = "Zeige Übersichtstabelle (noch nicht verfügbar)", 
                                           value = FALSE),
                             checkboxInput("show_truth_by_reporting", label = "Zeitreihe nach Erscheinen in RKI-Daten (noch nicht verfügbar)", 
                                           value = FALSE),
                             checkboxInput("show_retrospective_nowcasts", label = "Nachträglich erstellte Nowcasts zeigen", 
                                           value = FALSE)
            ),
            conditionalPanel("input.select_language == 'DE'",
                             helper(strong("Erklärung der Kontrollelemente"),
                                    content = "erklaerung",
                                    type = "markdown",
                                    size = "m")),
            conditionalPanel("input.select_language == 'EN'",
                             helper(strong("Explanation of control elements"),
                                    type = "markdown",
                                    content = "explanation",
                                    size = "m")),
        ),
        
        mainPanel(
            add_busy_spinner(spin = "fading-circle"),
            conditionalPanel("input.select_language == 'DE'",
                             p(strong("Diese Seite ist derzeit in einer Pilotphase und dient nur zum wissenschaftlichen Austausch. Die Analysen werden noch nicht regelmäßig aktualisiert. Derzeit sind die Nowcasts außerdem durch Meldeartefakte zu Weihnachten beeinträchtigt.")),
                             p("Diese Plattform vereint Nowcasts für ausgewählte epidemiologische Indikatoren zu respiratorischen Erregern in Deutschland. Sie ist Teil des Projektes", a('RespiNow', href="https://respinow.de/"), ". Künftig sollen verschiedene Verfahren zusammengeführt werden, derzeit ist jedoch erst ein Modell operationell."),
                             p("Alle derzeit dargestellten Daten stammen aus dem", a("RKI SurvStat", href = "https://survstat.rki.de/"), "Routinebüberwachunssystem. Andere Datenquellen sollen demnächst hinzugefügt werden."),
                             p("Bei Unregelmäßigkeiten im Meldeprozess durch z.B. starke Belastung des Gesundheitssystems oder Feiertage kann die Verlässlichkeit der Nowcasts beeinträchtigt werden.")
            ),
            conditionalPanel("input.select_language == 'EN'",
                             p(strong("This website is currently in a pilot phase and serves purely for scientific exchange. The analyses are not yet updated regularly.")),
                             p("This platform unites nowcasts of selected epidemiological indicators on respiratory diseases in Germany, with the goal of providing reliable assessments of recent trends. We aim to provide results from multiple independently run models, but at the current stage only one is already operational. This project is part of the consortium", a('RespiNow', href="https://respinow.de/"), "."),
                             p("All currently displayed data come from the", a("RKI SurvStat", href = "https://survstat.rki.de/"), "routine surveillance system. Other data sources shall be added shortly."),
                             p("If there are irregularities in the reporting process due to, for example, high burdens on the health care system or holidays, the nowcasts may be less reliable.")
            ),
            
            #             conditionalPanel("input.select_language == 'DE'",
            #                              p("??ber den Jahreswechsel kann es zu Verz??gerungen bei der Erstellung der Nowcasts kommen. Au??erdem ist zu erwarten, dass sich die Verz??ge, mit denen Hospitalisierungen gemeldet werden w??hrend dieser Zeit anders verhalten als im Rest des Jahres. Dies kann die Verl??sslichkeit der Nowcasts vermindern und diese sollten mit besonderer Vorsicht interpretiert werden."),
            #             ),
            #             conditionalPanel("input.select_language == 'EN'",
            #                              p("During the holiday period delays may occur in the creation of nowcasts. Moreover, the delays with which hospitalizations get reported are expected to behave differently than during the rest of the year. This can reduce the reliability of nowcasts, which should be interpreted with particular care."),
            #             ),
            
            conditionalPanel(paste("input.select_language == 'DE' &", disclaimer_necessary),
                             strong("Nowcasts werden gewöhnlich gegen 13:00 aktualisiert, jedoch scheint für den heutigen Tag noch kein Update vorzuliegen. Eine Aktualisiserung wird u.U. erst morgen wieder verfügbar (dies ist ein automatischer Hinweis)."),
            ),
            conditionalPanel(paste("input.select_language == 'EN' &", disclaimer_necessary),
                             strong("Nowcasts are usually updated at around 1pm, but it seems that there has not yet been an update for today. An update may only become available tomorrow (this is an automated notification)."),
            ),
            conditionalPanel("input.select_pathogen == 'survstat-rsv'",
                             p(strong("Achtung: SurvStat-Daten für RSV sind nur für das Bundesland Sachsen verfügbar. / SurvStat data for RSV are only available for the state of Saxony."))),
            
            conditionalPanel("input.select_plot_type == 'interactive'",
                             plotlyOutput("tsplot", height = "440px")),
            conditionalPanel("input.select_plot_type == 'overview' | input.show_table",
                             div(style="display: inline-block;vertical-align:top;width:400px",
                                 selectInput("select_model", "Modell:",
                                             choices = sort(dat_models$model),
                                             selected = "NowcastHub-MeanEnsemble")),
                             checkboxInput("use_same_ylim", label = "Einheitliche y-Achsenabschnitte in Übersicht", 
                                           value = TRUE)),
            conditionalPanel("input.select_plot_type == 'overview'",
                             plotOutput("overview_plot", height = "1300px")),
            conditionalPanel("input.show_table",
                             div(style="display: inline-block;vertical-align:top;width:200px",
                                 dateInput("select_target_end_date", label = "Meldedatum", value = max(available_nowcast_dates),
                                           min = min(available_nowcast_dates), max = max(available_nowcast_dates))),
                             conditionalPanel("input.select_language == 'DE'",
                                              p("Untenstehende Tabelle fasst die Nowcasts eines gewählten Modells für ein bestimmtes Meldedatum (Zieldatum des Nowcasts) und verschiedene Bundesländer oder Altersgruppen zusammen. Der verwendete Datenstand ist der selbe wie für die grafischen Darstellung."),
                             ),
                             conditionalPanel("input.select_language == 'EN'",
                                              p("This table summarizes the nowcasts made by the selected model for a given Meldedatum (target date of the nowcast) and all German states or age groups. The data version is the same as in the graphical display."),
                             ),
                             DTOutput("table"), 
                             br()),

            
            
            # p(),
            # conditionalPanel("input.select_language == 'DE'",
            #                  p('Das Wichtigste in K??rze (siehe', a('"Hintergrund"', href="https://covid19nowcasthub.de/hintergrund.html"), " f??r Details)"),
            #                  p('- Die 7-Tages-Hospitalisierungsinzidenz ist einer der Leitindikatoren f??r die COVID-19 Pandemie in Deutschland (siehe "Hintergrund" f??r die Definition).', style = style_explanation),
            #                  p("- Aufgrund von Verz??gerungen sind die f??r die letzten Tage ver??ffentlichten rohen Inzidenzwerte stets zu niedrig. Nowcasts helfen, diese Werte zu korrigieren. Sie stellen eine Vorhersage daf??r dar, um wie viel die Hospitalisierungsinzidenz noch nach oben korrigiert werden wird.", style = style_explanation),
            #                  p('- Es gibt unterschiedliche Nowcasting-Verfahren. Diese vergleichen wir hier systematisch und kombinieren sie in einem sogenannten Ensemble-Nowcast.', style = style_explanation),
            #                  p('- Unregelm????igkeiten oder ??berlastungen im Meldeprozess k??nnen die Zuverl??ssigkeit der Nowcasts beeintr??chtigen.', style = style_explanation),
            #                  br(),
            #                  br()
            # ),
            # conditionalPanel("input.select_language == 'EN'",
            #                  p('Short summary (see',  a('"Background"', href="https://covid19nowcasthub.de/background.html"), "for details)"),
            #                  p('- The 7-day hospitalization incidence is one of the main indicators for the assessment of the COVID-19 pandemic in Germany (see "Background" for the definition).', style = style_explanation),
            #                  p("- Due to delays, the published raw incidence values for the last few days are biased downward. Nowcasts can help to correct these, predicting by how much the hospitalization incidence will be corrected.", style = style_explanation),
            #                  p('- A variety of nowcasting methods exist. We systematically compile results based on different methods and combine them into so-called ensemble nowcasts.', style = style_explanation),
            #                  p('- Overstrain of the reporting system and other irregularities and may impair the reliability of the nowcasts.', style = style_explanation),
            #                  br(),
            #                  br()
            # ),
            p(),
            conditionalPanel("input.select_language == 'DE'",
                             p("Die interaktive Visualisierung funktioniert am besten unter Google Chrome und ist nicht für Mobilgeräte optimiert.", style = style_explanation),
                             p("Kontakt: ", a("Lehrstuhl für Statistische Methoden und Ökonometrie", href = "https://statistik.econ.kit.edu/index.php"), 
                               ", Karlsruher Institut für Technologie. Email: johannes.bracher@kit.edu", style = style_explanation)
                             # p("Diese Plattform wird von Mitgliedern des ",
                             #   a("Lehrstuhls f??r ??konometrie und Statistik", href = "https://statistik.econ.kit.edu/index.php"),
                             #   "am Karlsruher Institut f??r Technologie betrieben. Kontakt: forecasthub@econ.kit.edu")
            ),
            conditionalPanel("input.select_language == 'EN'",
                             p("The interactive visualization works best under Google Chrome and is not optimized for mobile devices.", style = style_explanation),
                             p("Contact: ", a("Chair of Statistical Methods and Econometrics", href = "https://statistik.econ.kit.edu/index.php"),
                               "Karlsruhe Institute of Technology. Email: johannes.bracher@kit.edu", style = style_explanation)
                             # p("This platform is run by members of the ",
                             #   a("Chair of Statistics and Econometrics", href = "https://statistik.econ.kit.edu/index.php"),
                             #   "at Karlsruhe Institute of Technology. Contact: forecasthub@econ.kit.edu")
            ),
            # conditionalPanel("input.select_language == 'DE'",
            #                  p(a("covid19nowcasthub.de", href = "https://covid19nowcasthub.de"), " - ",
            #                    a("Lehrstuhl f??r ??konometrie und Statistik, Karlsruher Institut f??r Technologie", href = "https://statistik.econ.kit.edu/index.php"), " - ",
            #                    a("Kontakt", href = "https://covid19nowcasthub.de/contact.html"))
            # ),
            # conditionalPanel("input.select_language == 'EN'",
            #                  p(a("covid19nowcasthub.de", href = "https://covid19nowcasthub.de"), " - ",
            #                    a("Chair of Statistics and Econometrics, Karlsruhe Institute of Technology", href = "https://statistik.econ.kit.edu/index.php"), "-",
            #                    a("Contact", href = "https://covid19nowcasthub.de/contact.html"))
            # )
        )
    )
))
