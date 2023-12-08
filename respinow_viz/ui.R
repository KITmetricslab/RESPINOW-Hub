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
                   choices = c("Deutsch" = "DE", "English" = "EN"), selected = "EN", inline = TRUE),
      conditionalPanel("input.select_language == 'DE'", strong("Datenstand")),
      conditionalPanel("input.select_language == 'EN'", strong("Data version")),
      div(style="display: inline-block;vertical-align:top;", actionButton("skip_backward", "<")),
      div(style="display: inline-block;vertical-align:top;width:200px", 
          dateInput("select_date", label = NULL, value = max(available_nowcast_dates),
                    min = min(available_data_versions), max = max(available_data_versions),
                    daysofweekdisabled = c(1, 2, 3, 5, 6, 7))), # only Sundays can be selected
      div(style="display: inline-block;vertical-align:top;", actionButton("skip_forward", ">")),
      conditionalPanel("input.select_language == 'DE'",
                       helper(strong("Krankheit / Indikator"),
                              content = "indikatoren",
                              type = "markdown",
                              size = "m")),
      conditionalPanel("input.select_language == 'EN'",
                       helper(strong("Disease / indicator"),
                              type = "markdown",
                              content = "indicators",
                              size = "m")),
      selectizeInput("select_pathogen",
                     label = NULL,
                     choices = c("SARI (ICOSARI)" = "icosari-sari",
                                 "Saisonale Influenza (SurvStat)" = "survstat-influenza",
                                 "RSV (SurvStat)" = "survstat-rsv",
                                 "Invasive Pneumokokken (SurvStat)" = "survstat-pneumococcal",
                                 "ARE (AGI)" = "agi-are",
                                 "Saisonale Influenza (NRZ)" = "nrz-influenza",
                                 # "Saisonale Influenza Tests (NRZ)" = "nrz-influenza-tests",
                                 "RSV (NRZ)" = "nrz-rsv",
                                 # "RSV Tests (NRZ)" = "nrz-rsv-tests",
                                 "Saisonale Influenza (CVN)" = "cvn-influenza",
                                 "RSV (CVN)" = "cvn-rsv",
                                 "Invasive Pneumokokken (CVN)" = "cvn-pneumococcal"
                     ),
                     width = "300px"),
      conditionalPanel("input.select_language == 'DE'",
                       p("Nowcasts und Vorhersagen werden je nach Datenverfügbarkeit am Donnerstag oder Freitag aktualisiert.",
                         style = "font-size:11px;")),
      conditionalPanel("input.select_language == 'EN'",
                       p("Nowcasts and predictions are updated on Thursdays or Fridays, depending on data availability.",
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
                       p("Nicht alle Zeitreihen sind nach Bundesländern und Altersgruppen aufgeschlüsselt verfügbar. Beachten Sie beim Vergleich der Altersgruppen bzw. der Bundesländer die unterschiedlichen Skalen in der Grafik.",
                         style = "font-size:11px;")),
      conditionalPanel("input.select_language == 'EN'",
                       p("Not all time series are available per Bundesland and age group. When comparing age groups or Bundesländer please note that the scales in the figure differ.",
                         style = "font-size:11px;")),
      # radioButtons("select_plot_type", label = "Grafische Darstellung:", 
      #              choices = c("Interaktiv, mehrere Modelle" = "interactive",
      #                          "Detailliert, ein Modell" = "overview"), inline = TRUE),
      
      
      conditionalPanel("input.select_language == 'DE'", strong("Weitere Optionen")),
      conditionalPanel("input.select_language == 'EN'", strong("More options")),
      strong(checkboxInput("show_additional_controls", label = "Öffne weitere Optionen", 
                           value = FALSE)),
      
      conditionalPanel("input.show_additional_controls",
                       radioButtons("select_max_lag", label = "Maximaler Meldeverzug",
                                    choices = c("4 Wochen" = "4", "beliebig" = "any"),
                                    selected = "4", inline = TRUE),
                       radioButtons("select_scale", label = "Anzeige", 
                                    choices = c("pro 100.000" = "per 100.000",
                                                "absolute Zahlen" = "absolute counts"),
                                    selected = "per 100.000", inline = TRUE),
                       radioButtons("select_log", label = NULL, 
                                    choices = c("natürliche Skala" = "natural scale",
                                                "log-Skala"  ="log scale"), 
                                    selected = "natural scale", inline = TRUE),
                       checkboxInput("show_truth_frozen", label = "Zeitreihe eingefrorener Werte", 
                                     value = FALSE),
                       radioButtons("select_point_estimate", label = "Punktschätzer:", 
                                    choices = c("Median" = "median", "Erwartungswert" = "mean"),
                                    selected = "median", inline = TRUE),
                       radioButtons("select_interval", label = "Unsicherheitsintervall", 
                                    choices = c("95%" = "95%", "50%" = "50%", "keines" = "none"), selected = "95%", inline = TRUE),
                       # not showing this stuff as not implemented currently (but leaving bases in)
                       conditionalPanel("0 == 1",
                                        conditionalPanel("input.select_language == 'DE'", strong("Weitere Anzeigeoptionen")),
                                        conditionalPanel("input.select_language == 'EN'", strong("Further display options")),
                                        checkboxInput("show_table", label = "Zeige Übersichtstabelle (noch nicht verfügbar)", 
                                                      value = FALSE),
                                        checkboxInput("show_truth_by_reporting", label = "Zeitreihe nach Erscheinen in RKI-Daten (noch nicht verfügbar)", 
                                                      value = FALSE),
                                        checkboxInput("show_retrospective_nowcasts", label = "Nachträglich erstellte Nowcasts zeigen", 
                                                      value = FALSE)
                       )
                       
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
                       p(strong("Diese Seite ist derzeit in einer Pilotphase und dient nur zum wissenschaftlichen Austausch. Die Analysen werden noch nicht regelmäßig aktualisiert. Nicht alle gezeigten Analysen wurden in Echtzeit erstellt.")),
                       # p("Diese Plattform vereint Nowcasts für ausgewählte epidemiologische Indikatoren zu respiratorischen Erregern in Deutschland. Sie ist Teil des Projektes", a('RespiNow', href="https://respinow.de/"), " innerhalb des ", a("MONID Netzwerks", href = "https://webszh.uk-halle.de/monid/"), ". Künftig sollen verschiedene Verfahren zusammengeführt werden, derzeit ist jedoch erst ein Modell operationell."),
                       # p("Die Datenquellen sind am Ende der Seite angegeben."),
                       # p("Bei Unregelmäßigkeiten im Meldeprozess durch z.B. starke Belastung des Gesundheitssystems oder Feiertage kann die Verlässlichkeit der Nowcasts beeinträchtigt werden.")
      ),
      conditionalPanel("input.select_language == 'EN'",
                       p(strong("This website is currently in a pilot phase and serves purely for scientific exchange. The analyses are not yet updated regularly. Not all displayed results were computed in real time.")),
                       # p("This platform unites nowcasts of selected epidemiological indicators on respiratory diseases in Germany, with the goal of providing reliable assessments of recent trends. We aim to provide results from multiple independently run models, but at the current stage only one is already operational. This project is part of the consortium", a('RespiNow', href="https://respinow.de/"), "within the", a("MONID Network", href = "https://webszh.uk-halle.de/monid/"), "."),
                       # p("Data sources are indicated at the bottom of the page."),
                       # p("If there are irregularities in the reporting process due to, for example, high burdens on the health care system or holidays, the nowcasts may be less reliable.")
      ),
      
      #             conditionalPanel("input.select_language == 'DE'",
      #                              p("??ber den Jahreswechsel kann es zu Verz??gerungen bei der Erstellung der Nowcasts kommen. Au??erdem ist zu erwarten, dass sich die Verz??ge, mit denen Hospitalisierungen gemeldet werden w??hrend dieser Zeit anders verhalten als im Rest des Jahres. Dies kann die Verl??sslichkeit der Nowcasts vermindern und diese sollten mit besonderer Vorsicht interpretiert werden."),
      #             ),
      #             conditionalPanel("input.select_language == 'EN'",
      #                              p("During the holiday period delays may occur in the creation of nowcasts. Moreover, the delays with which hospitalizations get reported are expected to behave differently than during the rest of the year. This can reduce the reliability of nowcasts, which should be interpreted with particular care."),
      #             ),
      
      
      # conditionalPanel("input.select_plot_type == 'interactive'",
      plotlyOutput("tsplot", height = "440px"),
      #                  ),
      # conditionalPanel("input.select_plot_type == 'overview' | input.show_table",
      #                  div(style="display: inline-block;vertical-align:top;width:400px",
      #                      selectInput("select_model", "Modell:",
      #                                  choices = sort(dat_models$model),
      #                                  selected = "NowcastHub-MeanEnsemble")),
      #                  checkboxInput("use_same_ylim", label = "Einheitliche y-Achsenabschnitte in Übersicht", 
      #                                value = TRUE)),
      # conditionalPanel("input.select_plot_type == 'overview'",
      #                  plotOutput("overview_plot", height = "1300px")),
      # conditionalPanel("input.show_table",
      #                  div(style="display: inline-block;vertical-align:top;width:200px",
      #                      dateInput("select_target_end_date", label = "Meldedatum", value = max(available_nowcast_dates),
      #                                min = min(available_nowcast_dates), max = max(available_nowcast_dates))),
      #                  conditionalPanel("input.select_language == 'DE'",
      #                                   p("Untenstehende Tabelle fasst die Nowcasts eines gewählten Modells für ein bestimmtes Meldedatum (Zieldatum des Nowcasts) und verschiedene Bundesländer oder Altersgruppen zusammen. Der verwendete Datenstand ist der selbe wie für die grafischen Darstellung."),
      #                  ),
      #                  conditionalPanel("input.select_language == 'EN'",
      #                                   p("This table summarizes the nowcasts made by the selected model for a given Meldedatum (target date of the nowcast) and all German states or age groups. The data version is the same as in the graphical display."),
      #                  ),
      #                  DTOutput("table"), 
      #                  br()),
      
      
      
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
                       p(strong("Kurzerklärung:"), "Die schwarze Linie zeigt den Datenstand zum Zeitpunkt des letzten Updates dieser Webseite (meist Donnerstag / Freitag). Die jeweils letzten Werte dieser Daten sind
                               meist unvollständig und werden noch nach oben korrigiert. Farbige Linien und Bänder zeigen eine Vorhersage dieser Korrektur sowie ggf. des weiteren Verlaufs.", style = style_explanation),
                       p(strong("Besonderheiten der gewählten Datenquelle:"), style = style_explanation)
                       # p("Kontakt: ", a("Lehrstuhl für Statistische Methoden und Ökonometrie", href = "https://statistik.econ.kit.edu/index.php"), 
                       #  ", Karlsruher Institut für Technologie. Email: johannes.bracher@kit.edu", style = style_explanation)
                       # p("Diese Plattform wird von Mitgliedern des ",
                       #   a("Lehrstuhls f??r ??konometrie und Statistik", href = "https://statistik.econ.kit.edu/index.php"),
                       #   "am Karlsruher Institut f??r Technologie betrieben. Kontakt: forecasthub@econ.kit.edu")
      ),
      conditionalPanel("input.select_language == 'EN'",
                       p(strong("Brief explanation:"), "The black line shows the data as available at the time of last update of this website (usually Thursday / Friday). The most recent values of these data are
                               typically incomplete and will still be corrected upwards. Coloured lines show the anticipated correction and, where applicable, the predicted future course.", style = style_explanation,
                         ),
                       p(strong("Particularities of the chosen data source:"), style = style_explanation)
                       # p("Contact: ", a("Chair of Statistical Methods and Econometrics", href = "https://statistik.econ.kit.edu/index.php"),
                       #    "Karlsruhe Institute of Technology. Email: johannes.bracher@kit.edu", style = style_explanation)
                       # p("This platform is run by members of the ",
                       #   a("Chair of Statistics and Econometrics", href = "https://statistik.econ.kit.edu/index.php"),
                       #   "at Karlsruhe Institute of Technology. Contact: forecasthub@econ.kit.edu")
      ),
      
      
      conditionalPanel("input.select_pathogen == 'survstat-rsv' & input.select_language == 'DE'",
                       p("- SurvStat-Daten für RSV sind nur für das Bundesland Sachsen verfügbar.", style = style_explanation)),
      
      conditionalPanel("input.select_pathogen == 'survstat-rsv' & input.select_language == 'EN'",
                       p("- SurvStat data for RSV are only available for the state of Saxony.", style = style_explanation)),
      
      conditionalPanel("(input.select_pathogen == 'survstat-influenza' | input.select_pathogen == 'survstat-pneumococcal' | input.select_pathogen == 'survstat-rsv') & input.select_language == 'DE'",
                       p("- SurvStat-Daten enthalten am Donnerstag bereits Einträge für die laufende Woche, die aber stark unvollständig sind.", style = style_explanation)),
      
      conditionalPanel("(input.select_pathogen == 'survstat-influenza' | input.select_pathogen == 'survstat-pneumococcal' | input.select_pathogen == 'survstat-rsv') & input.select_language == 'EN'",
                       p("- On Thursdays, SurvStat data already contain entries for the current week, but these are strongly incomplete.", style = style_explanation)),
      
      
      conditionalPanel("input.select_language == 'DE'",
                       p("Die interaktive Visualisierung funktioniert am besten unter Google Chrome und ist nicht für Mobilgeräte optimiert.", style = style_explanation)),
      conditionalPanel("input.select_language == 'EN'",
                       p("The interactive visualization works best under Google Chrome and is not optimized for mobile devices.", style = style_explanation))
      
      # conditionalPanel("input.select_language == 'DE'",
      #                  p("Datenquellen:", style = style_explanation),
      #                  p("AGI: ", a("Wochenberichte Arbeitsgemeinschaft Influenza", href = "https://influenza.rki.de/Saisonbericht.aspx"), 
      #                    "- CVN: ", a("Clinical Virology Network", href = "https://clinical-virology.net/en"), 
      #                    "- ICOSARI: ", a("Robert Koch Institut", href = "https://github.com/robert-koch-institut/ARE-Konsultationsinzidenz"), 
      #                    "- NRZ: ", a("Virologische Surveillance, Nationales Referenzzentrum", href = "https://influenza.rki.de/Diagrams.aspx"), 
      #                    "- SurvStat: ", a("SurvStat@RKI 2.0, Robert Koch Institut", href = "https://survstat.rki.de"), style = style_explanation)
      # ),
      # conditionalPanel("input.select_language == 'EN'",
      #                  p("Data sources:", style = style_explanation),
      #                  p("AGI: ", a("Weekly reports, Arbeitsgemeinschaft Influenza", href = "https://influenza.rki.de/Saisonbericht.aspx"), 
      #                    "- CVN: ", a("Clinical Virology Network", href = "https://clinical-virology.net/en"), 
      #                    "- ICOSARI: ", a("Robert Koch Institut", href = "https://github.com/robert-koch-institut/ARE-Konsultationsinzidenz"), 
      #                    "- NRZ: ", a("Virological surveillance, National Reference Centre", href = "https://influenza.rki.de/Diagrams.aspx"), 
      #                    "- SurvStat: ", a("SurvStat@RKI 2.0, Robert Koch Institut", href = "https://survstat.rki.de"), style = style_explanation)
      # ),
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
      # div(img(src='respinow.png', align = "left", height="25%", width="25%")),
      # div(img(src='monid.jpg', align = "right", height="35%", width="35%"))
    )
  )
))
