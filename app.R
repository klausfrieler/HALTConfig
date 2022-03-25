#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(DT)
library(HALT)

source("util.R")
printf <- function(...) print(sprintf(...))
messagef <- function(...) message(sprintf(...))

default_color <- "lightblue4"

parameter_description <- read.csv("parameter_description.csv", sep = ";", stringsAsFactors = F) %>% as_tibble()

get_intro_text <- function(){
  div(h4("Welcome to the Headphone and Loudspeaker Test (HALT) Configurator & Calculator"), 
      p("This tool determines an optimal screening test combination for headphones and loudspeakers.",
        "It is to be used a priori (i.e., in advance of data collection).",
        "The selected test and other parameters (see Info tab) for setting the HALT can then be saved in a configuration file for later use."),
      p("In the future, we also intend to implement a tool for post-hoc calculations."),
      p("This is Version 1.0 for HALT v0.11.0 and higher"),
      p("Thank you for your interest."),
      p("Kilian Sander, Yves Wycisk"), style = "width:90%;text-align:justify")
}

input_width <- 500

ui <- fluidPage(
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
  # Application title
  titlePanel("HALT Configurator & Calculator"),
  tabsetPanel(type = "tabs",
              tabPanel("Introduction",
                       p(htmlOutput("introduction"))),
              tabPanel("Configuration",
                       sidebarLayout(
                         sidebarPanel(
                           tags$style("#mode:{background-color: #72b573}"),
                           selectInput("volume_level", "Volume level:",
                                       c("-8.4 LUFS", "-20.0 LUFS")),
                           textInput("max_loops", "Loop Threshold:", value = 5, width = input_width),
                           checkboxInput("channel_check", tags$b("Use channel check"), value = TRUE),
                           conditionalPanel(condition = "input.channel_check == 1",
                                            selectInput("lr_disc", "Left-Right Discrimination Behaviour:",
                                                        c("EX" = TRUE, "SAVE" = FALSE), selected = "ex"),
                                            selectInput("mono_inter", "Mono/Interchanged CH Behaviour:",
                                                        c("EX" = TRUE, "SAVE" = FALSE), selected = "ex")),
                           checkboxInput("frequency_check", tags$b("Use frequency check"), value = TRUE),
                           checkboxInput("screening_parts", tags$b("Use screening parts"), value = TRUE),
                           conditionalPanel(condition = "input.screening_parts == 1",
                                            selectInput("conf_auto", "Screening configuration based on:",
                                                        c("prevalence and overall utility" = "auto",
                                                          "sample size estimations" = "est",
                                                          "manual threshold specification" = "manual")),
                                            selectInput("devices", "Target Device:",
                                                        c("Headphone" = "HP", "Loudspeaker" = "LS")),
                                            textInput("baserate_hp", "Base Rate / Prevalence for Headphones:",
                                                      value = round(211.0/1194.0, 2), width = input_width),
                                            conditionalPanel(condition = "input.conf_auto == 'manual'",
                                                             selectInput("combination_method",
                                                                         "Test combination / Evaluation key:",
                                                                         1:18),
                                                             selectInput("A_threshold", "Test A threshold:",1:6),
                                                             selectInput("B_threshold", "Test B threshold", 1:6),
                                                             selectInput("C_threshold", "Test C threshold", 1:6)),
                                            conditionalPanel(condition = "input.conf_auto == 'est'",
                                                             textInput("min_prob",
                                                                       "Minimum Probability  for participants with target device:",
                                                                       value = .80, width = input_width),
                                                             textInput("participants",
                                                                       "Participants with target device:",
                                                                       value = 300, width = input_width),
                                                             textInput("tolerance", "Tolerance:", value = 0,  width = input_width)),
                                            selectInput("devices_exclude", "Screening Behavior:",
                                                        c("EX" = TRUE, "SAVE" = FALSE), selected = "save"),
                                            checkboxInput("use_scc", tags$b("SCC"), value = FALSE),
                                            conditionalPanel(condition = "input.use_scc == 1",
                                                             textInput("changerate", "Switching prevalence / change rate",
                                                                       value = .75))
                                            ),
                           width = 2
                         ),
                         mainPanel(h4("Configuration", style = "margin-left:0px;margin-top:30px"),
                                   div(tableOutput("config_output"),
                                       style = "background-color: #9ad6db;border: solid 1px; padding: 10px;  border-radius: 5px;"),
                                   div(downloadButton("download_config", "Download HALT config file", 
                                                      style = "margin:20px;font-size:large;background-color:#ede2a4")),
                                   width = 10)
                       )
              ),
              tabPanel("Info",
                       tableOutput("parameter_description")
                       )
  )
)

build_config <- function(input, row) {
  if(input$conf_auto == "manual") {
    config <- HALT::make_config(volume_level = input$volume_level,
                                loop_exclude = as.numeric(input$max_loops),
                                channel_check = as.logical(input$channel_check),
                                lr_img_exclude = as.logical(input$lr_disc),
                                lr_audio_exclude = as.logical(input$mono_inter),
                                frequency_check = as.logical(input$frequency_check),
                                screening_parts = as.logical(input$screening_parts),
                                combination_method = input$combination_method,
                                A_threshold = input$A_threshold,
                                B_threshold = input$B_threshold,
                                C_threshold = input$C_threshold,
                                baserate_hp = as.numeric(input$baserate_hp),
                                devices = input$devices,
                                use_scc = as.logical(input$use_scc),
                                devices_exclude = as.logical(input$devices_exclude))
  } else {
    if (input$conf_auto == "est") {
      a_priori <- HALT::a_priori_est(baserate_hp = as.numeric(input$baserate_hp), 
                                     device = input$device, 
                                     min_number = as.numeric(input$participants), 
                                     min_prob =  as.numeric(input$min_prob), 
                                     tolerance = as.numeric(input$tolerance))
    } else {
      #if (input$conf_auto == "auto")
      a_priori <- HALT::tests_pv_utility(baserate_hp = as.numeric(input$baserate_hp))
      a_priori <- a_priori %>% dplyr::arrange(desc(utility)) %>% dplyr::select(-logic_expr)
    }
    if(length(row) == 0){
      row <- 1
    }
    a_priori <- a_priori[row,]
    config <- HALT::make_config(combination_method = a_priori$method_code,
                                A_threshold = a_priori$A,
                                B_threshold = a_priori$B,
                                C_threshold = a_priori$C,
                                baserate_hp = input$baserate_hp,
                                devices = input$device,
                                use_scc = as.logical(input$SCC),
                                loop_exclude = as.numeric(input$max_loops),
                                lr_img_exclude = TRUE,
                                lr_audio_exclude = TRUE,
                                devices_exclude = as.logical(input$devices_exclude), 
                                volume_level = input$volume_level,
                                channel_check = TRUE,
                                screening_parts = TRUE,
                                frequency_check = as.logical(input$frequency_check))
  }
  attr(config, "class") <- "list"
  config <- config %>% as.data.frame()
  names(config) <- c("Volume level", "Max. Loops", "Channel check", "LR Exclude (img)", 
                     "LR Exclude (audio)", "Frequency check", "Screening parts",
                     "Method Code", "A", "B", "C", "Base Rate", "SCC", "Exclude by devcie", "Device")
  config
}

server <- function(input, output, session) {
  message("*** STARTING APP***")
  output$introduction <- renderUI({
    get_intro_text()
  })
  
  output$config_output <- renderTable({
    build_config(input, input$selection_output_rows_selected) %>% mutate(A = as.integer(A), B = as.integer(B), C = as.integer(C), `Max. Loops` = as.integer(`Max. Loops`))
  }, width = "100%"
  )
  
  output$download_config <- downloadHandler(
    filename = "HALT_config.csv",
    content = function(file){
      config <- c("Test", "file")#make_config(input, input$selection_output_rows_selected)
      write.table(as.data.frame(config), file, sep = ";", row.names = FALSE, quote = FALSE)
    }
  )
  
  output$parameter_description <- renderTable({
    parameter_description %>% select(-parameter) %>% rename("Parameter" = label, "Description" = description)
  })
}
  
# Run the application 
shinyApp(ui = ui, server = server)
