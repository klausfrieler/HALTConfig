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
      p(" This is Version 1.0 for HALT v0.10.0 and higher"),
      p("Thank you for your interest."),
      p("Kilian Sander, Yves Wycisk"), style = "width:60%;text-align:justify")
}

input_width <- 300
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
  
   # Application title
   titlePanel("HALT Configurator & Calculator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        tags$style("#mode:{background-color: #72b573}"),
        selectInput("mode", "Mode:", c("A priori" = "config", "Post-hoc" = "post_hoc")), 
        selectInput("device", "Target Device:",
                    c("Headphone" = "HP", "Loudspeaker" = "LS")),
        selectInput("volume_level", "Volume Level:",
                    c("-8.4 LUFS", "-20.0 LUFS")),
        #checkboxInput("mode", "Post Hoc", TRUE),
        textInput("participants", "Participants with target device:", value = 300, width = input_width),
        textInput("baserate_hp", "Base Rate / Prevalence for Headphones:", value = round(211.0/1194.0, 2), width = input_width),
        
        #checkboxInput("SCC", "SCC", FALSE),
        #textInput("change_rate", "Change Rate:", value = 0, width = input_width),
        textInput("min_prob", "Minimum Probability  for participants with target device:", value = .80, width = input_width),
        
        textInput("tolerance", "Tolerance:", value = 0,  width = input_width),
        selectInput("lr_disc", "Left-Right Discrimination Behaviour:",
                    c("EX" = TRUE, "SAVE" = FALSE), selected = "ex"),
        selectInput("mono_inter", "Mono/Interchanged CH Behaviour:",
                    c("EX" = TRUE, "SAVE" = FALSE), selected = "ex"),
        selectInput("screening", "Screening Behavior:",
                    c("EX" = TRUE, "SAVE" = FALSE), selected = "save"),
        textInput("max_loops", "Loop Threshold:", value = 2, width = input_width),
        
        width = 2
      ),      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type ="tabs",
          tabPanel("Introduction",
           p(htmlOutput("introduction")),
          ),
          tabPanel("Output", 
                   h4("A Priori Estimation", style = "margin-left:0px;margin-top:30px"),
                   div(DT::dataTableOutput("selection_output")),
                   h4("Description", style = "margin-left:0px;margin-top:30px"),
                   p(htmlOutput("basic_output")),
                   h4("Configuration", style = "margin-left:0px;margin-top:30px"),
                   div(
                     tableOutput("config_output"),
                     style = "background-color: #9ad6db;border: solid 1px; padding: 10px;  border-radius: 5px;"),
                   div(downloadButton("download_config", "Download HALT config file", 
                                      style = "margin:20px;font-size:large;background-color:#ede2a4")),
                   ),
          tabPanel("Info", 
                   #p(htmlOutput("input_info"))
                   tableOutput("parameter_description")
                   )
        )
      )
   )
)

rename_apriori_est <- function(a_priori){
  names(a_priori) <- c("Method", "Code", "A", "B", "C", "HP Rate", "LS Rate", "HP Prev.", "LS Prev.", 
                       "Utility", "Sample Size", "Expected Part.", "Min Quality %")
  a_priori
}
make_config <- function(input, row){
  a_priori <- HALT::a_priori_est(baserate_hp = as.numeric(input$baserate_hp), 
                                 device = input$device, 
                                 min_number = as.numeric(input$participants), 
                                 min_prob =  as.numeric(input$min_prob), 
                                 tolerance = as.numeric(input$tolerance))
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
                              #use_scc = as.logical(input$SCC),
                              use_scc = FALSE,
                              loop_exclude = as.numeric(input$max_loops),
                              lr_img_exclude = as.logical(input$lr_disc),
                              lr_audio_exclude = as.logical(input$mono_inter),
                              devices_exclude = as.logical(input$screening), 
                              volume_level = input$volume_level) 
  attr(config, "class") <- "list"
  config <- config %>% as.data.frame() 
  names(config) <- c("Method Code", "A", "B", "C", "Base Rate", "SCC", "Max. Loops", "LR Exclude (img)", 
                     "LR Exclude (audio)", "Exclude by Device", "Volume Level", "Device")
  config
}

# Define server logic required to draw a plot
server <- function(input, output, session) {
   message("*** STARTING APP***")
   output$introduction <- renderUI({
     get_intro_text()
   })
   output$basic_output <- renderUI({
     selection <- HALT::a_priori_est(baserate_hp = as.numeric(input$baserate_hp), 
                                     device = input$device, 
                                     min_number = as.numeric(input$participants), 
                                     min_prob =  as.numeric(input$min_prob), 
                                     tolerance = as.numeric(input$tolerance)) %>% 
       mutate_if(is.numeric, round, 2)
     shiny::p(attr(selection, "explanation"), 
              style = "width:50%;background-color:#e6c5cd;border: solid 1px;padding: 10px;  border-radius: 5px;")
   })
   
   output$selection_output <- DT::renderDataTable({
     selection <- HALT::a_priori_est(baserate_hp = as.numeric(input$baserate_hp), 
                                     device = input$device, 
                                     min_number = as.numeric(input$participants), 
                                     min_prob =  as.numeric(input$min_prob), 
                                     tolerance = as.numeric(input$tolerance)) %>% 
       mutate_if(is.numeric, round, 2)
     DT::datatable(selection %>% rename_apriori_est(), options = list(lengthMenu = c(5, 30, 50), pageLength = 30), selection = "single")
   }, width = "100%"
   )
   # output$config_output <- DT::renderDataTable({
   #   #print(input$selection_output_rows_selected)
   #   config <- make_config(input, input$selection_output_rows_selected)
   #   #print(config)
   #   tabl(config, options = list(lengthMenu = c(5, 30, 50), pageLength = 30), selection = "single")
   #   
   #   DT::datatable(config, options = list(lengthMenu = c(5, 30, 50), pageLength = 30), selection = "single")
   # }, width = "100%"
   # )

   output$config_output <- renderTable({
      make_config(input, input$selection_output_rows_selected) %>% mutate(A = as.integer(A), B = as.integer(B), C = as.integer(C))
   }, width = "100%"
   )
   
   output$parameter_description <- renderTable({
     parameter_description %>% select(-parameter) %>% rename("Parameter" = label, "Description" = description)
   })
   
   # Downloadable csv of selected dataset ----
   output$download_config <- downloadHandler(
     filename = "HALT_config.csv",
     content = function(file){
       config <- make_config(input, input$selection_output_rows_selected)
       write.table(as.data.frame(config), file, sep = ";", row.names = FALSE, quote = FALSE)
     }
   )   
}

# Run the application 
shinyApp(ui = ui, server = server)

