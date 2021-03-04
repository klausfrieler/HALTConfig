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
         p("This app allows you to create a configuration files according to your needs, and also to estimate",
           "prevalence values post-hoc"),
      p("Have fun!"),
      p("Your HALT team"), style = "width:50%:text-align:justify")
}

input_width <- 300
# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(tags$style(HTML("#advanced_output:{background-color: #9ad6db}"))),
  
   # Application title
   titlePanel("HALT Configurator & Calculator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        tags$style("#mode:{background-color: #72b573}"),
        selectInput("mode", "Mode:", c("A priori" = "config", "Post-hoc" = "post_hoc")), 
        selectInput("device", "Playback Device:",
                    c("Headphone" = "HP", "Loudspeaker" = "LS")),
        #checkboxInput("mode", "Post Hoc", TRUE),
        textInput("participants", "Participants:", value = 300, width = input_width),
        textInput("base_rate", "Base Rate / Prevalence:", value = round(211.0/1194.0, 2), width = input_width),
        
        checkboxInput("SCC", "SCC", FALSE),
        textInput("change_rate", "Change Rate:", value = 0, width = input_width),
        textInput("min_prob", "Minimum Probability:", value = .80, width = input_width),
        
        textInput("tolerance", "Tolerance:", value = 0,  width = input_width),
        selectInput("lr_disc", "Left-Right Discrimination:",
                    c("EX" = TRUE, "SAVE" = FALSE), selected = "ex"),
        selectInput("mono_inter", "Mono/Interchanged CH:",
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
                   p(htmlOutput("basic_output")),
                   downloadButton("download_config", "Download HALT config file", style = "margin: 20px"),
                   DT::dataTableOutput("advanced_output")),
          tabPanel("Info", 
                   #p(htmlOutput("input_info"))
                   tableOutput("parameter_description")
                   )
        )
      )
   )
)

# Define server logic required to draw a plot
server <- function(input, output, session) {
   message("*** STARTING APP***")

   output$introduction <- renderUI({
     get_intro_text()
   })
   output$basic_output <- renderUI({
     config <- HALT::a_priori_est(min_number = as.numeric(input$participants), 
                                  min_prob =  as.numeric(input$min_prob), 
                                  tolerance = as.numeric(input$tolerance)) %>% 
       mutate_if(is.numeric, round, 2)
     shiny::p(attr(config, "explanation"), style = "width:50%;background-color:#8bc99c;border: solid 1px;padding: 2px")
   })
   
   output$advanced_output <- DT::renderDataTable({
     config <- HALT::a_priori_est(min_number = as.numeric(input$participants), 
                                  min_prob =  as.numeric(input$min_prob), 
                                  tolerance = as.numeric(input$tolerance)) %>% 
       mutate_if(is.numeric, round, 2)
     DT::datatable(config, options = list(lengthMenu = c(5, 30, 50), pageLength = 30))
   }, width = "100%"
   )
   output$parameter_description <- renderTable({
     parameter_description %>% select(-parameter) %>% rename("Parameter" = label, "Description" = description)
   })
   
   # Downloadable csv of selected dataset ----
   output$download_config <- downloadHandler(
     filename = "HALT_config.csv",
     content = function(file){
       a_priori <- HALT::a_priori_est(min_number = as.numeric(input$participants), 
                                    min_prob =  as.numeric(input$min_prob), 
                                    tolerance = as.numeric(input$tolerance))
       row <- input$advanced_output_rows_selectedI[1]
       if(length(row) == 0){
         row <- 1
       }
       a_priori <- a_priori[row,]
       browser()
       config <- HALT::make_config(combination_method = a_priori$method_code,
                                  A_threshold = a_priori$A,
                                  B_threshold = a_priori$B,
                                  C_threshold = a_priori$C,
                                  baserate_hp = a_priori$true_hp_rate,
                                  devices = input$device,
                                  use_scc = as.logical(input$SCC),
                                  loop_exclude = as.numeric(input$max_loops),
                                  lr_img_exclude = as.logical(input$lr_disc),
                                  lr_audio_exclude = as.logical(input$mono_inter),
                                  devices_exclude = as.logical(input$screening)
       )
       attr(config, "class") <- "list"
       write.table(as.data.frame(config), file, sep = ";", row.names = FALSE, quote = FALSE)
     }
   )   
}

# Run the application 
shinyApp(ui = ui, server = server)

