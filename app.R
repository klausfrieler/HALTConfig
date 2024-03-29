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
      p("We also implemented a tool for post-hoc calculations (see Post hoc Sample Size Estimations tab).",
        "With this tool you can estimate the composition of a given sample after a screening procedure was applied."),
      p("This is Version 1.2 for HALT v0.11.0 and higher"),
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
                           h4("Volume Adjustment"),
                           selectInput("volume_level", "Volume Level:",
                                       c("-8.4 LUFS", "-20.0 LUFS")),
                           textInput("max_loops", "Loop Threshold:", value = 5, width = input_width),
                           tags$hr(),
                           h4("Channel Check"),
                           checkboxInput("channel_check", tags$b("Use Channel Check"), value = TRUE),
                           conditionalPanel(condition = "input.channel_check == 1",
                                            selectInput("lr_disc", "Left-Right Discrimination Behavior:",
                                                        c("EX" = TRUE, "SAVE" = FALSE), selected = "ex"),
                                            selectInput("mono_inter", "Mono/Interchanged CH Behavior:",
                                                        c("EX" = TRUE, "SAVE" = FALSE), selected = "ex")),
                           tags$hr(),
                           h4("Frequency Check"),
                           checkboxInput("frequency_check", tags$b("Use Frequency Check"), value = TRUE),
                           tags$hr(),
                           h4("Screening"),
                           checkboxInput("screening_parts", tags$b("Use Screening Parts"), value = TRUE),
                           conditionalPanel(condition = "input.screening_parts == 1",
                                            checkboxInput("use_scc", tags$b("SCC"), value = FALSE),
                                            textInput("baserate_hp", "Base Rate/Prevalence for Headphones:",
                                                      value = round(211.0/1194.0, 2), width = input_width),
                                            conditionalPanel(condition = "input.use_scc == 1",
                                                             textInput("switch_to_target", "Switching prevalence:",
                                                                       value = 0.75)),
                                            selectInput("conf_auto", "Screening configuration based on:",
                                                        c("prevalence and overall utility" = "auto",
                                                          "sample size estimations" = "est",
                                                          "manual threshold specification" = "manual")),
                                            selectInput("devices", "Target Device:",
                                                        c("Headphone" = "HP", "Loudspeaker" = "LS")),
                                            conditionalPanel(condition = "input.conf_auto == 'manual'",
                                                             selectInput("combination_method",
                                                                         "Test Combination/Evaluation Key:",
                                                                         evaluation_keys() %>% setNames(as.numeric(names(.)), .),
                                                                         selected = 11),
                                                             selectInput("A_threshold", "Test A Threshold:",1:6, selected = 5),
                                                             selectInput("B_threshold", "Test B Threshold:", 1:6, selected = 5),
                                                             selectInput("C_threshold", "Test C Threshold:", 1:6, selected = 5)),
                                            conditionalPanel(condition = "input.conf_auto == 'est'",
                                                             textInput("min_prob",
                                                                       "Minimum Probability  for participants with target device:",
                                                                       value = .80, width = input_width),
                                                             textInput("participants",
                                                                       "Participants with target device:",
                                                                       value = 300, width = input_width)#,
                                                             #textInput("tolerance", "Tolerance:", value = 0,  width = input_width)
                                                             ),
                                            selectInput("devices_exclude", "Screening Behavior:",
                                                        c("EX" = TRUE, "SAVE" = FALSE), selected = "save")#,
                                            #checkboxInput("use_scc", tags$b("SCC"), value = FALSE),
                                            #conditionalPanel(condition = "input.use_scc == 1",
                                            #                 textInput("changerate", "Switching prevalence / change rate",
                                            #                           value = .75))
                                            ),
                           width = 2
                         ),
                         mainPanel(h4("Choose a screening test combination:",
                                      style = "margin-left:0px;margin-top:30px"),
                                   div(DT::dataTableOutput("selection_output")),
                                   h3("Your Configuration", style = "margin-left:0px;margin-top:30px"),
                                   div(tableOutput("config_output"),
                                       style = "background-color: #9ad6db;border: solid 1px; padding: 10px;  border-radius: 5px;"),
                                   div(downloadButton("download_config", "Download HALT config file", 
                                                      style = "margin:20px;font-size:large;background-color:#ede2a4")),
                                   # explanation
                                   h3("Explanation"),
                                   div(htmlOutput("explanation"),
                                       style = "width:50%;background-color:#e6c5cd;border: solid 1px;padding: 10px;  border-radius: 5px; margin-bottom: 25px;"),
                                   width = 10)
                       )
              ),
              tabPanel("Configuration Info",
                       tableOutput("parameter_description")
                       ),
              tabPanel("Post hoc Sample Size Estimations",
                       sidebarLayout(
                         sidebarPanel(
                           tags$style("#mode:{background-color: #72b573}"),
                           h4("Used Screening Configuration"),
                           selectInput("post_hoc_target", "Target Device:",
                                       c("Headphones" = "HP",
                                         "Loudspeakers" = "LS")),
                           selectInput("screening_strat", "Screening Strategy",
                                       c("Filter without request" = "fwr",
                                         "Filter after request" = "far",
                                         "Split-convince-compare" = "scc")),
                           selectInput("post_hoc_test_combi", "Test combination/Evaluation key",
                                       evaluation_keys() %>% setNames(as.numeric(names(.)), .),
                                       selected = 11),
                           selectInput("post_hoc_A", "Test A Threshold:",
                                       1:6, selected = 5),
                           selectInput("post_hoc_B", "Test B Threshold:",
                                       1:6, selected = 5),
                           selectInput("post_hoc_C", "Test C Threshold:",
                                       1:6, selected = 5),
                           conditionalPanel(condition = "input.screening_strat != 'far'",
                                            tags$b("(Estimated) Unbiased Base Rate/Prevalence for Headphones:")),
                           conditionalPanel(condition = "input.screening_strat == 'far'",
                                            tags$b("(Estimated) Base Rate/Prevalence for Headphones after Instruction to use the target device:")),
                           textInput("post_hoc_baserate", "",
                                     value = round(211/1194, 2), width = input_width),
                           conditionalPanel(condition = "input.screening_strat != 'scc'",
                                            textInput("post_hoc_samplesize",
                                                      "Number of participants with target device according to screening:",
                                                      value = "10")),
                           conditionalPanel(condition = "input.screening_strat == 'scc'",
                                            textInput("post_hoc_switch_rate",
                                                      "(Estimated) Switching Prevalence:",
                                                      value = 3/4, width = input_width),
                                            textInput("post_hoc_target_selfreport",
                                                      "Number of participants who reported the target device:",
                                                      value = 10),
                                            textInput("post_hoc_target_tested",
                                                      "Number of participants who switched to the target device according to screening:",
                                                      value = 10)),
                           selectInput("post_hoc_mode", "Estimation based on:",
                                       c("Minimum number" = "min_number",
                                         "Minimum probability" = "min_prob")),
                           conditionalPanel(condition = "input.post_hoc_mode == 'min_number'",
                                            sliderInput("post_hoc_min_number", "Minimum number of participants with target device:",
                                                        min = 1, max = 10000, value = 2, step = 1,
                                                        round = TRUE)),
                           conditionalPanel(condition = "input.post_hoc_mode == 'min_prob'",
                                            sliderInput("post_hoc_min_prob",
                                                        "Minimum probability for number of target device users:",
                                                        min = .5, max = .99, value = .8, step = .01,
                                                        round = FALSE)),
                           width = 2),
                         mainPanel(h4("Probabilistic Statements About the Composition of a Sample",
                                      style = "margin-left:0px;margin-top:30px"),
                                   div(textOutput("post_hoc_explanation"),
                                       style = "width:50%;background-color:#e6c5cd;border: solid 1px;padding: 10px;  border-radius: 5px;"),
                                   div(tableOutput("post_hoc_table"),
                                                   style = "background-color: #9ad6db;border: solid 1px; padding: 10px;  border-radius: 5px;margin-top:20px;"),
                                   width = 10)
                       )),
              tabPanel("References",
                       includeMarkdown("inst/references.md"))
  )
)

build_config <- function(input, row) {
  if(input$screening_parts) {
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
                                  #use_scc = as.logical(input$use_scc),
                                  devices_exclude = as.logical(input$devices_exclude))
    } else {
      if (input$conf_auto == "est") {
        a_priori <- HALT::a_priori_est(baserate_hp = as.numeric(input$baserate_hp), 
                                       device = input$devices, 
                                       min_number = as.numeric(input$participants), 
                                       min_prob =  as.numeric(input$min_prob), 
                                       tolerance = 10000)
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
                                  baserate_hp = as.numeric(input$baserate_hp),
                                  devices = input$devices,
                                  #use_scc = as.logical(input$use_scc),
                                  loop_exclude = as.numeric(input$max_loops),
                                  lr_img_exclude = TRUE,
                                  lr_audio_exclude = TRUE,
                                  devices_exclude = as.logical(input$devices_exclude), 
                                  volume_level = input$volume_level,
                                  channel_check = TRUE,
                                  screening_parts = TRUE,
                                  frequency_check = as.logical(input$frequency_check))
    }
  } else {
    config <- HALT::auto_config(volume_level = input$volume_level,
                                loop_exclude = as.numeric(input$max_loops),
                                channel_check = as.logical(input$channel_check),
                                lr_img_exclude = as.logical(input$lr_disc),
                                lr_audio_exclude = as.logical(input$mono_inter),
                                frequency_check = as.logical(input$frequency_check),
                                screening_parts = as.logical(input$screening_parts))
  }
  attr(config, "class") <- "list"
  config <- config %>% as.data.frame()
  names(config) <- c("Volume level", "Max. Loops", "Channel check", "LR Exclude (img)", 
                     "LR Exclude (audio)", "Frequency check", "Screening parts",
                     "Method Code", "A", "B", "C", "Base Rate", "SCC", "Exclude by Devcie", "Device")
  config
}

selection_table <- function(input) {
  if (input$screening_parts) {
    if (input$conf_auto == "est") {
      selection <-
        HALT::a_priori_estimation(screening_strat = ifelse(as.logical(input$use_scc),
                                                           "scc",
                                                           "far"),
                                  baserate_hp = as.numeric(input$baserate_hp),
                                  switch_to_target = as.numeric(input$switch_to_target),
                                  devices = input$devices,
                                  min_number = as.numeric(input$participants),
                                  min_prob =  as.numeric(input$min_prob),
                                  tolerance = 10000) %>%
        dplyr::rename(Method = method,
                      `Eval. Key` = method_code,
                      Sensitivity = true_hp_rate,
                      Specificity = true_ls_rate,
                      PPV = ifelse(as.logical(input$use_scc),
                                   "scc_hp_pv",
                                   "hp_pv"),
                      NPV = ifelse(as.logical(input$use_scc),
                                   "scc_ls_pv",
                                   "ls_pv"),
                      Utility = ifelse(as.logical(input$use_scc),
                                       "scc_utility",
                                       "utility"),
                      `Sample Size` = samplesize,
                      `Expect. total participants` = expectation_total_participants,
                      `Min Quality %` = min_quality_percent)
      #if (as.logical(input$use_scc)) {
      #  selection <- selection %>% select(-scc_target, -prob_scc_target)
      #}
    } else {# if(input$conf_auto == "manual" || input$conf_auto == "auto")
      if (input$use_scc) {
        selection <-
          tests_scc_utility(baserate_hp = as.numeric(input$baserate_hp),
                            devices = input$devices,
                            switch_to_target = as.numeric(input$switch_to_target)) %>%
          select(-scc_target,-prob_scc_target)
      } else {
        selection <-
          tests_pv_utility(baserate_hp = as.numeric(input$baserate_hp))
      }
      selection <- selection %>%
        dplyr::select(-c(logic_expr, false_ls_rate, false_hp_rate)) %>% 
        dplyr::rename(Method = method,
                      `Eval. Key` = method_code,
                      Sensitivity = true_hp_rate,
                      Specificity = true_ls_rate,
                      PPV = ifelse(input$use_scc,
                                   "scc_hp_pv",
                                   "hp_pv"),
                      NPV = ifelse(input$use_scc,
                                   "scc_ls_pv",
                                   "ls_pv"),
                      Utility = ifelse(input$use_scc,
                                       "scc_utility",
                                       "utility")) %>% 
        dplyr::arrange(desc(Utility))
      if(input$conf_auto == "manual") {
        A_threshold <- as.numeric(input$A_threshold)
        B_threshold <- as.numeric(input$B_threshold)
        C_threshold <- as.numeric(input$C_threshold)
        if(input$combination_method %in% c(2,3,6,7)){A_threshold <- 0}
        if(input$combination_method %in% c(1,3,8,9)){B_threshold <- 0}
        if(input$combination_method %in% c(1,2,4,5)){C_threshold <- 0}
        selection <- selection %>%
          dplyr::filter(`Eval. Key` == as.numeric(input$combination_method),
                        A == A_threshold,
                        B == B_threshold,
                        C == C_threshold)
      }
    }
  } else {
    selection <- data.frame(X = "Placeholders are used in the config.") %>% 
      dplyr::rename(`You deactivated the screening tests.` = X)
  }
  selection %>% mutate_if(is.numeric, round, 4)
}

explanation_text <- function(input, row) {
  devices <- ifelse(input$devices == "HP", "headphones", "loudspeakers")
  if(input$conf_auto == "est") {
    a_priori <- HALT::a_priori_est(baserate_hp = ifelse(input$use_scc,
                                                        ifelse(input$devices == "HP",
                                                               as.numeric(input$switch_to_target),
                                                               1 - as.numeric(input$switch_to_target)),
                                                        as.numeric(input$baserate_hp)),
                                   device = input$devices,
                                   min_number = as.numeric(input$participants),
                                   min_prob =  as.numeric(input$min_prob),
                                   tolerance = 10000)
    if (length(row) == 0) {
      row <- 1
    }
    a_priori <- a_priori[row,]
    expl <- 
      #ifelse(!input$use_scc,
      #       sprintf(
      #         "When the prevelance for headphones in your target sample is assumed to be %.4f and the screening method '%s' (code %i) with thresholds of %i, %i, and %i correct responses for tests A, B, and C is used a sample of %i participants classified as %s users is required to have a probability of at least %.2f that %i participants actually used %s. The percentage of correct identified target playback devices ('quality') of such a sample would then be at least %.1f percent.",
      #         as.numeric(input$baserate_hp), a_priori$method[1], a_priori$method_code[1],
      #         as.integer(a_priori$A[1]), as.integer(a_priori$B[1]),
      #         as.integer(a_priori$C[1]), as.integer(a_priori$samplesize[1]),
      #         devices, as.numeric(input$min_prob), as.integer(input$participants),
      #         devices, a_priori$min_quality_percent[1]),
      #       sprintf(
      #         "When the switching prevalence in your target sample is assumed to be %.4f and the screening method '%s' (code %i) with thresholds of %i, %i, and %i correct responses for tests A, B, and C is used a sample of %i participants who reported use of %s or whose tests indicate the use of %s is required to have a probability of at least %.2f that %i participants actually used %s. The percentage of correct identified target playback devices ('quality') of such a sample would then be at least ",
      #         as.numeric(input$switch_to_target), a_priori$method[1], a_priori$method_code[1],
      #         as.integer(a_priori$A[1]), as.integer(a_priori$B[1]),
      #         as.integer(a_priori$C[1]), a_priori$samplesize,
      #         devices, devices, as.numeric(input$min_prob),
      #         as.integer(input$participants), devices, a_priori
      #       )
      #)
      a_priori_explanation(screening_strat = ifelse(input$use_scc,
                                                    "scc",
                                                    "fwr/far"),
                           devices = input$devices,
                           baserate_hp = as.numeric(input$baserate_hp),
                           switch_to_target = as.numeric(input$switch_to_target),
                           min_number = as.numeric(input$participants),
                           min_prob = as.numeric(input$min_prob),
                           test_method = a_priori)
  }
  if(input$conf_auto == "auto") {
    expl <- paste0("The overall utility describes a utilities-weighted sum of the probabilities ",
                   "of the four decision-making outcomes (Treat & Viken, 2012, p. 725). ",
                   "Utilities for the four decision-making are set to maximise the percentage of correct classifications ",
                   "for the given prevalence. The 'best' test (combination) is the one with the highest overall utility ",
                   "among several tests for the application.<br>",
                   "PPV expresses the probability of headphone usage when the screening test is positive. ",
                   "NPV expresses the probability of loudspeaker usage when the screening test is negative.<br>")
    if (input$use_scc) {
      a_priori <-
        tests_scc_utility(baserate_hp = as.numeric(input$baserate_hp),
                          devices = input$devices,
                          switch_to_target = as.numeric(input$switch_to_target)) %>%
        filter(scc_utility == max(scc_utility))
    } else {
      a_priori <- tests_pv_utility(baserate_hp = as.numeric(input$baserate_hp)) %>%
      filter(utility == max(utility))
    }
    
    multiple_best_tests <- dim(a_priori)[[1]] > 1
    expl <- paste0(
      expl,
      paste0(sprintf("When the prevalence for headphones is %.4f ", as.numeric(input$baserate_hp)),
             ifelse(input$use_scc,
                    sprintf("and the switching prevalence is %.4f ",
                            as.numeric(input$switch_to_target)),
                    ""),
             ifelse(multiple_best_tests,
                    "there are multiple 'best' test combinations ",
                    sprintf("the test (combination) '%s' with thresholds %i, %i, and %i for Test A, Test B, and Test C, respectively, is the 'best' test (combination) ",
                            evaluation_keys()[input$combination_method], a_priori$A[[1]],
                            a_priori$B[[1]], a_priori$C[[1]])),
             sprintf("within %s.",
                     ifelse(input$use_scc,
                            "SCC",
                            "FWR/FAR")))
    )
  }
  if(input$conf_auto == "manual") {
    expl <- paste0("You specified a test combination and the threshold of each individual test. ",
                   "If you want to compare various combinations of tests and their thresholds select ",
                   "'prevalence and overall utility'", " or ", "'sample size estimations'", ".")
  }
  if(!input$screening_parts) {
    expl <- "Participants will not be screened for their playback devices."
  }
  expl
}

post_hoc_text <- function(input) {
  config <- make_config(combination_method = input$post_hoc_test_combi,
                        A_threshold = input$post_hoc_A,
                        B_threshold = input$post_hoc_B,
                        C_threshold = input$post_hoc_C,
                        baserate_hp = ifelse(input$screening_strat == "scc",
                                             input$post_hoc_switch_rate,
                                             input$post_hoc_baserate) %>% as.numeric(),
                        devices = input$post_hoc_target,
                        use_scc = (input$screening_strat == "scc"))
  if (input$post_hoc_mode == "min_number") {
    min_number <- input$post_hoc_min_number
    min_prob <-
      post_hoc_calc_min_prob(screening_strat = input$screening_strat,
                             config = config,
                             min_number = min_number,
                             sample_size = input$post_hoc_samplesize %>% as.numeric(),
                             target_selfreported = input$post_hoc_target_selfreport %>% as.numeric(),
                             target_tested = input$post_hoc_target_tested %>% as.numeric(),
                             switch_to_target = input$post_hoc_switch_rate %>% as.numeric())
    
  } else {
    min_prob <- input$post_hoc_min_prob
    min_number <-
      post_hoc_calc_min_number(screening_strat = input$screening_strat,
                               config = config,
                               min_prob = min_prob,
                               sample_size = input$post_hoc_samplesize %>% as.numeric(),
                               target_selfreported = input$post_hoc_target_selfreport %>% as.numeric(),
                               target_tested = input$post_hoc_target_tested %>% as.numeric(),
                               switch_to_target = input$post_hoc_switch_rate %>% as.numeric())
  }
  min_data_qual_perc <- ifelse(input$screening_strat == "scc",
                               min_number / (as.numeric(input$post_hoc_target_selfreport) +
                                               as.numeric(input$post_hoc_target_tested)),
                               min_number / as.numeric(input$post_hoc_samplesize)) * 100
  post_hoc_explanation(screening_strat = input$screening_strat,
                       combination_method = input$post_hoc_test_combi %>% as.numeric(),
                       A = input$post_hoc_A %>% as.numeric(),
                       B = input$post_hoc_B %>% as.numeric(),
                       C = input$post_hoc_C %>% as.numeric(),
                       devices = input$post_hoc_target,
                       baserate_hp = input$post_hoc_baserate %>% as.numeric(),
                       min_number = min_number,
                       min_prob = min_prob,
                       sample_size = input$post_hoc_samplesize %>% as.numeric(),
                       target_selfreported = input$post_hoc_target_selfreport %>% as.numeric(),
                       target_tested = input$post_hoc_target_tested %>% as.numeric(),
                       switch_to_target = input$post_hoc_switch_rate %>% as.numeric(),
                       min_data_qual_perc = min_data_qual_perc)
}

post_hoc_table <- function(input) {
  config <- make_config(combination_method = input$post_hoc_test_combi,
                        A_threshold = input$post_hoc_A,
                        B_threshold = input$post_hoc_B,
                        C_threshold = input$post_hoc_C,
                        baserate_hp = ifelse(input$screening_strat == "scc",
                                             input$post_hoc_switch_rate,
                                             input$post_hoc_baserate) %>% as.numeric(),
                        devices = input$post_hoc_target,
                        use_scc = (input$screening_strat == "scc"))
  
  if (input$post_hoc_mode == "min_number") {
    min_number <- input$post_hoc_min_number
    min_prob <-
      post_hoc_calc_min_prob(screening_strat = input$screening_strat,
                             config = config,
                             min_number = min_number,
                             sample_size = as.numeric(input$post_hoc_samplesize),
                             target_selfreported = as.numeric(input$post_hoc_target_selfreport),
                             target_tested = as.numeric(input$post_hoc_target_tested),
                             switch_to_target = as.numeric(input$post_hoc_switch_rate))
    
  } else {
    min_prob <- input$post_hoc_min_prob
    min_number <-
      post_hoc_calc_min_number(screening_strat = input$screening_strat,
                               config = config,
                               min_prob = min_prob,
                               sample_size = as.numeric(input$post_hoc_samplesize),
                               target_selfreported = as.numeric(input$post_hoc_target_selfreport),
                               target_tested = as.numeric(input$post_hoc_target_tested),
                               switch_to_target = as.numeric(input$post_hoc_switch_rate))
  }
  pht <-
    post_hoc_tibble(screening_strat = input$screening_strat,
                    combination_method = config$combination_method,
                    A = config$A_threshold,
                    B = config$B_threshold,
                    C = config$C_threshold,
                    target_device = input$post_hoc_target,
                    sample_size = as.integer(input$post_hoc_samplesize),
                    target_selfreported = as.integer(input$post_hoc_target_selfreport),
                    target_tested = as.integer(input$post_hoc_target_tested),
                    baserate_hp = as.numeric(input$post_hoc_baserate),
                    switch_to_target = as.numeric(input$post_hoc_switch_rate),
                    min_number = as.integer(min_number),
                    min_prob = min_prob) %>% 
    rename(`Screening Strategy` = screening_strat,
           `Combination Method` = combination_method,
           `Target Device` = target_device,
           `Final Sample Size` = sample_size,
           `Minimum Number` = min_number,
           `Minium Probability` = min_prob,
           `Min. Quality %` = min_data_qual_perc)
  if (input$screening_strat == "scc") {
    pht %>% rename(`Switching Prevalence` = switch_to_target,
                   `Reported Target Devices` = target_selfreported,
                   `Tested Target Devices` = target_tested)
  } else {
    pht %>% rename(`Prevalence for Headphones` = baserate_hp)
  }
}

server <- function(input, output, session) {
  message("*** STARTING APP***")
  output$introduction <- renderUI({
    get_intro_text()
  })
  
  output$selection_output <- DT::renderDataTable({
    DT::datatable(selection_table(input), options = list(lengthMenu = c(5, 10, 50), pageLength = 10),
                  selection = "single")
  }, width = "100%"
  )
  
  output$config_output <- renderTable({
    build_config(input, input$selection_output_rows_selected) %>% mutate(`Method Code` = as.integer(`Method Code`),
                                                                         A = as.integer(A),
                                                                         B = as.integer(B),
                                                                         C = as.integer(C),
                                                                         `Max. Loops` = as.integer(`Max. Loops`))
  }, width = "100%"
  )
  
  output$download_config <- downloadHandler(
    filename = "HALT_config.csv",
    content = function(file){
      config <- build_config(input, input$selection_output_rows_selected)
      names(config) <- names(HALT::auto_config())
      write.table(as.data.frame(config), file, sep = ";", row.names = FALSE, quote = FALSE)
    }
  )
  
  output$explanation <- renderText({
    explanation_text(input, input$selection_output_rows_selected)
    })
  
  output$parameter_description <- renderTable({
    parameter_description %>% select(-parameter) %>% rename("Parameter" = label, "Description" = description)
  })
  
  output$post_hoc_explanation <- renderText({
    post_hoc_text(input = input)
  })
  
  output$post_hoc_table <- renderTable({
    post_hoc_table(input = input)
  })
  
  observe({
    max_min_number <-
      ifelse(input$screening_strat == "scc",
             as.numeric(input$post_hoc_target_selfreport) +
               as.numeric(input$post_hoc_target_tested),
             as.numeric(input$post_hoc_samplesize))
    # Control the value, min, max
    updateSliderInput(session, "post_hoc_min_number", value = max_min_number %/% 2,
                      min = 1, max = max_min_number)
  })
}
  
# Run the application 
shinyApp(ui = ui, server = server)
