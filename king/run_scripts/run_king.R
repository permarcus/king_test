# Loading packages of interest

library(bigrquery)
library(checkmate)
library(yaml)
library(dplyr)
library(caret)
library(ggplot2)
library(knitr)
library(rmarkdown)
library(tinytex)


# Settig seed for consistency
set.seed(4711)

# Connecting project to bash for app - functionality
# args <- commandArgs()

args <- c("config", "configs/test_king_kongfig.yaml")

try_king <- try({
  # Read in config ----
    cfg <- get_king_kongfig(cfg_path = args[[2]])
    if(cfg$verbose) cat(args, "\n")
    if(cfg$verbose) cat("Read config object\n")
  
  # Check what datasets exists within project ----
    if(cfg$verbose) cat(args, "\n")
    if(cfg$verbose) cat("Check data sets\n")
    bigrquery::bq_project_datasets(x = cfg$io$google$project)
  
  # Assert dataset exists ----
    if(cfg$verbose) cat(args, "\n")
    if(cfg$verbose) cat("Assert data set\n")
    bigrquery::bq_dataset_exists(bq_project_datasets(x = cfg$io$google$project)[[1]])
  
  # Assert that all tables exist ----
    if(cfg$verbose) cat(args, "\n")
    if(cfg$verbose) cat("Check and assert tables\n")
    check_king_tables(cfg)
  
  # Check conversion proportion ----
    if(cfg$verbose) cat(args, "\n")
    if(cfg$verbose) cat("Check conversion proportion in total n' a and b group\n")
    conv_proportion <- king_konv_prop(cfg)
  
  # Collect assignment data from BIG google ----
    if(cfg$verbose) cat(args, "\n")
    if(cfg$verbose) cat("Collect assignment data\n")
    assignment_df <- collect_ass_df(cfg, rows = 1000)
  
  # Collect activity data from BIG google ----
    if(cfg$verbose) cat(args, "\n")
    if(cfg$verbose) cat("Collect activity data\n")
    activity_df <- collect_act_df(cfg, assignment_df)
  
  # Joining data sets for modelleing ----
    if(cfg$verbose) cat(args, "\n")
    if(cfg$verbose) cat("Setting modelling data set and train/validation data\n")
    king_df_list <- modelling_df(activity_df, assignment_df, train_prop = cfg$model$train_prop)
  
  # Modelling, or magic? ----
    king_model <- train_model(king_df_list$train, cfg)
    
  # Predict on new data. (test data) ----
    predictions <- predict(king_model, king_df_list$test)
  
  # Model validation ----
    king_validate <- model_eval(king_df_list, king_model, predictions)
      
  # Plot Results ----
    plot_king <- plots_king(king_df_list)
  
  # Statistics Results ----
    statistics_king <- statistics_king(king_df_list)
    
  
  # Create report ----
    rmarkdown::render(input = "markdown/test_rmark.Rmd", output_dir = "markdown")
  
}, 
silent = FALSE)


