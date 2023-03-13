# Sensitivity analysis workflow
# Author: Matthieu VERON
# Contact: mveron@uw.edu

# This script houses the material needed to set up the sensitivity analysis
# workflow. It has to be run only once at the beginning.


rm(list = ls(all.names = TRUE))

# 1. Set up ----

# load packages ----

updateKableExtra <- FALSE # Needed the first time
if(updateKableExtra)
  devtools::install_github(repo="haozhu233/kableExtra", ref="a6af5c0")
library(kableExtra)

# Local declarations ----
fsep <- .Platform$file.sep

# Set directories ----
dir_model <- file.path(here::here(), "model", fsep = fsep)
dir_script <- file.path(here::here(), "R", fsep = fsep)


# Set up the Sensitivity Analysis root folders ----
dir_SensAnal <- file.path(dir_model, "Sensitivity_Anal", fsep = fsep)
dirScript_SensAnal  <- file.path(dir_script, "Sensitivity_Anal", fsep = fsep)

if(!dir.exists(dir_SensAnal))
  dir.create(dir_SensAnal)
if(!dir.exists(dirScript_SensAnal))
  dir.create(dirScript_SensAnal)

# load functions ----
source(file.path(dir_script, "utils", "sensistivity_analysis_utils.R", fsep = fsep))
# ----------------------------------------------------------


# 2. Create the materials needed for the sensitivity analyses ----
# ---------------------------------------------------------- #
# Summary_Sensitivity_analysis.pdf - Summary of all SA
# Models_Sensitivity_analysis.pdf - Models, names, individual directory
# SA_info.RData - data to build the documents

if(!file.exists(file.path(dir_SensAnal, "Summary_Sensitivity_analysis.pdf", fsep = fsep))){
  
  # Create the Data that summarizes the SA already done ----
  Topic <- data.frame(
    Topic = c("transition","landings", "discards", "surveys", "biological_Info", "model"),
    Nam = c("0. Transition","1. Landings", "2. Discards", "3. Surveys", "4. Biological Info", "5. Model"),
    ID = 0:5) 
  
  SumUp <- data.frame(matrix("", nrow = length(Topic$Topic), ncol = 11))
  colnames(SumUp) <- c("SA number","Topic",'Author',"Date",
                         "Folder","Script model","Script results",
                         "Base model","New model","Object","Features")
  
  SumUp$`SA number` <- c("Item 0.0","Item 1.1", "Item 2.1","Item 3.1","Item 4.1","Item 5.1")
  SumUp$Topic <- Topic$Topic
  SumUp$Object <- c("Bridging","Adding 2023 catch","Discard as fleet", "Adding 2023 surveys","Growth","Add fleet")
  
  # Create the .pdf file that summarizes the existing models
  Models_SA <-  data.frame(
    ID_SA = c("", "Item 0.0"),
    Topic = c("","transition"),
    Object = c("SS 3.24","SS 3.30.21"),
    nam_model = c("13.sq", "23.sq.fixQ"),
    descr = c("2013 Assessment SS3.24", 
              "2023 Assessment SS3.30-fixed catchability"
    ),
    paths = c(file.path("model","2013_SST"),
              file.path("model","2013_SST_SSV3_30_21"))
  )
  colnames(Models_SA) <- c("SA number","Topic","Object",'Model name',"Description",
                           "path")

  # Create the Summary_Sensitivity_analysis.pdf file that summarizes all SA
  suppressMessages(SumUp <- update_SA_table(SumUp = SumUp, dir_SensAnal = dir_SensAnal))
  cat("\n The Summary_Sensitivity_analysis.pdf file has been created.\n")
  SumUp <- SumUp[1,]
  SumUp <- SumUp  %>% 
    dplyr::mutate(Author = "The team",
                  Date = "2023-02-17 09:09:21",
                  Folder = file.path("model", "2013_SST_SSV3_30_21", fsep = fsep),
                  'Script model' = "v324_v330_transition.R", 
                  'Script results' = "2013_v324_v330_bridge_comparison.R",
                  'Base model' = "3.sq"
                  )
  # Create the Models_Sensitivity_analysis.pdf file that summarizes all SA models
  
  suppressMessages(Models_SA <- update_Models_SA_table(Models_SA = Models_SA,
                                                       dir_SensAnal = dir_SensAnal))
  cat("\n The Models_Sensitivity_analysis.pdf file has been created.\n")

  # Save the data
  SA_info <- list(SumUp = SumUp, Models_SA = Models_SA)
  if(!file.exists(file.path(dirScript_SensAnal, "SA_info.RData", fsep = fsep)))
    save(SA_info, file = file.path(dirScript_SensAnal, "SA_info.RData", fsep = fsep))
}
# ----------------------------------------------------------