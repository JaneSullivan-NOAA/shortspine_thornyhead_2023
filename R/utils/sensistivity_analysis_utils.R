# load packages ----
library(kableExtra)


# Basic functions ----
an <- function(x){return(as.numeric(x))}
ac <- function(x){return(as.character(x))}
af <- function(x){return(as.factor(x))}



#' 1. Function to update the SA table ----
#' ----------------------------------------------------------- #
#' 
#' @param SumUp (data frame)- Contains the info about the SA already done.
#' @param dir_SensAnal (character string)- path to the Sensitivity Analysis 
#' folder.
#
update_SA_table <- function(SumUp, dir_SensAnal) {
  
  Topic <- data.frame(
    Topic = c(
      "transition",
      "landings",
      "discards",
      "surveys",
      "biological_Info",
      "model"
    ),
    Nam = c(
      "0. Transition",
      "1. Landings",
      "2. Discards",
      "3. Surveys",
      "4. Biological Info",
      "5. Model"
    ),
    ID = 0:5
  )
  
  SumUp <- merge(SumUp, Topic, by = "Topic")
  SumUp <- SumUp[order(SumUp$ID), ]
  rownames(SumUp) <- NULL
  
  row_top <- NULL
  for (i in 1:dim(SumUp)[1]) {
    t <- unique(SumUp$Topic)[i]
    tmpL <- length(which(SumUp$Topic == t))
    row_top <- rbind(row_top,
                     c(which(SumUp$Topic == t)[1],
                       which(SumUp$Topic == t)[tmpL]))
    rownames(row_top)[i] <- t
  }
  
  
  row_group_label_fonts <- list(
    list(bold = T, italic = T),
    list(bold = F, italic = F)
  )
  
  tmpTable <- SumUp[, !colnames(SumUp) %in% c("Topic", "Nam", "ID")]
  colnames(tmpTable)[1] <- ""
  names(tmpTable)[4] <- paste0(colnames(tmpTable)[4], footnote_marker_alphabet(1))
  names(tmpTable)[5] <- paste0(colnames(tmpTable)[5], footnote_marker_alphabet(2))
  names(tmpTable)[6] <- paste0(colnames(tmpTable)[6], footnote_marker_alphabet(2))
  names(tmpTable)[7] <- paste0(colnames(tmpTable)[7], footnote_marker_alphabet(3))
  names(tmpTable)[8] <- paste0(colnames(tmpTable)[8], footnote_marker_alphabet(3))
  
  table <-
    kbl(tmpTable,
        caption = "Sensitivity analyses summary.", booktabs = T,
        align = "r",
        digits = 3,
        escape = F) %>%
    kable_styling(
      full_width = FALSE,
      font_size = 20,
      position = "left",
      latex_options = c("repeat_header",
                        "striped",
                        "hold_position")) %>% 
    landscape() %>%
    kable_paper(full_width = F) %>%
    collapse_rows(columns = c(1:6,10), 
                  row_group_label_position = 'stack', 
                  valign = "middle",
                  row_group_label_fonts = row_group_label_fonts)
  
  for (i in 1:length(Topic$Topic)) {
    Nam <- Topic$Nam[i]
    item <- Topic$Topic[i]
    if(item %in% SumUp$Topic)
      eval(parse(
        text = paste0(
          "table <- table %>% pack_rows('",
          Nam,
          "', row_top['",
          item,
          "',1], row_top['",
          item,
          "', 2])"
        )
      ))
  }
  table <- table %>% 
    footnote(general = "\nHere is a general comments where to find scripts, folders, and models. \n",
             alphabet = c("The **Folder** is housed in the 'model/Sensitivity_Anal' repertory; \n",
                          "The scripts are housed in the 'R/Sensitivity_Anal' repertory; \n",
                          "The models are housed in the 'model/Sensitivity_Anal/**Folder**' repertory"),
             footnote_as_chunk = T)
  
  
  SumUp <- SumUp[, !colnames(SumUp) %in% c("Nam", "ID")]
  rownames(SumUp) <- NULL
  kableExtra::save_kable(table, 
                         file = file.path(dir_SensAnal, "Summary_Sensitivity_analysis.pdf", fsep = fsep))
  return(SumUp)
}

#' 2. Function to update the summary of models table ----
#' ----------------------------------------------------------- #
#' 
#' @param Models_SA (data frame)- Contains the info about the SA already done.
#' @param dir_SensAnal (character string)- path to the Sensitivity Analysis 
#' folder.
#
update_Models_SA_table <- function(Models_SA, dir_SensAnal) {
  
  Topic <- data.frame(
    Topic = c(
      "transition",
      "landings",
      "discards",
      "surveys",
      "biological_Info",
      "model"
    ),
    Nam = c(
      "0. Transition",
      "1. Landings",
      "2. Discards",
      "3. Surveys",
      "4. Biological Info",
      "5. Model"
    ),
    ID = 0:5
  )
  
  baseModel <- Models_SA[Models_SA$Topic=="",]
  baseModel <- baseModel %>%
    dplyr::mutate(Topic ="",
                  Nam = "",
                  ID = "")
  
  Models_SA <- merge(Models_SA, Topic, by = "Topic")
  Models_SA <- rbind(baseModel, Models_SA)
  rownames(Models_SA) <- NULL
  Models_SA <- Models_SA[order(Models_SA$ID), ]
  
  row_top <- NULL
  for (i in 1:dim(Models_SA)[1]) {
    t <- unique(Models_SA$Topic)[i]
    if (!is.na(t)) {
      tmpL <- length(which(Models_SA$Topic == t))
      row_top <- rbind(row_top,
                       c(which(Models_SA$Topic == t)[1],
                         which(Models_SA$Topic == t)[tmpL]))
      rownames(row_top)[i] <- t
    }
  }
  
  tmpTable <- Models_SA[, !colnames(Models_SA) %in% c("Topic", "Nam", "ID")]
  colnames(tmpTable)[1] <- ""
  table <-
    kbl(tmpTable,
        caption = "Sensitivity analyses models.", booktabs = T,
        align = "r",
        digits = 3) %>%
    kable_styling(
      full_width = FALSE,
      font_size = 20,
      position = "left",
      latex_options = c("repeat_header",
                        "striped",
                        "hold_position")
    ) %>% landscape() %>%
    kable_paper(full_width = F) %>%
    collapse_rows(columns = c(1), 
                  row_group_label_position = 'stack', 
                  valign = "middle")
  
  for (i in 1:length(Topic$Topic)) {
    Nam <- Topic$Nam[i]
    item <- Topic$Topic[i]
    if(item %in% Models_SA$Topic)
      eval(parse(
        text = paste0(
          "table <- table %>% pack_rows('",
          Nam,
          "', row_top['",
          item,
          "',1], row_top['",
          item,
          "', 2])"
        )
      ))
  }
  
  Models_SA <- Models_SA[, !colnames(Models_SA) %in% c("Nam", "ID")]
  rownames(Models_SA) <- NULL
  kableExtra::save_kable(table, 
                         file = file.path(dir_SensAnal, "Models_Sensitivity_analysis.pdf", fsep = fsep))
  return(Models_SA)
}

#' 3. Function to start a new sensitivity analysis ----
#' ----------------------------------------------------------- #
#' 
#' @param topic (character string)- Indicates the main topic of the sensitivity 
#' analysis. This HAS to be either"transition",  "landings", "discards", "surveys",
#' "biological_Info", or "model".
#' @param object (character string)- Indicates a specific item of 
#' the main topic/model. For example, "Length_Comp", "Age_comp", "Growth", "Fecundity",...
#' If multiple models are built, the length of \code{object} equals the numder of
#' models with a specific input for each model.
#' @param author (character string)- Who leads the sensitivity analysis and runs
#' the model.
#' @param folder_name (character string)- name of the folder to create for that 
#' particular sensitivity analysis. This folder will be housed in the root folder 
#' 'Sensitivity_Anal' and will hold the scripts  used to set up the model and both 
#' models input and output files for this analysis.
#' @param script_model (character string)- Name of the script used to create the 
#' model (without extension).
#' @param script_results (character string)- Name of the script used to analyze 
#' the results from that sensitivity analysis  (without extension).
#' @param base_model (character string)- which model has been used as the basis 
#' of this analysis (before any modification). By default, the function considers
#' the 2013 Assessment model transitioned to the 3.30 version of SS with all
#' parameters estimated i.e., \code{"23.sq.est"}(See "Models_Sensitivity_analysis.pdf")
#' @param new_model (character string)- name of the model you're gonna develop for
#' this analysis.
#' 
#' @details
#' The function creates:
#' 1. one folder named as \code{folder_name} in the 
#'    "shortspine_thornyhead_2023/model/Sensitivity_Anal" repertory. This folder will
#'    house model input and output files,
#' 2. the "Sensitivity_Analysis_Features.txt" file (in the previously created folder).
#'    This file summaryzes the features of the sensitivity analysis 
#'    (\code{topic}, \code{object}, \code{author}, ...)
#' 3. Update the "Summary_Sensitivity_analysis.pdf" file housed in the root folder.
#' 4. Update the "Models_Sensitivity_analysis.pdf" file housed in the root folder.
#' 5. Generate SA-specific templates for the scripts to build and analyze the models 
#

NewSensAnal <- function(topic = NULL,
                        object = NULL,
                        author = NULL,
                        folder_name = NULL,
                        script_model = NULL,
                        script_results = NULL,
                        base_model = "23.sq.est",
                        new_model = NULL
){
  
  # local declarations
  fsep <- .Platform$file.sep
  
  dir_model <- file.path(here::here(), "model", fsep = fsep)
  dir_script <- file.path(here::here(), "R", fsep = fsep)
  dir_SensAnal <- file.path(dir_model, "Sensitivity_Anal", fsep = fsep)
  dirScript_SensAnal  <- file.path(dir_script, "Sensitivity_Anal", fsep = fsep)
  
  # load the SA_info dataframe
  load(file.path(dirScript_SensAnal, "SA_info.RData", fsep = fsep))
  SumUp <- SA_info$SumUp
  Models_SA <- SA_info$Models_SA
  
  
  # 1. Set an ID for this sensitivity analysis ----
  # ======================================================== #
  Topic <- data.frame(
    Topic = c(
      "transition",
      "landings",
      "discards",
      "surveys",
      "biological_Info",
      "model"
    ),
    Nam = c(
      "0. Transition",
      "1. Landings",
      "2. Discards",
      "3. Surveys",
      "4. Biological Info",
      "5. Model"
    ),
    ID = 0:5
  )
  
  tmpSumUp <- SumUp[SumUp$Topic==topic,]
  ID_topic <- Topic[Topic$Topic==topic,"ID"]
  
  if(dim(tmpSumUp)[1]==0){
    SA_ID <- paste0("Item ",ID_topic, ".1")
  } else {
    IDs <- max(an(stringr::str_split_fixed(string = tmpSumUp$`SA number`,
                                            pattern=paste0(ID_topic,"."), n = 2)[,2]))
    IDs <- IDs +1
    SA_ID <- paste0("Item ",ID_topic, ".",IDs)
  }
  
  # Update both the folder_name and scripts names with the SA ID
  folder_name <- paste0(ID_topic, ".",IDs,"_",folder_name)
  script_model <- paste0(ID_topic, ".",IDs,"_",script_model,".R")
  script_results <- paste0(ID_topic, ".",IDs,"_",script_results,".R")
  # ========================================================

  # 2. Check the input arguments ----
  # ======================================================== #
  if(!topic %in%c("transition", "landings", "discards", "surveys","biological_Info","model")){
    cat('\n')
    cat('=> The "topic" argument has to be defined as either  "landings", "discards", 
 "surveys","biological_Info", or "model".\n')
    stop()
  }
  if(is.null(object)){
    cat('\n')
    cat('=> The "object" argument has to be defined. Please indicate the item of
 your sensitivity analysis (e.g., "Length_Comp", "Age_comp", "Growth", "Fecundity",...)\n')
    stop()
  } else if(length(new_model)>1 && length(unique(object))==1){
    cat('\n')
    cat('=> The "object" argument is dentic for several models. Please indicate a
 specific item for each model.\n')
    stop()
  }
  if(is.null(author)){
    cat('\n')
    cat('=> The "author" argument has to be defined. Please provide an author name
 (leader of the sensitivity analysis).\n')
    stop()
  }
  if(is.null(folder_name)){
    cat('\n')
    cat('=> The "folder_name" argument has to be defined. Please provide a name
 for the folder where model input and output files will be save for this analyisis.\n')
    stop()
  } else if(folder_name %in% SumUp$Folder && 
            length(unique(new_model %in% Models_SA$`Model name`))==1 && 
            unique(new_model %in% Models_SA$`Model name`)){
    cat('\n')
    # cat('=> This "folder_name": ',folder_name,' already exist for another sensitivity analysis:\n',
    #     '\t => Please enter a different folder name for your analysis.\n')
    cat('=> This(ese) new model name(s_: ',new_model,' already exist in the folder ',folder_name,':\n',
        '\t => Please enter different model name(s) for your analysis.\n')
    stop()
  }
  if(is.null(script_model)){
    cat('\n')
    cat('=> The "script_model" argument has to be defined. Please provide the name 
 of the script used to implement the SS model used for this sensitivity analysis.\n')
    stop()
  } else if(script_model %in% SumUp$`Script model`){
    cat('\n')
    cat('=> This "script_model": ',script_model,' already exist for another sensitivity analysis:\n',
        "\t => Please enter a different name for the script used to create the model for this analysis.\n")
    stop()
  }
  
  if(is.null(script_results)){
    cat('\n')
    cat('=> The "script_results" argument has to be defined. Please provide the name 
 of the script used to analyze the results of this sensitivity analysis.\n')
    stop()
  } else if(script_results %in% SumUp$`Script results`){
    cat('\n')
    cat('=> This "script_results": ', script_results ,' already exist for another sensitivity analysis:\n',
        "\t => Please enter a different name for the script used to analyze the
 outputs of the model for this analysis.\n")
    stop()
  }
  
  if(is.null(base_model)){
    cat('\n')
    cat('=> The "base_model" argument has to be defined. Please provide the name 
 of the model used as a basis for this sensitivity analysis.\n')
    stop()
  }

  if(is.null(new_model)){
    cat('\n')
    cat('=> The "new_model" argument has to be defined. Please provide the name 
 of the model you are building for this sensitivity analysis.\n')
    stop()
  } else {
    if(length(new_model)==1){
      if(new_model %in% Models_SA$`Model name`){
        cat('\n')
        cat('=> This "new_model": ',new_model,' already exists for another sensitivity analysis:\n',
            "\t => Please enter a different name for the model you are building for this analysis.\n")
        stop()
      }
    } else { #length(new_model)>1
      if(length(unique(new_model %in% Models_SA$`Model name`))>1){
        wrong <- which(new_model %in% Models_SA$`Model name`)
        cat('\n')
        cat('This "new_model": ',new_model[wrong],' already exists for another sensitivity analysis:\n',
            "\t => Please enter a different name for this new model.\n")
        stop()
      } else if(unique(new_model %in% Models_SA$`Model name`)){
        cat('\n')
        cat('These "new_model": ',new_model,' already exist for another sensitivity analysis:\n',
            "\t => Please enter a different name for the model you are building for this analysis.\n")
        stop()
      } else if(length(base_model)==1){
        cat("\nYou entered only one model as base model for this analysis. The new",
            " created models will therefore be based on the model:",base_model,"\n")
      }
      
    }
  }
  # ========================================================
  
  # 2.1 Detect if a base model does not exist yet ----
  # Set the base_model for each model
  if (length(new_model) > 1)
    if(length(base_model)==1)
      base_model <- rep(base_model, length(new_model))
  # Store the models comparison details
  Modcomp <- data.frame(base_model = base_model,
                        new_model = new_model,
                        warning = 0)
  ModcompWARNING <- FALSE
  for(m in 1:dim(Modcomp)[1]){
    if(!Modcomp$base_model[m] %in% Models_SA$`Model name`) {
      ModcompWARNING <- TRUE
      Modcomp$warning[m] <- 1
      if(m ==1) {
        cat("*************************************\n")
        cat("---           WARNINGS            ---\n")
        cat("*************************************\n")
      }
      cat(
        "\n\t=> The base model specified for the new model:",
        Modcomp$new_model[m],
        " does not exist yet. This means you'll have to start by creating this base model (",
        Modcomp$base_model[m],
        " before creating your new model !!!!"
      )
      if(m==dim(Modcomp)[1]){
        cat("*************************************\n")
        cat("*************************************\n")
      }
    }
  }
  # ========================================================
  
  # 3. Create the root folders for the analysis----
  # ======================================================== #
  # Create the root repertory
  WD <- file.path(dir_SensAnal, folder_name, fsep = fsep)
  if(dir.exists(WD)){ # double check
    cat("The specified folder name (",folder_name,") already exist for a sensitivity
 analysis. Please set another name.\n")
    stop()
  } else {
    dir.create(WD)
  }
  
  # Create the root folder for each model and copy and paste 
  # the input files from the base model
  pathMod <- NULL
  
  for(m in 1:length(new_model)){
    cat("\n")
    tmpWD <- file.path(WD, new_model[m],fsep = fsep)
    # Create directory
    dir.create(tmpWD)
    # Save the directory
    pathMod <- c(pathMod, file.path("model", "Sensitivity_Anal", folder_name, new_model[m]))
    
    # Copy SS input files from the base model
    modelFrom <- Models_SA[Models_SA$`Model name`==base_model[m], "path"]
    modelFrom <- file.path(here::here(), modelFrom, fsep = fsep)
    SS.files <- dir(modelFrom, "*.ss", ignore.case = TRUE, all.files = TRUE)
    data_echo <- file.path("run", "data_echo.ss_new", fsep = fsep)
    file.copy(
      from = file.path(modelFrom, SS.files, fsep = fsep),
      to = file.path(tmpWD, SS.files, fsep = fsep),
      overwrite = TRUE, copy.date = TRUE
    )
    
    if(m==1)
      cat("- Copying SS input files from :\n")
    cat("\t=>",modelFrom, "\n for the model :",new_model[m],"\n")
    
    if(file.exists(file.path(modelFrom, data_echo, fsep = fsep))){
      if(!dir.exists(dirname(file.path(tmpWD, data_echo, fsep = fsep))))
        dir.create(dirname(file.path(tmpWD, data_echo, fsep = fsep)))
      file.copy(
        from = file.path(modelFrom, data_echo, fsep = fsep),
        to = file.path(tmpWD, data_echo, fsep = fsep),
        overwrite = TRUE, copy.date = TRUE
      )
    } else {
      cat("WARNING\n")
      cat("- The 'data_echo.ss_new' file does not exist in the directory of the following base model:\n")
      cat(file.path(modelFrom, "run", fsep = fsep),"\n")
      cat("\t=> If you are planning to modify the 'control file' of your new model, you will first need to run again your 
 base model to get its 'data_echo.ss_new' file.\n")
      cat("\n")
      if(length(new_model)>1 && m<length(new_model))
        cat("- Copying SS input files from :")
    }
  }
  # ========================================================
  
  # 4. Create a descriptive .txt file for this analysis ----
  # ======================================================== #
  # Ask the user for a description of the sensitivity analysis
  Features <- NULL
  if (length(new_model) > 1){
    Mod_Feat <- NULL
    cat("\nYou are going to build different models for this sensitivity analysis.\n")
    cat("\nPlease first provide a global description of this analysis and then a description of each model.\n")
  } else {
    cat("Please first provide a global description of this analysis.")
  }
  
  # Get the global description of the sensitivity analysis
  while (is.null(Features)) {
    Impl <-
      readline(prompt = "Please describe the general features of this sensistivity analysis:")
    Features <- Impl
    Sys.sleep(0.1)
  }
  
  # Get a description of each model
  if (length(new_model) > 1) {
    Nimpl <- length(new_model)
    while (is.null(Mod_Feat)) {
      for (i in 1:Nimpl) {
        eval(parse(text = paste0(
          "Mod_", i, " <- readline(prompt = 'Please describe the ", new_model[i], " model: ')"
        )))
        
        eval(parse(text = paste0(
          "Mod_", i, " <- paste0('\n# - Model ", new_model[i], ":\n# ', Mod_", i, "\n)"
        )))
      }
      if (i == Nimpl)
        eval(parse(text =
                     paste0(
                       'Mod_Feat <-paste(',
                       paste(' Mod_', 1:Nimpl, sep = '', collapse = ','),
                       ')'
                     )))
      Sys.sleep(0.1)
    }
  }
  
  timeSA <- ac(Sys.time())
  out <- file.path(WD, "Sensitivity_Analysis_Features.txt", fsep = fsep)
  
  environment(write_SA_files) <- environment()
  
  write_SA_files(out = out)
  cat("- The 'Sensitivity_Analysis_Features.txt' file has been created in the",
      paste0("'",folder_name,"'"),"folder.")
  
  
  # fs::file_create(path = out)
  # base::sink(out)
  # cat("# ============================================================ #\n")
  # cat("#              Sensitivity analysis description \n")
  # cat("# ============================================================ #\n")
  # cat("# \n")
  # cat("# *** \n")
  # cat("# Topic of the sensitivity analysis: ", topic, "\n")
  # cat("# Specific item in that topic: ", object, "\n")
  # cat("# Author: ", author, "\n")
  # cat("# Date:", timeSA, "\n")
  # cat("# Sensitivity Analysis ID:", SA_ID, "\n")
  # if (length(new_model) > 1) {
  #   cat("# Names of the models created:\n")
  #   for(m in 1:length(new_model))
  #     cat("# - ", new_model[m], "\n")
  # } else {
  #   cat("# Name of the model created:", new_model, "\n")
  # }
  # cat("# *** \n")
  # cat("# \n")
  # if (length(new_model) > 1) {
  #   cat("# This analysis has been developed based on the following models: \n")
  #   cat("# New model \t Base model\n")
  #   for(m in 1:length(new_model))
  #     cat("#", new_model[m], "\t",base_model[m],"\n")
  # } else {
  #   cat("# This analysis has been developed based on the following model: ", base_model, "\n")
  # }
  # cat("# \n")
  # cat("# Results are stored in the following foler: \n")
  # cat("#\t", file.path(dir_SensAnal,folder_name,fsep = fsep), "\n")
  # cat("# Name of the script used to build the new model: \n")
  # cat("#\t",file.path(dirScript_SensAnal,script_model, fsep = fsep), ".R\n")
  # cat("# Name of the script used to analyze the results from this model: \n")
  # cat("#\t",file.path(dirScript_SensAnal,script_results, fsep = fsep), ".R\n")
  # cat("# \n")
  # if(length(new_model) > 1){
  #   cat("# General features: \n")
  #   cat("#",Features,"\n")
  #   cat("# \n")
  #   cat("# Model features:")
  #   # cat("#", Mod_Feat, "\n")
  #   cat(Mod_Feat)
  # } else {
  #   cat("# Features: \n")
  #   cat(Features)
  # }
  # cat("\n")
  # base::sink()
  # ========================================================

  # 5. Update the "Summary_Sensitivity_analysis.pdf" ----
  # ======================================================== #
  cat("- Now updating the 'Summary_Sensitivity_analysis.pdf' file.\n")
  SumUp <- SumUp %>% 
    tibble::add_row('SA number' = SA_ID,
            Topic = topic,
            Object = object,
            Author = author,
            Date = timeSA,
            Folder = folder_name,
            'Script model' = script_model,
            'Script results' = script_results,
            'Base model' = base_model,
            'New model' = new_model,
            Features = Features)
  suppressMessages(SumUp <- update_SA_table(SumUp = SumUp, dir_SensAnal = dir_SensAnal))
  # ========================================================
  
  # 6. Update the "Models_Sensitivity_analysis.pdf" ----
  # ======================================================== #
  cat("- Now updating the 'Models_Sensitivity_analysis.pdf' file.\n")
  cat("Please provide a short description for each model you're gonna implement.
\t => Refer to the 'Models_Sensitivity_analysis.pdf' for example.")

  # Get a description of each model
    Nimpl <- length(new_model)
    Mod_dsc <- NULL
    while (is.null(Mod_dsc)) {
      
      for (i in 1:Nimpl) {
        
        eval(parse(text = paste0(
          "dsc_", i, " <- readline(prompt = 'Short description for the ", new_model[i], " model: ')"
        )))
      }
      if (i == Nimpl)
        Mod_dsc <- "ok"
      Sys.sleep(0.1)
    }
    
    Desc <- NULL
    for(m in 1:Nimpl){
      eval(parse(text=paste0(
        "Desc <- c(Desc, dsc_",m,")"
      )))
    }
  Models_SA <- Models_SA %>% 
    tibble::add_row('SA number' = SA_ID,
                    Topic = topic,
                    Object = object,
                    'Model name' = new_model,
                    Description = Desc,
                    path = pathMod
                    )
  suppressMessages(Models_SA <- update_Models_SA_table(Models_SA = Models_SA, dir_SensAnal = dir_SensAnal))
  # ========================================================
  
  # 7. Build the scripts for this sensitivity analysis ----
  # ======================================================== #
  out <- file.path(dirScript_SensAnal, script_model, fsep = fsep)
  write_SA_files(out = out, do_model = TRUE)
  cat("- The",paste0("'",script_model,"'")," file has been created in the",
      paste0("'",dirScript_SensAnal,"'"),"folder.")
  
  out <- file.path(dirScript_SensAnal, script_results, fsep = fsep)
  write_SA_files(out = out, do_results = TRUE)
  cat("- The",paste0("'",script_results,"'")," file has been created in the",
      paste0("'",dirScript_SensAnal,"'"),"folder.")
  # ========================================================

  # 8. Update SA_info ----
  # ======================================================== #
  SA_info <- NULL
  SA_info <- list(SumUp = SumUp, Models_SA = Models_SA)
  save(SA_info, file = file.path(dirScript_SensAnal, "SA_info.RData", fsep = fsep))
  # ========================================================
}


#' 4. Function to automatically build the template scripts for the SA ----
#' ----------------------------------------------------------- #
#' 
#' @param out (character string)- Name of the file created.
#' @param do_model (logical)- if \code{TRUE}, the function will build the 
#' template script to develop the models considered in the SA. \code{default = FALSE} 
#' @param do_results (logical)- if \code{TRUE}, the function will build the 
#' template script to analyze (plot) the models considered in the SA. \code{default = FALSE}
#' 
#' @details 
#' By default the function is set to build the 'Sensitivity_Analysis_Features.txt'
#' file.
#
write_SA_files <- function(out = NULL, 
                           do_model = FALSE,
                           do_results = FALSE){
  
  
  
  #' 4.1. Internal Function to automatically write scripts header ----
  #' ----------------------------------------------------------- #
  #' 
  #' @param do_model (logical)- if \code{TRUE}, the function will build the 
  #' template script to develop the models considered in the SA. \code{default = FALSE} 
  #' @param do_results (logical)- if \code{TRUE}, the function will build the 
  #' template script to analyze (plot) the models considered in the SA. \code{default = FALSE}
  #' @param ModcompWARNING (logical)- Does the base model exist before the development 
  #' of the new model?
  #' @details 
  #' By default the function is set to build the 'Sensitivity_Analysis_Features.txt'
  #' file.
  #
  set_script_header <- function(do_model = NULL,
                                do_results = NULL,
                                ModcompWARNING = NULL){
    cat("# ------------------------------------------------------------ #\n")
    cat("# ------------------------------------------------------------ #\n")
    cat("\n")
    
    if(do_model){
      cat("# This script holds the code used to develop the models considered in this sensitivity analysis.\n")
      cat("# For each model, the base model is modified using the r4ss package. 
# The new input files are then written in the root directory of each model and the 
# model outputs are stored in the '/run/' folder housed in that directory.\n")
      cat("# The results of this sensitivity analysis are analyzed using the following script:\n")
      cat("#",file.path(dirScript_SensAnal,script_results, fsep = fsep),"\n")
      cat("\n")
      
      if(ModcompWARNING){
        cat("# *************************************\n")
        cat("# ---           WARNINGS            ---\n")
        cat("# *************************************\n")
        cat("# \n")
        warnMod_New <- Modcomp[which(Modcomp$warning==1), "new_model"]
        warnMod_Base <- Modcomp[which(Modcomp$warning==1), "base_model"]
        cat("# => The base model(s) you specified for the following new model(s):\n")
        cat("# \t", paste(warnMod_New, collapse = ", "), "\n")
        cat("# do(es) not exist yet. This means you'll have to start by developping its/their own base model before creating your new model !!!!\n")
        cat("# Specifically, you must first develop the following model(s):\n")
        cat("# \t", paste(warnMod_Base, collapse = ", "), "\n")
        cat("# \n")
        cat("# *************************************\n")
        cat("# *************************************\n")
        cat("\n")
      }
      cat("# ------------------------------------------------------------ #\n")
      cat("# ------------------------------------------------------------ #\n")
      cat("\n")
    } else {
      cat("# The results of the model run have been plot in the 'plots' sub-directory of 
# the 'run' folder. The comparison plots between models for the sensitivity analysis will be
# stored in the 'SA_plots' folder housed in the root directory of this sensitivity analysis.\n")
      cat("\n")
      cat("# ------------------------------------------------------------ #\n")
      cat("# ------------------------------------------------------------ #\n")
    }
    cat("\n")
    cat("rm(list = ls(all.names = TRUE))","\n")
    cat("\n")
    cat("# 1. Update r4ss ----\n")
    cat("\n")
    cat("update <- FALSE","\n")
    cat("\n")
    cat("if (update) {","\n")
    cat("# Indicate the library directory to remove the r4ss package from\n")
    cat("mylib <- '~/R/win-library/4.1'","\n")
    cat("remove.packages('r4ss', lib = mylib)","\n")
    cat("# Install r4ss from GitHub","\n")
    cat("remotes::install_github('r4ss/r4ss')","\n")
    cat("}","\n")
    cat("# -----------------------------------------------------------","\n")
    cat("\n")
    cat("# 2. Set up ----","\n")
    cat("\n")
    cat("rm(list = ls(all.names = TRUE))","\n")
    cat("# Local declaration","\n")
    cat("fsep <- .Platform$file.sep #easy for file.path() function","\n")
    cat("\n")
    cat("# packages","\n")
    cat("library(r4ss)","\n")
    cat("library(dplyr)","\n")
    cat("library(reshape2)","\n")
    cat("library(stringr)","\n")
    cat("\n")
    cat("# Directories","\n")
    cat("# Path to the model folder","\n")
    cat("dir_model <- file.path(here::here(), 'model', fsep = fsep)\n")
    cat("\n")
    cat("# Path to the Executable folder","\n")
    cat("Exe_path <- file.path(dir_model, 'ss_executables')","\n")
    cat("\n")
    cat("# Path to the R folder","\n")
    cat("dir_script <- file.path(here::here(), 'R', fsep = fsep)\n")
    cat("\n")
    cat("# Path to the Sensitivity analysis folder","\n")
    cat("dir_SensAnal <- file.path(dir_model, 'Sensitivity_Anal', fsep = fsep)\n")
    cat("\n")
    cat("# Useful function\n")
    cat("source(file=file.path(dir_script,'utils','clean_functions.R', fsep = fsep))\n")
    cat("source(file=file.path(dir_script,'utils','ss_utils.R', fsep=fsep))\n")
    cat("\n")
    cat("# Save directories and function\n")
    cat("# save.dir <- c('dir_model', 
        # 'Exe_path',
        # 'dir_script',
        # 'dir_SensAnal')","\n")
    cat("save.dir <- ls()\n")
    cat("# -----------------------------------------------------------","\n")
    cat("\n")
  }
  
  
  
  
  
  
  
  
  # timeSA <- as.character(Sys.time())
  
  fs::file_create(path = out)
  base::sink(out)
  
  if (!do_model && !do_results) {
    cat("# ============================================================ #\n")
    cat("#              Sensitivity analysis description \n")
    cat("# ============================================================ #\n")
  } else if (do_model){
    cat("# ============================================================ #\n")
    cat("# Script used to develop the model(s) considered in the 
# sensitivity analysis:",SA_ID,"\n")
    cat("# ============================================================ #\n")
    cat("# \n")
    cat("# Sensitivity analysis summary\n")
  } else {
    cat("# ============================================================ #\n")
    cat("# Script used to analyze and compare the model(s) considered in the 
# sensitivity analysis:",SA_ID,"\n")
    cat("# ============================================================ #\n")
    cat("# \n")
    cat("# Sensitivity analysis summary\n")
  }
  cat("# \n")
  cat("# *** \n")
  cat("# Topic of the sensitivity analysis: ", topic, "\n")
  cat("# Specific item in that topic: ", object, "\n")
  cat("# Author: ", author, "\n")
  cat("# Date:", timeSA, "\n")
  if (!do_model && !do_results)
    cat("# Sensitivity Analysis ID:", SA_ID, "\n")
  if (length(new_model) > 1) {
    cat("# Names of the models created:\n")
    for(m in 1:length(new_model))
      cat("# - ", new_model[m], "\n")
  } else {
    cat("# Name of the model created:", new_model, "\n")
  }
  cat("# *** \n")
  cat("# \n")
  if (length(new_model) > 1) {
    cat("# This analysis has been developped based on the following models: \n")
    cat("# New model \t Base model\n")
    for(m in 1:length(new_model))
      cat("#", new_model[m], "\t",base_model[m],"\n")
  } else {
    cat("# This analysis has been developped based on the following model: ", base_model, "\n")
  }
  cat("# \n")
  cat("# Results are stored in the following foler: \n")
  cat("#\t", file.path(dir_SensAnal,folder_name,fsep = fsep), "\n")
  if(!do_model && !do_results){
    cat("# Name of the script used to build the new model: \n")
    cat("#\t",file.path(dirScript_SensAnal,script_model, fsep = fsep), ".R\n")
    cat("# Name of the script used to analyze the results from this model: \n")
    cat("#\t",file.path(dirScript_SensAnal,script_results, fsep = fsep), ".R\n")
  }
  cat("# \n")
  if(length(new_model) > 1){
    cat("# General features: \n")
    cat("#",Features,"\n")
    cat("# \n")
    cat("# Model features:")
    # cat("#", Mod_Feat, "\n")
    cat(Mod_Feat,"\n")
  } else {
    cat("# Features: \n")
    cat(Features,"\n")
  }
  cat("# ============================================================ #\n")
  cat("\n")
  if(do_model || do_results){
    set_script_header(do_model = do_model,
                      do_results = do_results,
                      ModcompWARNING = ModcompWARNING)
    stp <- 3
  }
  
  if(do_model){
    # cat("# ------------------------------------------------------------ #\n")
    # cat("# ------------------------------------------------------------ #\n")
    # cat("\n")
#     cat("# This script holds the code used to develop the models considered in this sensitivity analysis.\n")
#     cat("# For each model, the base model is modified using the r4ss package. 
# The new input files are then written in the root directory of each model and the 
# model outputs are stored in the '/run/' folder housed in that directory.\n")
#     cat("# The results of this sensitivity analysis are analyzed using the following script:\n")
#     cat("#",file.path(dirScript_SensAnal,script_results, fsep = fsep),"\n")
#     cat("\n")
#     
#     if(ModcompWARNING){
#       cat("# *************************************\n")
#       cat("# ---           WARNINGS            ---\n")
#       cat("# *************************************\n")
#       cat("# \n")
#       warnMod_New <- Modcomp[which(Modcomp$warning==1), "new_model"]
#       warnMod_Base <- Modcomp[which(Modcomp$warning==1), "base_model"]
#       cat("# => The base model(s) you specified for the following new model(s):\n")
#       cat("# \t", paste(warnMod_New, collapse = ", "), "\n")
#       cat("# do(es) not exist yet. This means you'll have to start by developing its/their own base model before creating your new model !!!!\n")
#       cat("# Specifically, you must first develop the following model(s):\n")
#       cat("# \t", paste(warnMod_Base, collapse = ", "), "\n")
#       cat("# \n")
#       cat("# *************************************\n")
#       cat("# *************************************\n")
#       cat("\n")
#     }
    
    # cat("# ------------------------------------------------------------ #\n")
    # cat("# ------------------------------------------------------------ #\n")
    # cat("\n")
    # cat("\n")
    # cat("rm(list = ls(all.names = TRUE))","\n")
    # cat("\n")
    # cat("# 1. Update r4ss ----\n")
    # cat("\n")
    # cat("update <- FALSE","\n")
    # cat("\n")
    # cat("if (update) {","\n")
    # cat("# Indicate the library directory to remove the r4ss package from\n")
    # cat("mylib <- '~/R/win-library/4.1'","\n")
    # cat("remove.packages('r4ss', lib = mylib)","\n")
    # cat("# Install r4ss from GitHub","\n")
    # cat("remotes::install_github('r4ss/r4ss')","\n")
    # cat("}","\n")
    # cat("# -----------------------------------------------------------","\n")
    # cat("\n")
    # cat("# 2. Set up ----","\n")
    # cat("\n")
    # cat("rm(list = ls(all.names = TRUE))","\n")
    # cat("# Local declaration","\n")
    # cat("fsep <- .Platform$file.sep #easy for file.path() function","\n")
    # cat("\n")
    # cat("# packages","\n")
    # cat("library(r4ss)","\n")
    # cat("library(dplyr)","\n")
    # cat("library(reshape2)","\n")
    # cat("library(stringr)","\n")
    # cat("\n")
    # cat("# Directories","\n")
    # cat("# Path to the model folder","\n")
    # cat("dir_model <- file.path(here::here(), 'model', fsep = fsep)\n")
    # cat("\n")
    # cat("# Path to the Executable folder","\n")
    # cat("Exe_path <- file.path(dir_model, 'ss_executables')","\n")
    # cat("\n")
    # cat("# Path to the R folder","\n")
    # cat("dir_script <- file.path(here::here(), 'R', fsep = fsep)\n")
    # cat("\n")
    # cat("# Path to the Sensitivity analysis folder","\n")
    # cat("dir_SensAnal <- file.path(dir_model, 'Sensitivity_Anal', fsep = fsep)\n")
    # cat("\n")
    # cat("# Useful function\n")
    # cat("source(file=file.path(dir_script,'utils','clean_functions.R', fsep = fsep))\n")
    # cat("source(file=file.path(dir_script,'utils','ss_utils.R', fsep=fsep))\n")
    # cat("\n")
    # cat("# Save directories and function\n")
    # cat("# save.dir <- c('dir_model', 
    #     # 'Exe_path',
    #     # 'dir_script',
    #     # 'dir_SensAnal')","\n")
    # cat("save.dir <- ls()\n")
    # cat("# -----------------------------------------------------------","\n")
    # cat("\n")
    


    
    for(m in 1:dim(Modcomp)[1]){
      Dirmod <- NULL
      cat("# ", paste0(stp,"."), "Developing model", Modcomp[m,"new_model"]," ----\n")
      cat("# ----------------------------------------------------------- #\n")
      cat("\n")
      cat("# Path to the",Modcomp[m,"new_model"],"repertory\n")
      tmp <- gsub(pattern = file.path("model", "Sensitivity_Anal", fsep = fsep),
                  replacement = "", x = Models_SA[Models_SA$`Model name`==Modcomp[m,"new_model"], "path"])
      tmp <- stringr::str_split_fixed(string = tmp, pattern = fsep, n = 3)
      cat(paste0("Dir_",gsub('\\.', '_', Modcomp[m,"new_model"])), "<- file.path(dir_SensAnal,",
          paste0("'",paste(tmp[2],tmp[3], sep="','"),"'"),",fsep = fsep)\n")
      Dirmod <- paste0("Dir_",gsub('\\.', '_', Modcomp[m,"new_model"]))
      cat("# Add the model directory to the saved variables\n")
      cat("save.dir <- c(save.dir,",paste0("'",Dirmod,"')"),"\n")    
      cat("\n")
      cat("\n")
      
      # Starter file
      cat("#",paste0(stp,".1")," Work on the Starter file ----\n")
      cat("# ======================= #")
      cat("\n")
      cat("# Read in the file\n")
      cat("StarterFile <-",paste0("file.path(",Dirmod,","),"'starter.ss', fsep = fsep)\n")
      cat("Starter <- SS_readstarter(file = StarterFile,
          verbose = TRUE)\n")
      cat("\n")
      cat("# Make your modification if applicable\n")
      cat("# Code modifying the starter file\n")
      cat("# ..... \n")
      cat("# ..... \n")
      cat("\n")
      cat("\n")
      cat("# Save the starter file for the model\n")
      cat("SS_writestarter(
        mylist = Starter,
        dir = ",paste0(Dirmod,","),"
        overwrite = TRUE,
        verbose = TRUE
      )\n")
      cat("\n")
      cat("# Check file structure\n")
      cat("StarterFile <-",paste0("file.path(",Dirmod,","),", 'starter.ss')\n")
      cat("Starter <- SS_readstarter(file = StarterFile, verbose = TRUE)\n")
      cat("\n")
      cat("# clean environment\n")
      cat("var.to.save <- c(save.dir, 'Starter')\n")
      cat("rm(list = setdiff(ls(), var.to.save))\n")
      cat("var.to.save <- ls()\n")
      cat("# =======================")
      cat("\n")
      cat("\n")
      
      # Data file
      cat("#",paste0(stp,".2")," Work on the data file ----\n")
      cat("# ======================= #")
      cat("\n")
      cat("# Read in the file\n")
      cat("DatFile <-",paste0("file.path(",Dirmod,","),"Starter$datfile, fsep = fsep)\n")
      cat("
      dat <-
        SS_readdat_3.30(file = DatFile,
                        verbose = TRUE,
                        section = TRUE)\n")
      cat("\n")
      cat("# Make your modification if applicable\n")
      cat("# Code modifying the data file \n")
      cat("# ..... \n")
      cat("# ..... \n")
      cat("\n")
      cat("\n")
      cat("# Save the data file for the model\n")
      cat("SS_writedat(
        datlist = dat,
        outfile =",paste0("file.path(",Dirmod,","),"'SST_data.ss', fsep = fsep),
        version = '3.30',
        overwrite = TRUE
      )\n")
      
      cat("# Check file structure ----\n")
      cat("DatFile <-",paste0("file.path(",Dirmod,","),"'SST_data.ss', fsep = fsep)\n")
      cat("SSTdat_V330 <-
        SS_readdat_3.30(file = DatFile,
                        verbose = TRUE,
                        section = TRUE)\n")
      
      # clean environment\n")
      cat("rm(list = setdiff(ls(), var.to.save))\n")
      cat("var.to.save <- ls()\n")
      cat("# =======================")
      cat("\n")
      cat("\n")
      
      # Control file
      cat("#",paste0(stp,".3")," Work on the control file ----\n")
      cat("# ======================= #")
      cat("\n")
      cat("# The SS_readctl_3.30() function needs the 'data_echo.ss_new' file to read the control file
# This file is created while running SS. You may have had a warning when building
# this script. Please check that the existance of the 'data_echo.ss_new' file
# in the 'run' folder of your new model.\n")
      cat("\n")
      cat("# Read in the file\n")
      cat("Ctlfile <-",paste0("file.path(",Dirmod,","),"Starter$ctlfile, fsep = fsep)\n")
      
      cat("ctl <- SS_readctl_3.30(
        file = Ctlfile,
        use_datlist = TRUE,
        datlist =",paste0("file.path(",Dirmod,","),"'run','data_echo.ss_new', fsep = fsep),
        verbose = TRUE
      )\n")
      cat("\n")
      cat("# Make your modification if applicable\n")
      cat("# Code modifying the control file \n")
      cat("# ..... \n")
      cat("# ..... \n")
      cat("\n")
      cat("\n")
      cat("# Save the control file for the model\n")
      cat("SS_writectl(
        ctllist = ctl,
        outfile =",paste0("file.path(",Dirmod,","),"'SST_control.ss'),
        version = '3.30',
        overwrite = TRUE
      )\n")
      cat("# Check file structure ----\n")
      cat("# We actually need to run the model to check the file structure\n")
      
      # clean environment\n")
      cat("rm(list = setdiff(ls(), var.to.save))\n")
      cat("var.to.save <- ls()\n")
      cat("# =======================")
      cat("\n")
      cat("\n")
      
      
      # Forecast file
      cat("#",paste0(stp,".4")," Work on the forecast file ----\n")
      cat("# ======================= #\n")
      cat("\n")
      cat("# Read in the file\n")
      cat("ForeFile <-",paste0("file.path(",Dirmod,","),"'forecast.ss', fsep = fsep)\n")
      cat("Forec <-
        SS_readforecast(
          file = ForeFile,
          version = '3.30',
          verbose = T,
          readAll = T
        )\n")
      cat("\n")
      cat("# Make your modification if applicable\n")
      cat("# Code modifying the forecast file \n")
      cat("# ..... \n")
      cat("# ..... \n")
      cat("\n")
      cat("\n")
      cat("# Save the forecast file for the model\n")
      cat("SS_writeforecast(
        mylist = Forec,
        dir =",paste0(Dirmod,","),"
        file = 'forecast.ss',
        writeAll = TRUE,
        verbose = TRUE,
        overwrite = TRUE
      )\n")
      cat("# Check file structure ----\n")
      
      
      cat("ForeFile <-",paste0("file.path(",Dirmod,","),"'forecast.ss', fsep = fsep)\n")
      cat("Forec <-
        SS_readforecast(
          file = ForeFile,
          version = '3.30',
          verbose = T,
          readAll = T
        )\n")
      # clean environment\n")
      cat("rm(list = setdiff(ls(), var.to.save))\n")
      cat("var.to.save <- ls()\n")
      cat("# =======================\n")
      cat("\n")
      cat("# You are now ready to run this new model\n")
      cat("\n")
      cat("# *********************************************************** #\n")
      cat("#",paste0(stp,".5")," Run the new model using the new input files ----\n")
      cat("# ======================= #")
      cat("\n")
      cat("run_SS(
        SS_version = '3.30.21',
        # version of SS
        base_path =",paste0("file.path(",Dirmod,","),"
        # root directory where the input file are housed
        pathRun = NULL,
        # A 'run' folder is created if needed (where output files will be stored)
        copy_files = TRUE,
        # copy the input files from the",Modcomp[m,"new_model"],"folder
        cleanRun = TRUE
        # clean the folder after the run
      )\n")
      cat("\n")
      cat("#",paste0(stp,".")," Let's plot the outputs from this model ----\n")
      cat("# Making the default plots ----\n")
      cat("# ======================= #\n")
      
      cat("# read the model output and print diagnostic messages\n")
      cat("Dirplot <-",paste0("file.path(",Dirmod,",")," 'run', fsep = fsep)\n")
      cat("\n")
      cat("replist <- SS_output(
        dir = Dirplot,
        verbose = TRUE,
        printstats = TRUE
      )\n")
      cat("\n")
      
      cat("# plots the results (store in the 'plots' sub-directory)\n")
      cat("SS_plots(replist,
               dir = Dirplot,
               printfolder = 'plots')\n")
      cat("\n")
      cat("# *********************************************************** #\n")
      cat("\n")
      cat("\n")
      if (m == dim(Modcomp)[1]) {
        cat("# -----------------------------------------------------------\n")
        cat("# -----------------------------------------------------------\n")
        cat("\n")
        cat(
          "# You are ready to analyze differences between the models 
# considered in this sensitivity analysis.\n"
        )
        cat("\n")
        cat("# -----------------------------------------------------------\n")
        cat("# -----------------------------------------------------------\n")
      } else {
        cat("# -----------------------------------------------------------\n")
        cat("# -----------------------------------------------------------\n")
        cat("\n")
        cat("# You can now develop the next 'new model'.\n")
        cat("\n")
        cat("# -----------------------------------------------------------\n")
        cat("# -----------------------------------------------------------\n",sep = "")
      }
    } # end loop on m
    
  } else if(do_results){

    cat("#",paste0(stp,".4")," Make comparison plots between models ----\n")
    cat("# ======================= #\n")
    cat("\n")
    
    cat("# Use the SSgetoutput() function that apply the SS_output()\n")
    cat("# to get the outputs from different models\n")

    # path to the base model directory
    SaveDir <- NULL
    SaveMod <- NULL
    if (length(unique(Modcomp$base_model)) != 1) {
      for (m in 1:dim(Modcomp)[1]) {
        if (m == 1)
          cat(
            "# Multiple base models have been used for this analysis. Please to realize
# comparisons between those models, be sure that they are build in a 'hierarchical'.\n"
          )
        cat("\n")
        cat("# Path to the base model (", Modcomp[m, "base_model"], ") repertory\n")
        tmp <-
          gsub(
            pattern = file.path("model", "Sensitivity_Anal", fsep = fsep),
            replacement = "",
            x = Models_SA[Models_SA$`Model name` == Modcomp[m, "base_model"], "path"]
          )
        tmp <-
          stringr::str_split_fixed(string = tmp,
                                   pattern = fsep,
                                   n = 3)
        tmp <- tmp[!tmp %in% ""]
        cat(
          paste0("Dir_Base", gsub('\\.', '_', Modcomp[m, "new_model"])),
          "<- file.path(dir_SensAnal,",
          paste0("'", paste(tmp, collapse = "','"), "'"),
          ", 'run',fsep = fsep)\n"
        )
        cat("\n")
        SaveDir <- c(SaveDir, paste0("Dir_Base", gsub('\\.', '_', Modcomp[m, "new_model"])))
        SaveMod <- c(SaveMod, Modcomp[m, "base_model"])
      }
      
    } else {
      cat("# Path to the base model (",Modcomp[1,"base_model"],") repertory\n")
      tmp <- gsub(pattern = file.path("model", "Sensitivity_Anal", fsep = fsep),
                  replacement = "", x = Models_SA[Models_SA$`Model name`==Modcomp[1,"base_model"], "path"])
      tmp <- stringr::str_split_fixed(string = tmp, pattern = fsep, n = 3)
      tmp <- tmp[!tmp%in%""]
      cat("Dir_Base <- file.path(dir_SensAnal,",
          paste0("'",paste(tmp, collapse="','"),"'"),", 'run',fsep = fsep)\n")
      
      SaveDir <- c(SaveDir, 'Dir_Base')
      SaveMod <- c(SaveMod, Modcomp[1, "base_model"])
    }
    
    for(m in 1:dim(Modcomp)[1]){
      # Path to each new model directory
      Dirmod <- NULL
      cat("# Path to the",Modcomp[m,"new_model"],"repertory\n")
      tmp <- gsub(pattern = file.path("model", "Sensitivity_Anal", fsep = fsep),
                  replacement = "", x = Models_SA[Models_SA$`Model name`==Modcomp[m,"new_model"], "path"])
      tmp <- stringr::str_split_fixed(string = tmp, pattern = fsep, n = 3)
      cat(paste0("Dir_",gsub('\\.', '_', Modcomp[m,"new_model"])), "<- file.path(dir_SensAnal,",
          paste0("'",paste(tmp[2],tmp[3], sep="','"),"'"),", 'run',fsep = fsep)\n")
      cat("\n")
      SaveDir <- c(SaveDir, paste0("Dir_Base", gsub('\\.', '_', Modcomp[m,"new_model"])))
      SaveMod <- c(SaveMod, Modcomp[m, "new_model"])
    }
    cat("\n")
    cat("# Extract the outputs for all models\n")
    cat("SensiMod <- SSgetoutput(dirvec = c(",paste0(SaveDir,sep='', collapse = ','),"))\n")
    cat("\n")
    
    cat("# Rename the list holding the report files from each model\n")
    cat("names(SensiMod)\n")
    cat("names(SensiMod) <- c(",paste0("'",paste(SaveMod, collapse="','"),"'"),")\n",sep = "")
    cat("\n")
    
    cat("# summarize the results\n")
    cat("Version_Summary <- SSsummarize(SensiMod)\n")
    cat("\n")
    
    cat("# make plots comparing the models\n")
    
    cat("SSplotComparisons(
      Version_Summary,
      # print = TRUE,
      pdf = TRUE,
      plotdir = SST_path,
      legendlabels = c(",paste0("'",paste(SaveMod, collapse="','"),"'"),")
    )\n",sep = "")
  } # if(do_results)
  base::sink()
}

# ----------------------------------------------------------