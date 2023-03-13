# Comparing Shortspine Thornyhead 2013 model (SS V3.24) with SS V3.30.21
# Matthieu VERON - @: mveron@uw.edu
# Last updated: February 2013

# The transition has been realized using the "Transition_SST_From_V324_To_V330.R"
# script.

# This script  realized the comparison between the two version of the 2013 model
# It uses functions from the r4ss package.

rm(list = ls(all.names = TRUE))

# 1. Update r4ss ----

update <- FALSE

if (update) {
  # Indicate the library directory to remove the r4ss package from
  mylib <- "~/R/win-library/4.1"
  remove.packages("r4ss", lib = mylib)
  # Install r4ss from GitHub
  remotes::install_github("r4ss/r4ss")
}
# -----------------------------------------------------------

# 2. Set up ----

# Local declaration
fsep <- .Platform$file.sep #easy for file.path() function

# packages
library(r4ss)

# Useful function
source(file=file.path(here::here(), "R","utils","clean_functions.R", fsep = fsep))
source(file=file.path(here::here(), "R","utils","ss_utils.R", fsep=fsep))

# Directories
dirBase <- here::here()
# Path to the old SST model (i.e. the 2013 model using SS V3.24)
oldSST_path <- file.path(dirBase, "model","2013_SST")
# Path to the new SST model (i.e. the 2013 model using SS V3.30.21)
SST_path <- file.path(dirBase, "model","2013_SST_SSV3_30_21")
# SST_pathRUN <- file.path(SST_path, "run", fsep = fsep)

save.dir <- c('dirBase',
              'oldSST_path',
              'SST_path')
# -----------------------------------------------------------

# 1. Run the models ----
# ----------------------------------------------------------- #

# ==================================================== #
# Reminder: All executables are stored in the
# "model/ss_executables" folder.
# The choice of the .exe to use depends on:
# i. the version of SS you want to work with
# ii. the OS you are working on
# Here this script is set up for a windows machine
# ==================================================== #


# Run the SS V3.24 version of the SST 2013 model
run_SS(
  SS_version = "3.24.U",
  # version of SS
  Exe_extra = "opt",
  # we use the "opt" exe
  base_path = oldSST_path,
  # root directory where the input file are housed
  pathRun = NULL,
  # A 'run' folder is created (where output files will be stored)
  copy_files = TRUE,
  # copy the input files from the oldSST_path folder
  cleanRun = TRUE
  # clean the folder after the run
)

# Run the SS V3.30.21 version of the SST 2013 model
run_SS(
  SS_version = "3.30.21",
  # version of SS
  base_path = SST_path,
  # root directory where the input file are housed
  pathRun = NULL,
  # A 'run' folder is created (where output files will be stored)
  copy_files = TRUE,
  # copy the input files from the oldSST_path folder
  cleanRun = FALSE
  # DO NOT clean the folder after the run
)

# Clean the repertory where we run the 3.30.21 version
clean_bat(path = file.path(SST_path, 'run', fsep = fsep))
# -----------------------------------------------------------


# 2. Establish comparisons ----
# ----------------------------------------------------------- #

# Use the SSgetoutput() function that apply the SS_output()
# to get the outputs from the two versions

old_pathRun <- file.path(oldSST_path, "run", fsep = fsep)
pathRUN <- file.path(SST_path, "run", fsep = fsep)

SST_Vs <- SSgetoutput(dirvec = c(old_pathRun, pathRUN))


# SST_Vs is a named list with the report files from each version
names(SST_Vs)
names(SST_Vs) <- c('V3.24', 'V3.30')

# summarize the version results
Version_Summary <- SSsummarize(SST_Vs)

# make plots comparing the 2 versions
SSplotComparisons(
  Version_Summary,
  # print = TRUE,
  pdf = TRUE,
  plotdir = SST_path,
  legendlabels = c('V3_24', 'V3.30')
)
