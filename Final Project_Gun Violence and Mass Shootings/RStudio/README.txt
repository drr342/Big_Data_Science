The following files are included as the results of the analysis performed using RStudio:

- R script files:
    1) analysis2014.R - R script for the analysis of the 2014 ZIP Codes Dataset.
    2) analysis2015.R - R script for the analysis of the 2015 ZIP Codes Dataset.
    3) analysis2016.R - R script for the analysis of the 2016 ZIP Codes Dataset.
    4) Stanford.R ----- R script for the analysis of the Stanford MSA Dataset.

- RStudio workspace files:
    1) ranking2014.RData ---- Workspace with the results of the feature ranking process for the 2014 ZIP Codes Dataset.
    2) ranking2015.RData ---- Workspace with the results of the feature ranking process for the 2015 ZIP Codes Dataset.
    3) ranking2016.RData ---- Workspace with the results of the feature ranking process for the 2016 ZIP Codes Dataset.
    4) selection2014.RData -- Workspace with the results of the feature selection process for the 2014 ZIP Codes Dataset.
    5) selection2015.RData -- Workspace with the results of the feature selection process for the 2015 ZIP Codes Dataset.
    6) selection2016.RData -- Workspace with the results of the feature selection process for the 2016 ZIP Codes Dataset.
    7) clustering2014.RData - Workspace with the results of the clustering process for the 2014 ZIP Codes Dataset.
    8) clustering2015.RData - Workspace with the results of the clustering process for the 2015 ZIP Codes Dataset.
    9) clustering2016.RData - Workspace with the results of the clustering process for the 2016 ZIP Codes Dataset.
   10) Stanford.RData ------- Workspace with the results of the complete analysis for the Stanford MSA Dataset.

- The instructions to execute the script files can be found as comments within each script.
- It is important to install and load all the required libraries for the scripts to execute successfully.
- It is also important to specify the right path when loading files, either raw data (.csv files) or previously created
  workspaces (.RData files).
- Some steps of the analysis are very compute-intensive and can take several hours to complete (an approximate execution
  time is included in the script files). In order to avoid recomputing the models, simply load the workspaces provided,
  which include all the resources necessary for data visualization.
