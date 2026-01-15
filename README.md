#Learning Analytics:Learner Outcomes and Engagement Patterns
#Author: Lavanya Vijayakumar
#Date: 14/12/26

#This project analyses learner behaviour data from seven runs of FutureLearn online course. The analysis follows the CRISP-DM framework to study learner outcomes and engagement patterns. 
#The project is organised using the ProjectTemplate structure to ensure reproductibility.

#The project addresses the following questions:
 #1. How do learner outcomes(course completion, unenrolment, and certificate purchase) differ across course runs?
 #2. At which stages of the course do learners disengage, and are these disengagement patterns consistent across runs?


#This file provides instructions on project setup and execution.

#Project setup:#
 #The project uses ProjectTemplate system.
 #All raw data are stored in the 'data' folder. 
 #During knitting process the data are cached to the 'cache' folder.
 #All data preprocessing steps are implemented as R scripts in the 'munge' folder.
 #The main analysis and reporting are carried out using an R Markdown file located in the 'reports' folder.
 #Various options are set using 'global.dcf' in the 'config' folder.
 #For this project, munging is set to FALSE unless preprocessing scripts are modified, and load_libraries is set to TRUE.


#Project Execution:#
  #clone the ProjectTemplate repository.
 #To run the analysis, open the file anlaysis_report.Rmd, which is located in the 'reports' folder.
 #Ensure that the project is opened using the .Rproj file so that the working directory is set correctly.
 #Click 'Knit' in Rstudio to reproduce the full analysis and generate final report.

#Directory Map#
 #The project follows the standard ProjectTemplate directory structure:
 
  #cache-stores processed datasets and objects created using preprocessing.
  
  #config- contains the global.dcf file used to set project-wide options.
  
  #data- stores the raw datasets used in the analysis
  
  #diagnostics- currently unused.
  
  #docs- currently unused.
  
  #graphs-currently unused.
  
  #lib- currently unused.
  
  #logs- currently unused.
  
  #munge- Contains R scripts used for data cleaning and preprocessing.
  
  #profiling- currently unused
  
  #README.md- provides an overview of the projects and instructions for setup and execution.
  
  #renv- stores files required to manage package dependencies.
  
  #reports- Contains the main R Markdown file 'analysis_report.Rmd' and the knitted output.
  
  #src- Contains R scripts used to generate summary tables and figures.
  
  #tests- currently unused.







 
