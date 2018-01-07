## Welcome to Fish Bioenergetics 4.0

You have downloaded FB4, version v1.0.3 (January 7, 2018) Details about this update are given at the bottom of this file.
 
The FB4 software, User Manual, and sample input files are all in the same zipped file. Instructions for getting started are in the User Manual (included in the zip file) and are also summarized below.

The Open Access article by Deslauriers et al. (2017) gives an introduction to this R-based modeling application.

### Instructions
#### Steps to download and install Fish Bioenergetics 4.0

Fish Bioenergetics 4.0 (FB4) uses R as a computing language and Shiny through RStudio as the supporting interface. You will need both of these programs in order to be able to run FB4 on your computer.

- Go to http://cran.r-project.org and download the latest version of R that is compatible with your computer (Windows or Mac).

- Go to http://www.rstudio.com/products/rstudio/download/, click on the button to download the free RStudio Desktop version, then download the latest version of RStudio under "Installers for supported platforms" that is compatible with your computer (Windows or Mac) operating system.

- Once both programs have been installed, save the FB4 folder downloaded from this website onto your desktop (or another suitable location).

- In RStudio, select File -> Open File... and double click on the "server.R" file found in the FB4 folder on your desktop. This step only needs to be repeated if the server.R file has been modified or a different one is used, or if a different FB4 folder is used.

- In RStudio, go to Session -> Set Working Directory -> Choose Directory... and select the FB4 folder you just saved. Once the folder has been selected, press the select (in Windows) or open (in Mac) tab. This step only needs to be performed once if the same FB4 folder is continually used. However, this step will have to be repeated if a different FB4 folder is created.

- You will now need to download the "Shiny" package, which will allow you to run the application. To do so, go to Tools -> Install Packages... and type Shiny under Packages (separate multiple with space or comma). Then, click on Install. This might take 1-2 minutes.

- Once you successfully install and open FB4 you are ready to run bioenergetics simulations. To run a basic simulation, you will need to supplement the program with Initial Settings and Input Files. For details, see the User Manual PDF.


### FB4 versions:
- Latest version: FB4 v1.0.3, was released 1/7/2018. This minor release adds the creation of a log file (FB4_Log_File.csv), which is written to the current directory after each run. After the file is first created, information from subsequent runs is appended to the previous file. This log file records the date-time, program version, input values, file names used for the simulation run, and summary output values. These input and output values are also printed to the RStudio Console. If you open FB4_Log_File.csv in Excel to see the values, be sure to close the file before doing another run, or else you will get an error: R will not be able to write (append) new information to the log file while it is open in Excel.

- Latest version: FB4 v1.0.2, was released 12/13/2017. Click the "Download .zip" button at the top of this page to obtain the latest version. This minor update includes checks to avoid reading some additional input files that are not needed for a particular run. The Mortality input file is not read unless that sub-model is selected. Reproduction file is not read unless that sub-model is selected. If the last day of values in these two files are less than the Final Day of the simulation entered in Initial Values, then a caution message is displayed on the Sub-Model graph to remind the user there may be a problem unless more days are added to the file. The purpose of these changes is to help prevent file errors for new users, who may delete files or forget to update files they don't think they need.

- FB4 v1.0.1 was released 12/6/2017. This minor update includes checks to avoid reading input files that are not needed for the selected species parameters. The Indigestible_prey file is not read unless EGEQ == 3; the program needs this file only if Egestion Model 3 is being used. Similarly, the Pred_E file is not read unless PREDEDEQ == 1; this file is not needed if predator energy density is based on weight (PREDEDEQ 2 or 3). The purpose of these changes is to help prevent file errors for new users, who may delete files or forget to update prey types in files they don't think they need.

- FB4 v1.0.0 was released on 12/1/2017.
