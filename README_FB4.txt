## Welcome to Fish Bioenergetics 4.0

You have downloaded FB4, version v1.1.0. (October 31, 2018)  Details about this update are given at the bottom of this file.
 
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
- __Latest version: FB4 v1.1.0__. was released 10/31/2018. Click the "Download .zip" button at the top of the web page to obtain the latest version. This version can be used in the same way as earlier versions. There are two minor changes and one major change. One minor change is that the number of decimal places for output of the p-value is increased from 4 to 6 decimal places. A second minor change is the addition of some warning messages if fish weight becomes negative or if the program cannot calculate the new weight at the end of a day, to help in debugging simulations. The major change with this version is the ability to use a __Design File__ to run a series of simulations, each with different input values and input files (e.g., Temperature.csv, Temperature_plus2C.csv, Pred_E.csv, Pred_E_4500.csv). For example, users could compare food consumption by different sizes of fish, or food consumption under different temperature regimes or prey energy densities. The summary output from the series of simulations is written to a separate log file, which the user specifies in the Design File. See the example Design Files that are included. To try using a Design file, pick one of the examples in lines 15-20 of the R code, and remove the starting "#" so that the chosen line of code becomes active. For example, line 17 (Design_File = "FB4_Design_Examples_1-4.csv") specifies a Design file which has input values for all the runs for Examples 1, 2, 3, and 4 in the User Guide. (Example 5 is a separate Design file.) Then go to line 53 of the R code and change FALSE to TRUE:  UseDesignFile <- TRUE  Then, from the top menu in RStudio, click File | Save to save these changes in the code. Then run the model. In this version, you need to pick a Species, set initial and final days, select the fit option and value, in order to start the model going. But if UseDesignFile is TRUE, these user inputs are ignored: only the input values in the Design File are used in the runs. Note that the user can create multiple input files, e.g., for different temperature regimes, different diet files, different energy density files, etc., and can specify the desired files to use for each separate run in the Design File. (A future update will probably allow users to choose a Design file from a menu.)  To stop using a Design File, go to line 53 of the R code, make UseDesignFile <- FALSE and save that change in the R code by clicking File | Save in the top menu in RStudio.

- __FB4 v1.0.3__, was released 1/7/2018. This minor release adds the creation of a log file (FB4_Log_File.csv), which is written to the current directory after each run. After the file is first created, information from subsequent runs is appended to the previous file. This log file records the date-time, program version, input values, file names used for the simulation run, and summary output values. These input and output values are also printed to the RStudio Console. If you open FB4_Log_File.csv in Excel to see the values, be sure to close the file before doing another run, or else you will get an error: R will not be able to write (append) new information to the log file while it is open in Excel.

- __FB4 v1.0.2__, was released 12/13/2017. This minor update includes checks to avoid reading some additional input files that are not needed for a particular run. The Mortality input file is not read unless that sub-model is selected. Reproduction file is not read unless that sub-model is selected. If the last day of values in these two files are less than the Final Day of the simulation entered in Initial Values, then a caution message is displayed on the Sub-Model graph to remind the user there may be a problem unless more days are added to the file. The purpose of these changes is to help prevent file errors for new users, who may delete files or forget to update files they don't think they need.

- FB4 v1.0.1 was released 12/6/2017. This minor update includes checks to avoid reading input files that are not needed for the selected species parameters. The Indigestible_prey file is not read unless EGEQ == 3; the program needs this file only if Egestion Model 3 is being used. Similarly, the Pred_E file is not read unless PREDEDEQ == 1; this file is not needed if predator energy density is based on weight (PREDEDEQ 2 or 3). The purpose of these changes is to help prevent file errors for new users, who may delete files or forget to update prey types in files they don't think they need.

- FB4 v1.0.0 was released on 12/1/2017.
