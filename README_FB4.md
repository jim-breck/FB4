## Welcome to Fish Bioenergetics 4.0

You have downloaded FB4, version v1.1.3. (May 26, 2021) (March 4, 2019)
 
The FB4 software, User Manual, and sample input files are all in the same zipped file. Instructions for getting started are in the User Manual (included in the zip file) and are also summarized below.

The Open Access article by Deslauriers et al. (2017) gives an introduction to this R-based modeling application.

### Instructions
#### Steps to download, install and run Fish Bioenergetics 4.0

Fish Bioenergetics 4.0 (FB4) uses __R__ as a computing language and __Shiny__ through __RStudio__ as the supporting interface. You will need both of these programs in order to be able to run FB4 on your computer.

- Go to http://cran.r-project.org and download the latest version of R that is compatible with your computer (Windows or Mac).

- Go to http://www.rstudio.com/products/rstudio/download/, click on the button to download the free RStudio Desktop version, then download the latest version of RStudio under "Installers for supported platforms" that is compatible with your computer (Windows or Mac) operating system.

- Once both programs have been installed, save the FB4 folder downloaded from this website onto your desktop (or another suitable location).

- In RStudio, select __File -> Open File...__ and double click on the "server.R" file found in the FB4 folder on your desktop. This step only needs to be repeated if the server.R file has been modified or a different one is used, or if a different FB4 folder is used.

- In RStudio, go to __Session -> Set Working Directory -> Choose Directory...__ and select the __FB4__ folder you just saved. Once the folder has been selected, press the select (in Windows) or open (in Mac) tab. This step only needs to be performed once if the same FB4 folder is continually used. However, this step will have to be repeated if a different FB4 folder is created.

- You will now need to download the "Shiny" package, which will allow you to run the application. To do so, go to __Tools -> Install Packages...__ and type __Shiny__ under __Packages (separate multiple with space or comma)__. Then, click on __Install__. This might take 1-2 minutes.

- You are now all set to run the application. To do so, press the down-arrow icon to the right of the __Run App__ tab found in the upper right corner of the script window (upper left window) and select __Run External__. Once this is done, press the __Run App__ tab and FB4 will start in a new window of your browser.

Note: In some versions of RStudio, the __Run App__ tab does not appear. In that case, type __runApp()__ in the __Console__ window (lower left window) and insert the FB4 folder path inside the parentheses. The folder path should be inside quotation marks and should look something like: runApp("/Users/david/Desktop/Fish Bioen 4.0") Once done, press __Enter__ and the application should run.
- __Warning__: Any alteration to the __R__ script will cause the application to either be modified or not function. If you inadvertently modify the code, just make sure you do not save the __server.R__ or __ui.R__ files before closing and re-opening them again (or save them under a different name).

A similar set of instructions for getting started is in the FB Wiki: [FB Wiki](https://github.com/jim-breck/FB4/wiki)

### Simulation Setup
#### Setting up the files needed to run bioenergetics simulations

Once you successfully install and open __FB4__ you are ready to run bioenergetics simulations. To run a basic simulation, you will need to supplement the program with __Initial Settings__ and __Input Files__.

#### Initial Settings

* Species

You must first decide on the species you want to use for the simulation. Under the __Initial Settings__ tab, select the species of interest by scrolling through the dropdown menu under __Species__. You can see the species’ common and scientific name, along with the reference and parameters value on the right-hand side of the screen.

* Duration

You must then select the duration of your simulation. The duration of the simulation must not exceed the length of time found in the __Input Files__. In addition, the simulation days are arbitrary and do not fall under any calendar constraints (i.e., day of year). Type your first day in the __Initial Day__ box and your last day in the __Final Day__ box. The days you input here will be used to determine the range of data from the input files to run the simulations.

* Initial weight

All simulations require you to input an initial weight (in grams of wet weight) for the fish used in the simulation. Make sure you use a relevant weight that corresponds to the life stage of the species model you are using.

* Oxycalorific coefficient

This parameter is used to convert oxygen consumption into an energy unit (Joules/grams of O<sub>2</sub>) that is compatible with the modeling process. The default value was taken from Stewart et al. (1983) but be aware that all species do not require the same coefficient (see Elliot & Davison 1975 for more information).

* Fit to

This is where you decide what kind of simulation you want to run. By selecting __Final Weight__, you are telling the program that you want your fish’s final weight (in grams of wet weight) to equal the value you input in the box. The program will then iteratively adjust daily consumption so that the final weight calculated from the simulation matches the final weight you used as an input.

If you select __Consumption__, the program will use the value you input in the box (in grams of total prey wet weight) and assign the fish a daily consumption value so that the total consumption value from the simulation matches the consumption value you used as an input.

By selecting __Ration__, you are telling the program to feed the fish a % of the fish’s wet body weight every day.

Lastly, if you select __P-value__, the program will calculate a consumption value based on maximum consumption (C<sub>max</sub>) on any given day. Remember that C<sub>max</sub> will vary depending on water temperature and fish body mass.

#### Input Files

Input data files contain information that might have been collected in the field, retrieved from historical archives, or pulled from scientific literature. These files are created outside FB4 and are loaded automatically by the program once it is launched. Not all simulations require the same input files but all simulations require the files in the Main Inputs folder to be filled out.

* Main Inputs

The Main Input files include: __Temperature__, __Prey Energy Density__ (__Prey_E__), __Prey Proportions__ (__Diet_prop__), __Predator Energy Density__ (__Pred_E__), and __Indigestible Prey__ (only if using Equation 3 for waste losses; see Stewart et al. 1983). All files are saved in .csv format. Default files have been provided with the Fish Bioen 4.0 folder but these can be modified to reflect your own data. To do so, simply replace the days and input data with your own data and save the .csv file before launching FB4. Data are linearly interpolated for the days where the data are missing. You must always have the same number of prey items for each of the input files (except __Predator Energy Density__) that you plan on using for the simulations.

The mortality file is the one case where daily values are __not__ interpolated. The value specified for one day remains in effect until a different value is specified (see User Manual for details).

__Note__: All input files __must__ start on day 1. Failure to do so will result in FB4 using the wrong set of days for the simulation. 

* Input Files

Once you give FB4 the input data you want to use for your simulation, you can visualize them by selecting the __Input Files__ tab. However, you must make sure that you filled out the __Initial Day__ and __Final Day__ sections under the __Initial Settings__ tab. This will allow you to visualize the input data for the time range you want to run your simulation for. You will also notice that the plots generated under the __Input Files__ tab are reactive. This means that the plots will automatically adjust to reflect the __Initial Day__ and __Final Day__ of your simulation.

* Output (to run the program)

Once you have inspected your input data and arranged your initial settings, you can run your simulation. To do so, go to the __Output__ tab and the program will run. Under this tab, you can see the results of the simulation on a daily basis under the __Table__ tab. To view different outputs, select within the __Variables__ box and many output options will become available. Select the ones of interest and they will appear in the table. Once you have all the data you need, you can download them into a .csv file onto the Fish Bioen 4.0 folder. To do so, select the __Download__ button and save the file to the folder.

* Plot

It is also possible to visualize the outputs in a plot format. To do so, select the __Plot__ tab. This will bring you to a plot with 2 y-axes. You can modify the variables found on either y-axes or the x-axis by scrolling down the dropdown menus below __Plot Output__.

* Summary

Finally, by selecting the __Summary__ tab, you will be able to see the final weight, consumption and p-value calculated or used for the simulation. Keep in mind that the p-value is irrelevant if you choose to run a simulation using the __Fit to: Ration__ option.

### FB4 versions:
- __Latest version: FB4 v1.1.3__. was released 5/26/2021. Click the "Download .zip" button at the top of the web page to obtain the latest version. This version fixes an error in the reporting of Specific.Growth.Rate.g.g.d. Thanks to Steve Blumenshine for identifying and reporting this error.

- __FB4 v1.1.2__. was released 3/4/2019. Click the "Download .zip" button at the top of the web page to obtain the latest version. This version fixes a bug that stopped the program during p-fitting if weight became negative; now, the program prints a message to the Console and continues with the p-fitting.

- __FB4 v1.1.1__, was released 1/28/2019. This version has two minor changes. The first change fixes a bug that had prevented use of the "Download Table" button when a run is completed. Thanks to Nick Barrett for assistance with this fix. The second change is to add another Design File ("FB4_Design_Bluegill_2.csv") and two Temperature files that it uses (Main Inputs/Temperature_20C.csv, and Main Inputs/Temperature_25C.csv). See the note for v1.1.0 for a brief explanation of how to use Design Files.

- __FB4 v1.1.0__, was released 10/31/2018. This version can be used in the same way as earlier versions. There are two minor changes and one major change. One minor change is that the number of decimal places for output of the p-value is increased from 4 to 6 decimal places. A second minor change is the addition of some warning messages if fish weight becomes negative or if the program cannot calculate the new weight at the end of a day, to help in debugging simulations. The major change with this version is the ability to use a __Design File__ to run a series of simulations, each with different input values and input files (e.g., Temperature.csv, Temperature_plus2C.csv, Pred_E.csv, Pred_E_4500.csv). For example, users could compare food consumption by different sizes of fish, or food consumption under different temperature regimes or prey energy densities. The summary output from the series of simulations is written to a separate log file, which the user specifies in the Design File. See the example Design Files that are included. To try using a Design file, pick one of the examples in lines 15-20 of the R code, and remove the starting "#" so that the chosen line of code becomes active. For example, line 17 (Design_File = "FB4_Design_Examples_1-4.csv") specifies a Design file which has input values for all the runs for Examples 1, 2, 3, and 4 in the User Guide. (Example 5 is a separate Design file.) Then go to line 53 of the R code and change FALSE to TRUE:  UseDesignFile <- TRUE  Then, from the top menu in RStudio, click File | Save to save these changes in the code. Then run the model. In this version, you need to pick a Species, set initial and final days, select the fit option and value, in order to start the model going. But if UseDesignFile is TRUE, these user inputs are ignored: only the input values in the Design File are used in the runs. Note that the user can create multiple input files, e.g., for different temperature regimes, different diet files, different energy density files, etc., and can specify the desired files to use for each separate run in the Design File. (A future update will probably allow users to choose a Design file from a menu.)  To stop using a Design File, go to line 53 of the R code, make UseDesignFile <- FALSE and save that change in the R code by clicking File | Save in the top menu in RStudio.

- __FB4 v1.0.3__, was released 1/1/2018. This minor release adds the creation of a log file (FB4_Log_File.csv), which is written to the current directory for each run. This log file records the date-time, input values and file names used for each simulation run, as well as summary output values. These input and output values are also printed to the RStudio Console.

- __FB4 v1.0.2__, was released 12/13/2017. Click the "Download .zip" button at the top of this page to obtain the latest version. This minor update includes checks to avoid reading some additional input files that are not needed for a particular run. The Mortality input file is not read unless that sub-model is selected. Reproduction file is not read unless that sub-model is selected. If the last day of values in these two files are less than the Final Day of the simulation entered in Initial Values, then a caution message is displayed on the Sub-Model graph to remind the user there may be a problem unless more days are added to the file. The purpose of these changes is to help prevent file errors for new users, who may delete files or forget to update files they don't think they need.

- FB4 v1.0.1 was released 12/6/2017. This minor update includes checks to avoid reading input files that are not needed for the selected species parameters. The Indigestible_prey file is not read unless EGEQ == 3; the program needs this file only if Egestion Model 3 is being used. Similarly, the Pred_E file is not read unless PREDEDEQ == 1; this file is not needed if predator energy density is based on weight (PREDEDEQ 2 or 3). The purpose of these changes is to help prevent file errors for new users, who may delete files or forget to update prey types in files they don't think they need.

- FB4 v1.0.0 was released on 12/1/2017.
