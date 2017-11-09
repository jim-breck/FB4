## Welcome to Fish Bioenergetics 4.0

New version coming soon! Version 1.0.0 of the FB4 software will be available in the next few days! The new software and an update of the User Manual should be available the week of November 13, 2017.

There are two main goals of this website:
- First, to make the current bioenergetics software available and easy to use;
- Second, to facilitate the wider use and further development of the bioenergetics approach.

We hope to add information such as the following:
- Online User's Manual, with full explanations
- List of species for which bioenergetics parameters have been published
- Table of parameter values (a .csv file comes with the code)
- References to published models
- Background material for an online introduction to fish bioenergetics
- Suggestions for fitting bioenergetics models to data
- Ideas for future modifications

### Instructions
#### Steps to download, install and run Fish Bioenergetics 4.0

The application uses __R__ as a computing language and __RStudio__ as the supporting interface. You will need both of these programs in order to be able to run Fish Bioenergetics 4.0 on your computer.

- Go to http://cran.r-project.org and download the latest version of R that is compatible with your computer (Windows or Mac).
- Go to http://www.rstudio.com/products/rstudio/download/ and download the latest version of RStudio under "Installers for supported platforms" that is compatible with your computer (Windows or Mac).
- Once both programs have been installed, save the Fish Bioenergetics 4.0 (FB4) folder from this website onto your desktop.
- In RStudio, select __File -> Open File...__ and double click on the "server.R" file found in the FB4 folder on your desktop.
- In RStudio, go to __Session -> Set Working Directory -> Choose Directory...__ and select the __FB4__ folder you just saved on your desktop. Once the folder has been selected, press the select (in Windows) or open (in Mac) tab.
- You will now need to download the "shiny" package, which will allow you to run the application. To do so, go to __Tools -> Install Packages...__ and type __shiny__ under __Packages (separate multiple with space or comma)__. Then, click on __Install__. This might take 1-2 minutes.
- You are now all set to run the application. To do so, select the __Run App__ tab found in the upper right corner of the script window. Note: In some versions of RStudio, the __Run App__ tab does not appear. In that case, type __runApp()__ in the __Console__ window (lower left window) and insert the FB4 folder path inside the parentheses. The folder path should be inside brackets and should look something like: runApp("/Users/david/Desktop/FB4") Once done, press __Enter__ and the application should run.
- __Warning__: Any alteration to the __R__ script will result in the application to either be modified or not function. If you inadvertently modify the code, just make sure you do not save the server.R or ui.R files before closing and re-opening them again.

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

The Main Input files include: __Temperature__, __Prey Energy Density__ (__Prey_E__), __Prey Proportions__ (__Diet_prop__), __Predator Energy Density__ (__Pred_E__), and in some instance, __Indigestible Prey__ (only if using Equation 3 for waste losses; see Stewart et al. 1983). All files are saved in .csv format. Default files have been provided with the Fish Bioen 4.0 folder but these can be modified to reflect your own data. To do so, simply replace the days and input data with your own data and save the .csv file before launching FB4. Data are linearly interpolated for the days where the data are missing. You must always have the same number of prey items for each of the input files (except __Predator Energy Density__) you plan on using for the simulations.

* Input Files

Once you give FB4 the input data you want to use for your simulation, you can visualize them by selecting the __Input Files__ tab. However, you must make sure that you filled out the __Initial Day__ and __Final Day__ sections under the __Initial Settings__ tab. This will allow you to visualize the input data for the time range you want to run your simulation for. You will also notice that the plots generated under the __Input Files__ tab are reactive. This means that the plots will automatically adjust to reflect the __Initial Day__ and __Final Day__ of your simulation.

* Output (to run the program)

Once you have inspected your input data and arranged your initial settings, you can run your simulation. To do so, go to the __Output__ tab and the program will run. Under this tab, you can see the results of the simulation on a daily basis under the __Table__ tab. To view different outputs, select within the __Variables__ box and many output options will become available. Select the ones of interest and they will appear in the table. Once you have all the data you need, you can download them into a .csv file onto the Fish Bioen 4.0 folder. To do so, select the __Download__ button and save the file to the folder.

* Plot

It is also possible to visualize the outputs in a plot format. To do so, select the __Plot__ tab. This will bring you to a plot with 2 y-axes. You can modify the variables found on either y-axes or the x-axis by scrolling down the dropdown menus below __Plot Output__.

* Summary

Finally, by selecting the __Summary__ tab, you will be able to see the final weight, consumption and p-value calculated or used for the simulation. Keep in mind that the p-value is irrelevant if you choose to run a simulation using the __Fit to: Ration__ option.
