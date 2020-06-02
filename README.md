<p align="center"><img width=40% src="https://github.com/HelBor/wpm/blob/master/inst/wpmApp/www/images/wpm_logo.png"></p>
<p align="center"><img width=70% src="https://github.com/HelBor/wpm/blob/master/inst/wpmApp/www/images/wpm_name.png"></p>

![R](https://img.shields.io/badge/R-v4.0+-blue?style=flat-square)
[![GitHub issues](https://img.shields.io/github/issues/HelBor/wpm?style=flat-square)](https://github.com/HelBor/wpm/blob/issues)
![Release](https://img.shields.io/badge/release-alpha-orange?style=flat-square)
![GitHub license](https://img.shields.io/github/license/HelBor/wpm?style=flat-square)

## Brief introduction

> WPM is a shiny application deployed in the form of an R package.
> Its objective is to allow a user to generate a well plate plan in order to perform his experiments by controlling batch effects (in particular preventing plate edge effects).
> The algorithm for placing the samples is inspired by the backtracking algorithm.

## Getting started

### Pre-requisites
`R version >= 4.0.0`
OS tested : `Windows`, `Fedora`, `Ubuntu`,`MacOS`
But the app should work also on the others.

WPM R package dependencies:

`shiny`, `shinydashboard`, `shinyWidgets`, `shinycustomloader`, `DT`,
`RColorBrewer`, `logging`, `dplyr`, `stringr`, `ggplot2`

### How to install

From GitHub
```R
devtools::install_github(repo = "HelBor/wpm")
```


## How to use WPM

There are two ways to use WPM:
* on the command line with the appropriate functions, for users wishing to work
on the command line or integrate wpm into their R scripts.
* via a shiny application (web interface), for users with no R programming 
skills.

### Load the library

```R
library(wpm)
```

### Using WPM from the command line

In command line there a some steps to process in the right order:
#### Prepare dataset
You can work with CSV files, `ExpressionSet` objects or `MSnSet` objects.
The first step is to create a dataframe containing all the data needed by wpm 
to work properly. To do so:
```R
# if you have a CSV file
df <- convertCSV("path-to-your-CSV")
# if you have an ExpressionSet or an MSnSet
df <- convertESet(myExpressionSet) # or convertESet(myMSnSet)
```
 
#### Run WPM

The next step is to run the wpm wrapper function by giving it all the parameters
needed.

```R
wrapperWPM(user_df = df,
            plate_dims = list(8,12),
            nb_plates = 1,
            forbidden_wells = "A1,A2,A3",
            QC_wells = "B1,B2",
            spatial_constraint = "NS")
```

#### Plate map visualization


1. Create the valid dataframe from a CSV file (with `importCSV()`) or from an ExpresssionSet or 
MSnSet object with `importCSV` or `importESet`
2. Use the 


### Use WPM in web interface

Since WPM provides also a GUI, the idea is to just provide a minimum of 
parameters to the application. No programming skills are required.
Simply write in the console:
```R
wpm()
```



To see a complete Tutorial, please see the Vignette of the package. 
```R
browseVignettes("wpm")
```

WPM has 4 main panels:

* __Home__
* __Parameters__
* __Results__


### Provide parameters

- **1)** Provide a CSV file containing the sample names and respective groups if any.

- **2)** Specify the plate dimensions and their number (the user can choose between 6,24,  48,  96,  386,  1534  and  custom)  (WPM  checks  that  all  the  given  settings  arecompatible)

- **3)** Specify the __Forbidden well__: These  wells  will  not  be  filled  with  any  kind  of  sample. We simply do not want to fill them (e.g. the coins of the plate), or in case of dirty wells, broken pipettes, etc.

- **4)** Specify the __Blanks__: correspond to solution without biological sample in it. Provide the neighborhood constraints, which depend on the "Blank" mode chosen. (Shouldn't samples from the same group be found side by side?)

- **5)** Specify the __Not Randomized samples__: correspond to Quality Control samples or standards.

- **6)** Choose a maximum number of iterations that WPM can do to find a solution,then start WPM. If the samples do not have a group, then the samples will be placedcompletely randomly on the plates. If there are groups, wpm will use an algorithminspired by the backtracking algorithm (in order to place the samples in the wellswhile respecting the specified constraints.).


### Check your Results

This Panel allows you to look after the final dataset containing the wells chosen for each sample and a plot of your final well-plate map. Dataframe and plots are downloadable separately.


## Pending Features
* For proteomics, add the option to generate serialization of samples.

## Citing Our work
> The published article of the project will be linked here.
