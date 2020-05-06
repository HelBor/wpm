<p align="center"><img width=40% src="https://github.com/HelBor/wpm/blob/master/inst/wpmApp/www/images/wpm_logo.png"></p>
<p align="center"><img width=70% src="https://github.com/HelBor/wpm/blob/master/inst/wpmApp/www/images/wpm_name.png"></p>

![R](https://img.shields.io/badge/R-v3.4+-blue?style=flat-square)
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

### Launch WPM

#### in RStudio or R console

```R
library(wpm)
wpm()
```
A new window will open in your default browser.

## How to use WPM

Since WPM is a GUI, the idea is to just provide a minimum of parameters to the application.
No programming skills are required. WPM supports multiple plates and places samples in a balanced way among the plates.

To see a complete Tutorial, please see the Vignette of the package. 
```R
browseVignettes("wpm")
```

WPM has 4 main panels:

* __Home__
* __Parameters__
* __Results__
* __Help__


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
