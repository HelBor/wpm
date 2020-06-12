<p align="center"><img width=40% src="https://github.com/HelBor/wpm/blob/master/inst/app/www/images/wpm_logo.png"></p>
<p align="center"><img width=70% src="https://github.com/HelBor/wpm/blob/master/inst/app/www/images/wpm_name.png"></p>

![R](https://img.shields.io/badge/R-v4.0+-blue?style=flat-square)
[![GitHub issues](https://img.shields.io/github/issues/HelBor/wpm?style=flat-square)](https://github.com/HelBor/wpm/blob/issues)
![Release](https://img.shields.io/badge/release-alpha-orange?style=flat-square)
![GitHub license](https://img.shields.io/badge/license-Artistic--2.0-green?style=flat-square)

## Brief introduction

> WPM is a shiny application deployed in the form of an R package. Functions for
a command-line/script use are also available. Its objective is to allow a user to
generate a well plate plan in order to perform his experiments by controlling 
batch effects (in particular preventing plate edge effects). The algorithm for 
placing the samples is inspired by the backtracking algorithm.

## Getting started

### Pre-requisites
`R version >= 4.0.0`
OS tested : `Windows`, `Fedora`, `Ubuntu`,`MacOS`
But the app should work also on the others.

WPM R package dependencies:
`utils`, `methods`, `Biobase`, `SummarizedExperiment`, `config`, `golem`, 
`rlang`, `shiny`, `shinydashboard`, `shinyWidgets`, `shinycustomloader`, `DT`, 
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

### Supported input formats

| Input Format          | Command line | WPM app |
| --------------------- |:------------:| :------:|
| CSV                   | yes          | yes     |
| ExpressionSet         | yes          | no      |
| SummarizedExperiment  | yes          | no      |
| MSnSet                | yes          | no      |

### Load the library

```R
library(wpm)
```

To see a complete Tutorial, please see the Vignette of the package. 
```R
browseVignettes("wpm")
```

### Using WPM from the command line

In command line there a some steps to process in the right order:

#### Prepare dataset

You can work with CSV files, or `ExpressionSet`, `MSnSet`, 
`SummarizedExperiment` objects.
The first step is to create a dataframe containing all the data needed by wpm 
to work properly. To do so, you need to specify which column in the CSV 
corresponds to the grouping factor if any. 
```R
# if you have a CSV file
df <- convertCSV("path-to-your-CSV", "grouping_factor")
# if you have an ExpressionSet or an MSnSet
df <- convertESet(myExpressionSet, "grouping_factor") # or convertESet(myMSnSet)
# if you have a SummarizedExperiment
df <- convertSE(mySummarizedExperiment, "grouping_factor")
```
 
#### Run WPM

The next step is to run the wpm wrapper function by giving it all the parameters
needed: the dataframe, the plate dimensions, the number of plates to fill, the 
forbidden wells (wells that must not be filled at all for the experiment), buffer
wells (wells where there will be solution without sample in it), fixed
wells, the spatial constraint to place the samples and the maximal number of 
attemps for WPM to find a valid solution.

```R
# example where we do not specify buffers
wpm_res <- wrapperWPM(user_df = df,
            plate_dims = list(8,12),
            nb_plates = 1,
            forbidden_wells = "A1,A2,A3",
            fixed_wells = "B1,B2",
            spatial_constraint = "NS")
```

#### Plate map visualization

The last step is to plot the plate plan(s) using the `drawMap()` function :

```R
drawned_map <- wpm::drawMap(df = wpm_result,
        sample_gps = length(levels(as.factor(pd$Environment))),
        gp_levels = gp_lvl <- levels(as.factor(pd$Environment)),
        plate_lines = 8,
        plate_cols = 12,
        project_title = "my Project Title")
        
drawned_map
```

### Using WPM in web interface

Since WPM provides also a GUI, the idea is to just provide a minimum of 
parameters to the application. No programming skills are required.
Simply run in the console:
```R
wpm()
```

WPM has 4 main panels:

* __Home__
* __Parameters__
* __Results__
* __Help__

#### Provide parameters

- **1)** Provide a CSV file containing the sample names and variable factors if any.

- **2)** Specify the plate dimensions and their number (the user can choose 
between 6, 24, 48, 96, 386, 1534 and custom). WPM checks that all the given 
settings  arecompatible)

- **3)** Specify the __Forbidden well__: These wells will not be filled with 
any kind of sample. We simply do not want to fill them (e.g. the coins of the 
plate), or in case of dirty wells, broken pipettes, etc.

- **4)** Specify the __Buffers__: correspond to solution without biological 
sample in it. Provide the neighborhood constraints, which depend on the "Buffer"
mode chosen. (Shouldn't samples from the same group be found side by side?)

- **5)** Specify the __Fixed samples__: correspond to Quality Control samples or standards.

- **6)** Choose a maximum number of iterations that WPM can do to find a 
solution,then start WPM. If the samples do not have a group, then the samples 
will be placedcompletely randomly on the plates. If there are groups, wpm will 
use an algorithminspired by the backtracking algorithm (in order to place the 
samples in the wellswhile respecting the specified constraints.).


#### Check your Results

This Panel allows you to look after the final dataset containing the wells 
chosen for each sample and a plot of your final well-plate map. Dataframe and 
plots are downloadable separately.

Example fo final dataset:
<p align="center"><img width=40% src="https://github.com/HelBor/wpm/blob/master/vignettes/images/final_dataset.PNG"></p>


Example of final plot for a 96 well-plate with 80 samples divided into 10 groups: 

<p align="center"><img width=40% src="https://github.com/HelBor/wpm/blob/master/vignettes/images/plot1.png"></p>



## Pending Features
* Manage multiple grouping factors when importing the data
* For proteomics, add the option to generate serialization of samples.

## Citing Our work
> The published article of the project will be linked here.
