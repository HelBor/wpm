<p align="center"><img width=40% src="https://github.com/HelBor/wpm/blob/master/inst/wpmApp/www/images/wpm_logo.png"></p>
<p align="center"><img width=70% src="https://github.com/HelBor/wpm/blob/master/inst/wpmApp/www/images/wpm_name.png"></p>


![R](https://img.shields.io/badge/R-v3.4+-blue?style=flat-square)
[![GitHub issues](https://img.shields.io/github/issues/HelBor/wpm?style=flat-square)](https://github.com/HelBor/wpm/issues)
![Release](https://img.shields.io/badge/release-alpha-orange?style=flat-square)
![GitHub license](https://img.shields.io/github/license/HelBor/wpm?style=flat-square)


## Brief introduction

> WPM is a shiny application deployed in the form of an R package.
> Its objective is to allow a user to generate a well plate plan in order to perform his experiments by controlling batch effects (in particular preventing plate edge effects).
> The algorithm for placing the samples is inspired by the backtracking algorithm.

## Table of content
* [Getting started](https://github.com/HelBor/wpm#getting-started)

* [How to use WPM](https://github.com/HelBor/wpm#how-to-use-wpm)
  - [WPM structure](https://github.com/HelBor/wpm#wpm-structure)
  - [Provide parameters](https://github.com/HelBor/wpm#provide-parameters)
  - [Check your Results](https://github.com/HelBor/wpm#check-your-results)
  - [Export your results](https://github.com/HelBor/wpm#export-your-results)

* [Pending Features](https://github.com/HelBor/wpm#pending-features)




## Getting started

### Pre-requisites
`R version > 3.4.1`
OS platforms: `Windows 10, Fedora 24+, Ubuntu`

WPM R package depedencies:

`shiny`, `shinydashboard`, `shinyWidgets`, `shinycustomloader`, `DT`, 
`RColorBrewer`, `data.table`, `tidyverse`, `logging`

### How to install


From GitHub
```R
The project has not yet been built as a package 
```


### Launch WPM

#### in RStudio

```R


```
#### in an R console
```R

```


## How to use WPM

Since WPM is a GUI, the idea is to just provide a minimum of parameters to the application. No programming skills are required.

### WPM structure

WPM has 4 main panels:
* __Home__: explains you how to use WPM
* __Parameters__: the Panel where you fill the parameters needed to run WPM
* __Results__: the Panel where you can look after the final dataset containing the wells chosen for each sample and a plot of your final well-plate map.
* __Export__: the Panel to export your dataset in CSV and your map in PNG format.


### Provide parameters:
* provide a data set in a CSV format.
* specify a project name.
* specify the dimensions of the plate to be filled.
* specify whether there are blanks or not. (present the different possibilities)
* specify spatial neighboring constraints for the positioning of the samples (ie prevent 2 samples belonging to the same group from finding themselves side by side). (present the different possibilities))
* specify if there are "forbidden" boxes, that is to say leave them blank. (to put control samples for example) (put a capture for example)
* specify the number of attempts that WPM can make to find a possible configuration of the plate plan.


### Check your Results

add a capture of dataframe and map plot

### Export your results

add capture of export panel

## Pending Features

* Support for multiple plates and place samples in a balanced way.
* For proteomics, add the option to generate serialization of samples.