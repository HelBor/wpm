<p align="center"><img width=40% src="https://github.com/HelBor/wpm/blob/master/inst/wpmApp/www/images/wpm_logo.png"></p>
<p align="center"><img width=70% src="https://github.com/HelBor/wpm/blob/master/inst/wpmApp/www/images/wpm_name.png"></p>


[![R](https://img.shields.io/badge/R-v3.4+-blue?style=flat-square)](https://img.shields.io/badge/R-v3.4+-blue?style=flat-square)
[![GitHub issues](https://img.shields.io/github/issues/HelBor/wpm?style=flat-square)](https://github.com/HelBor/wpm/issues)
[![GitHub license](https://img.shields.io/github/license/HelBor/wpm?style=flat-square)](https://github.com/HelBor/wpm)


## Brief introduction

> WPM is a shiny application deployed in the form of an R package.
> Its objective is to allow a user to generate a well plate plan in order to perform his experiments by controlling batch effects (in particular preventing plate edge effects).
> The algorithm for placing the samples is inspired by the backtracking algorithm.

## Getting started

### Pre-requisites
`R version > 3.4.1`

WPM R package depedencies:
* `shiny`
* `shinydashboard`
* `shinyWidgets`
* `shinycustomloader`
* `DT`
* `RColorBrewer`
* `data.table`
* `tidyverse`
* `logging`

### How to install

```R

```


### Launch WPM

#### in RStudio

```R
library(wmp)

```
#### in an R console
```R
library(wmp)
```


## How to use WPM

Since WPM is a GUI, the idea is to just provide a minimum of parameters to the application. No programming skills are required.

### WPM structure

WPM has 


### Provide parameters:
* provide a data set in a CSV format.
* specify a project name.
* specify the dimensions of the plate to be filled.
* specify whether there are blanks or not.
* specify spatial neighboring constraints for the positioning of the samples (ie prevent 2 samples belonging to the same group from finding themselves side by side).
* specify if there are "forbidden" boxes, that is to say leave them blank. (to put control samples for example)
* specify the number of attempts that WPM can make to find a possible configuration of the plate plan.


### Check your Results


### Export your results

## Pending Features

* Support for multiple plates and place samples in a balanced way.