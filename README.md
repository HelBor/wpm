<p align="center"><img width=40% src="https://github.com/HelBor/wpm/blob/master/inst/app/www/images/wpm_logo.png"></p>
<p align="center"><img width=70% src="https://github.com/HelBor/wpm/blob/master/inst/app/www/images/wpm_name.png"></p>


![Project Status](https://img.shields.io/badge/status-active-success?style=flat-square)
![R](https://img.shields.io/badge/R-v4.0+-informational?style=flat-square)
[![GitHub issues](https://img.shields.io/github/issues/HelBor/wpm?style=flat-square)](https://github.com/HelBor/wpm/issues)
![GitHub license](https://img.shields.io/badge/license-Artistic--2.0-important?style=flat-square)

**Bioconductor informations**

![platforms](https://bioconductor.org/shields/availability/3.12/wpm.svg)
[![years in bioc](http://bioconductor.org/shields/years-in-bioc/wpm.svg)](https://bioconductor.org/packages/release/bioc/html/wpm.html)    
**Release** ![build release](http://bioconductor.org/shields/build/release/bioc/wpm.svg)    
**Devel** ![build devel](http://bioconductor.org/shields/build/devel/bioc/wpm.svg)

## Brief introduction

WPM is a shiny application deployed as an R package. Functions for
a command-line/script use are also available. WPM aims to allow users to 
generate well plate plans in order to carry out their experiments while 
controlling certain batch effects. In particular, it makes it possible to control the "plate 
effect" thanks to its ability to manage multiple well plates.
The algorithm for placing the samples is inspired by the backtracking algorithm.
Thus, the samples will be placed on the plates at random while respecting 
precise spatial constraints. The use of WPM as well as the definition of 
configurable spatial constraints are described in the following sections.

## Getting started

### Pre-requisites
`R version >= 4.0.0`
OS tested : `Windows`, `Fedora`, `Ubuntu`,`MacOS`
The application should also work on other platforms.
If problems are encountered on other OS, do not hesitate to report them by 
creating an [issue](https://github.com/HelBor/wpm/issues).

**WPM R package dependencies**

CRAN dependencies: `golem`, `rlang`, `shiny`, `shinydashboard`, `shinyWidgets`, `dplyr`,
`shinycustomloader`, `DT`, `RColorBrewer`, `logging`, `stringr`, `ggplot2`

Bioconductor dependencies: `Biobase`, `SummarizedExperiment`

### How to install

From GitHub (consider it a devel version):
```R
devtools::install_github("HelBor/wpm", build_vignettes=TRUE)
```

From Bioconductor (release, stable version):
```R
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("wpm")
```
Instructions can also be found on the 
[Bioconductor page](http://bioconductor.org/packages/release/bioc/html/wpm.html)


## How to use WPM

There are two ways to use WPM:

* Command line with appropriate R functions: for users who want to work with 
scripts or want to integrate wpm into a pre-existing pipeline.
* through a graphical interface: for users who do not necessarily have advanced
R programming skills.

### Supported input formats

| Input Format          | Command line | WPM app |
| --------------------- |:------------:| :------:|
| CSV / txt             | yes          | yes     |
| ExpressionSet         | yes          | no      |
| SummarizedExperiment  | yes          | no      |
| MSnSet                | yes          | no      |


### Types of samples

WPM identifies four different types of samples with decreasing priority: 

* **Forbidden wells** that should not be filled with any kind of sample, either 
because the user does not want to (e.g. plate corners in case of non-uniform
heat distribution), or because of material constraints (e.g. dirty wells, broken
pipettes). These wells will be colored in red.
* **Buffers** filled with solution but without biological material (e.g. to 
avoid/check for cross-contamination). These wells will be colored in grey.
* **Fixed samples** for quality control samples or standards, the precise 
location of these samples must be controlled by the researcher. These wells 
will be colored in black.
* **Randomized samples** split into groups according to their biological 
content, and which will be randomized within and between plates. These wells 
will be colored according to the group to which the sample belongs.

This priority rule allows consistent coloring of the wells. For example, if a 
well is declared *forbidden*, then this well will no longer be considered for 
the other types of samples: by definition when a well is prohibited, it means 
that nothing else should be put in the well concerned and it will always be 
colored red.

### Load the library

```R
library(wpm)
```

To see a complete Tutorial, please see the Vignette of the package.
```R
browseVignettes("wpm")
```

### Using WPM from the command line

In command line, there are few steps to be performed in the correct order:

#### Prepare the dataset

You can work with CSV/txt/TSV files, *ExpressionSet*, *MSnSet*, or 
*SummarizedExperiment* objects.
The first step is to create a dataframe containing all the data necessary for wpm 
to work properly. To do so, you need to specify which column in the file 
corresponds to the grouping factor if any. 
```R
# if you have a CSV file
df <- convertCSV("path-to-your-CSV", "grouping_factor")
# if you have an ExpressionSet or an MSnSet
df <- convertESet(myExpressionSet, "grouping_factor") # or convertESet(myMSnSet, "grouping_factor")
# if you have a SummarizedExperiment
df <- convertSE(mySummarizedExperiment, "grouping_factor")
```
For more details about the functions, please use `?wpm::<functionName>` R command.
 
#### Run WPM

The next step is to run the `wrapperWPM` function by giving it all the parameters
needed:

* the dataframe generated with `convertXXX` functions
* the plate dimensions
* the number of plates to fill
* the forbidden wells (wells that must not be filled at all for the experiment),
* buffer wells (wells where there will be solution without sample in it)
* The position of fixed samples.
* the spatial constraint to place the samples
* the maximal number of attemps for WPM to find a valid solution.

Suppose you have generated this toy dataframe:

```R
# create a MSnSet toy example
sample_names <- c("s1","s2","s3","s4", "s5")
M <- matrix(NA, nrow = 4, ncol = 5)
colnames(M) <- sample_names
rownames(M) <- paste0("id", LETTERS[1:4])
pd <- data.frame(Environment = rep_len(LETTERS[1:3], 5),
                 Category = rep_len(1:2, 5), row.names = sample_names)
rownames(pd) <- colnames(M)
x <- MSnbase::MSnSet(exprs = M,pData =  pd)

# convert it to a valid dataframe for wpm
df <- convertESet(x, "Environment")
```


```R
# example where we do not specify buffers
wpm_res <- wrapperWPM(user_df = df,
            plate_dims = list(8,12),
            nb_plates = 1,
            forbidden_wells = "A1,A2,A3",
            fixed_wells = "B1,B2",
            spatial_constraint = "NS")
```

For more details, see `?wpm::wrapperWPM`


#### Plate map visualization

The final step is to create a visual output of the generated plate plan(s) 
using the `drawMap()` function :

```R
drawned_map <- wpm::drawMap(df = wpm_res,
        sample_gps = length(levels(as.factor(pd$Environment))),
        gp_levels = gp_lvl <- levels(as.factor(pd$Environment)),
        plate_lines = 8,
        plate_cols = 12,
        project_title = "my Project Title")
        
drawned_map
```

For more details, see `?wpm::drawMap`


### Using WPM through a web interface

WPM provides also a graphical interface, the idea is to just provide a minimum of 
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

- **1)** Provide a CSV **file** containing the sample names and variable factors if any.

- **2)** Provide a **Project title**. It will be used for the plot(s) title and the 
identifiers in the [final dataframe](#final_dataframe).

- **3)** Specify the **plate dimensions** and their **number** (the user can choose 
between 6, 24, 48, 96, 386, 1534 and custom). WPM checks that all the given 
settings  are compatible)

- **4)** Specify the **Forbidden well**: simply insert in *LetterNumber* format separted with comma (e.g. *"A1,A2"*)

- **5)** Specify the **Buffers**: You need to specify the pattern (in line 
shape, in column shape, in checkerboard shape or filled by hand) and give the 
neighborhood constraint to let know WPM how to place randomized samples 
according to their group membership:
    - NS (North South): samples from the same group will not be placed side by 
    side in North and South positions. 
    <p align="center"><img width=40% src="https://github.com/HelBor/wpm/blob/master/vignettes/images/NCns.PNG.PNG"></p>
    - WE (West East): samples from the same group will not be placed side by 
    side in West and East positions.
    <p align="center"><img width=40% src="https://github.com/HelBor/wpm/blob/master/vignettes/images/NCew.PNG"></p>
    - NSEW (North South East West): samples from the same group wil not be 
    placed side by side in N, S, W and E positions. 
    <p align="center"><img width=40% src="https://github.com/HelBor/wpm/blob/master/vignettes/images/NCnsew.PNG"></p>
    - None: samples from the same group can be placed side by side.
    <p align="center"><img width=40% src="https://github.com/HelBor/wpm/blob/master/vignettes/images/NCnone.PNG"></p>

- **6)** Specify the **Fixed samples**: in the same way as for forbidden wells,
insert LetterNumber as is *"A1,B3,C10,A5"*.

- **7)** Choose a **maximum number of iterations** that WPM can do to find a 
solution,then start WPM. If the samples do not have a group, then the samples 
will be placed completely randomly on the plates. If there are groups, wpm will 
use an algorithm inspired by the backtracking algorithm (in order to place the 
samples in the wells while respecting the specified constraints.).


#### Check your Results

This Panel allows you to look after the final dataset containing the wells 
chosen for each sample and a plot of your final well-plate map. Dataframe and 
plots are downloadable separately.

Example fo final <a name="final_dataframe"></a> dataset:
<p align="center"><img width=60% src="https://github.com/HelBor/wpm/blob/master/vignettes/images/final_dataset.PNG"></p>


Example of final plot for a 96 well-plate with 80 samples divided into 10 groups: 

<p align="center"><img width=70% src="https://github.com/HelBor/wpm/blob/master/vignettes/images/plot1.png"></p>



## Pending Features
* Manage multiple grouping factors when importing the data
* For proteomics, add the option to generate serialization of samples.

## Citing Our work
> The published article of the project will be linked here.
