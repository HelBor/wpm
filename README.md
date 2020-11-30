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

The **W**ell-**P**late **M**aker (WPM) is a shiny application deployed as an R package. Functions for
a command-line/script use are also available. The WPM allows users to 
generate well plate maps to carry out their experiments while improving
the handling of batch effects. In particular, it helps controlling the "plate 
effect" thanks to its ability to randomize samples over multiple well plates.
The algorithm for placing the samples is inspired by the backtracking algorithm: 
the samples are placed at random while respecting specific spatial constraints. 
The use of WPM as well as the definition of configurable spatial constraints
are described in the following sections.

## Getting started

### Pre-requisites
`R version >= 4.0.0`
OS tested : `Windows`, `Fedora`, `Ubuntu`,`MacOS`
The application should also work on other platforms.
If problems are encountered on other OS, report them by 
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


## How to use the WPM package

There are two ways to use the WPM:

* Command line with appropriate R functions: for users who want to work with 
scripts or want to integrate the WPM into a pre-existing pipeline.
* through a graphical interface (GUI): for users who do not necessarily have advanced
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
pipettes). On the resulting map, these wells will be colored in red.
* **Buffers** filled with solution but do not biological material (e.g. to 
avoid/check for cross-contamination). On the resulting map, these wells will be colored in grey.
* **Fixed samples** for quality control samples or standards, the precise 
location of these samples must be controlled by the researcher. On the resulting map, these wells 
will be colored in black.
* **Randomized samples** split into groups according to their biological 
content, and which will be randomized within and between plates. On the resulting map, these wells 
will be colored according to the group to which the sample belongs.

This priority rule allows consistent distributions of samples into wells.
For example, if a well is declared *forbidden*, then this well will no longer be considered for 
the other types of samples.

### Load the WPM package

```R
library(wpm)
```

For a complete Tutorial, please see the Vignette of the package.
```R
browseVignettes("wpm")
```

### Using the WPM with the command lines

The following steps must be performed in the correct order.

#### Prepare the dataset

You can work with CSV/txt/TSV files, *ExpressionSet*, *MSnSet*, or 
*SummarizedExperiment* objects.
The first step is to create a dataframe containing all the data necessary for the WPM 
to work correctly. Notably, it is needed to specify which column in the file 
corresponds to the grouping factor, if any.
```R
# if you have a CSV file
df <- convertCSV("path-to-your-CSV", "grouping_factor")
# if you have an ExpressionSet or an MSnSet
df <- convertESet(myExpressionSet, "grouping_factor") # or convertESet(myMSnSet, "grouping_factor")
# if you have a SummarizedExperiment
df <- convertSE(mySummarizedExperiment, "grouping_factor")
```
For more details about the functions, please use `?wpm::<functionName>` R command.
 
#### Run the WPM

The next step is to run the `wrapperWPM` function by giving it all the parameters
needed:

* the dataframe generated with `convertXXX` functions
* the plate dimensions
* the number of plates to fill
* the forbidden wells (wells that must not be filled at all for the experiment)
* buffer wells (wells where there will be solution without biological material)
* The position of fixed samples
* the spatial constraint to place the samples
* the maximal number of attemps for the WPM to find a valid solution

Suppose you have generated this (toy) dataframe:

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

# convert it into a dataframe that is valid according to the WPM constraints
df <- convertESet(x, "Environment")
```


```R
# example without buffer specification
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


### Using the WPM through a web interface

The WPM is accompanied with a GUI providing the necessary parameters for the 
application to work. No programming skills are required.
Simply run in the console:
```R
wpm()
```
If everything is in order, a new window will open in your default browser.
If not, find the line written in the R console that looks like
`Listening on http://127.0.0.1:8000`, and paste the URL in your web browser.

WPM has 4 main panels:

* __Home__
* __Parameters__
* __Results__
* __Help__

#### Parameters setting

- **1)** Provide a CSV **file** containing the sample names and variable factors, if any.

- **2)** Provide a **Project title**. It will be used for the plot(s) title and the 
identifiers in the [final dataframe](#final_dataframe).

- **3)** Specify the **plate dimensions** and their **number** (the user can choose 
between 6, 24, 48, 96, 386, 1534 as well as custom dimensions). The WPM checks that all the given 
settings  are compatible)

- **4)** Specify the **Forbidden well**: simply insert in *LetterNumber* format separated with comma (e.g. *"A1,A2"*)

- **5)** Specify the **Buffers**: You need to specify the pattern (rows, columns, checkerboard or manually defined) and give the 
neighborhood constraint between groups for randomization:
    - NS (North South): samples from the same group will not be placed side by 
    side column-wise. 
    <p align="center"><img src="https://github.com/HelBor/wpm/blob/master/vignettes/images/NCns.PNG"></p>
    - WE (West East): samples from the same group will not be placed side by 
    side row-wise.
    <p align="center"><img src="https://github.com/HelBor/wpm/blob/master/vignettes/images/NCew.PNG"></p>
    - NSEW (North South East West): samples from the same group will not be 
    placed side by side either row-wise or colmun-wise. 
    <p align="center"><img src="https://github.com/HelBor/wpm/blob/master/vignettes/images/NCnsew.PNG"></p>
    - None: samples from the same group can be placed anywhere, including side by side.
    <p align="center"><img src="https://github.com/HelBor/wpm/blob/master/vignettes/images/NCnone.PNG"></p>

- **6)** Specify the **Fixed samples**: in the same way as for forbidden wells,
insert *LetterNumber* as is *"A1,B3,C10,A5"*.

- **7)** Choose a **maximum number of iterations** to find a 
solution, then start the WPM. If the samples do not have a group, then the samples 
will be placed completely randomly on the plates. If there are groups, the WPM will 
use an algorithm inspired by the backtracking algorithm (to place the 
samples in the wells while respecting the specified constraints).


#### Check the results

The Result panel allows you to look at the final dataset containing the wells 
chosen for each sample and a plot of your final well-plate map. Dataframe and 
plots are downloadable separately.

Example of a final <a name="final_dataframe"></a> dataset:
<p align="center"><img src="https://github.com/HelBor/wpm/blob/master/vignettes/images/final_dataset.PNG"></p>


Example of a final plot (96 well-plate with 80 samples divided into 10 groups): 

<p align="center"><img src="https://github.com/HelBor/wpm/blob/master/vignettes/images/plot1.png"></p>



## Pending Features
* Manage multiple grouping factors when importing the data
* For proteomics, add the option to serialize the samples into the LC-MS pipeline.

## Citing Our work
> The published article of the project will be linked here.
