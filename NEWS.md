# wpm changes in version 0.99.2

* Added a `NEWS.md` file to track changes to the package
* Modified code and files structure of the package
* Added Roxygen comments to functions

# changes in version 0.99.3 (2020-05-28)

* All the R code is now in the /R directory.
* Modified R code structure by creating additional modules:
    * mod_home, fusion between the Home panel and the Help Panel
    * mod_data_export, a specific module for data export
    * mod_plate_dimensions
    * mod_special_wells, mainly to avoid code redundancy and gain readability
* Added unit test for the convertVector2Df function
* Added Rd examples for convertVector2Df and drawMap functions


# changes in version 0.99.4 (2020-05-28)
* Fix the TIMEOUT error during R CMD CHECK

# changes in version 0.99.5 (2020-06-02)
* Added functions for importing CSV files and ExpressionSet / MSnSet objects
* Added the wrapper function allowing to use wpm in command line
* Added support for eSet and MSnSet objects as input of the wrapper function
* Added unit tests for import functions


# changes in version 0.99.6 (2020-06-02)
* Added function checkWpmInputs to control the correct use of the wrapperWpm
function (command line use).
* Updated the README file and the tutorial vignette explaining how to use WPM
using command line.

# changes in version 0.99.7 (2020-06-05)
* Added the convertSE function to manage SummarizedExperiments objects in
command line version
* Added the grouping factor option for data import (managed for both shiny app
and command line). Now the user can specify the column name corresponding to the
grouping factor wpm has to use for backtracking.
* Modified app_ui structure regarding the project title input
* Updated the tutorial vignette's content
* Updated README file
* Revised unit tests for import functions
* The WPM package version output in Home panel is now obtained with
packageVersion()
* Created a new module for the integration of markdown files in the shiny
application.

# changes in version 0.99.8 (2020-06-11)
* Modified structure of the Home tab in the shiny application
* Added new module for the help tab.
* Created the help.md file for the Help tab of the app.
* Added CSS for tables, and images.
* Changed the "blank" word to "buffer" throughout the package.
* Changed the "not random" term to "fixed" throughout the package.
* Updated images and text in the tutorial vignette.

# changes in version 0.99.9 (2020-06-18)
* Resolved crashes regarding bad settings when importing CSV
* Parameter sections are now collapsible
* created module for number of iterations
* defined the launch for the browser on port 8000
* changed the order of CSV import section in the parameters Panel
* some upload parameters are now available only if the CSV file is correctly 
imported
* added preview of CSV file when importing in the UI

# changes in version 0.99.10
* corrected unit tests for CSV import
* corrected README R commands for convertCSV section
* corrected Help tab for the upload file section
* corrected the Parameters section in the Vignette

# changes in version 0.99.11
* fixed issue #10
* fixed issue #22
* changed the display of the datatable in the Results tab
* updated Vignette and Help page about toy dataset (CSV section)

# changes in version 0.99.12
* fixed the lack of display of images in the help tab
* fixed inconsistencies regarding the setting of quotes when importing data
* added an infobox for plate dimensions compatibility with the number of samples

# changes in version 0.99.13
* Added a zzz.R file for the welcome message when loading the package in an R 
session
* Corrections in package vignette and Help tab of the shiny app
* Revised all documentation of functions

# changes in version 0.99.14
* corrections for Biocondutor release
