# wpm changes in version 0.99.2

* Added a `NEWS.md` file to track changes to the package.
* Modified code and files structure of the package
* Added Roxygen comments to functions

# changes in version 0.99.3 (2020-05-28)

* All the R code is now in the /R directory.
* Modified R code structure by creating additional modules:
    * mod_home, fusion between the Home panel and the Help Panel
    * mod_data_export, a specific module for data export
    * mod_plate_dimensions
    * mod_special_wells, mainly to avoid code redundancy and gain readability
* added unit test for the convertVector2Df function
* added Rd examples for convertVector2Df and drawMap functions
