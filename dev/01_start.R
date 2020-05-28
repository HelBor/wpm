golem::fill_desc(
  pkg_name = "wpm3",
  pkg_title = "Well Plate Maker",
  pkg_description = "This is a shiny application for creating well-plate plans. 
  It uses a backtracking-inspired algorithm to place samples on plates based on specfic neighborhood constraints.",
  author_first_name = "Hélène",
  author_last_name = "Borges",
  author_email = "borges.helene.sophie@gmail.com",
  repo_url = NULL
)     
golem::set_golem_options()
usethis::use_mit_license( name = "Golem User" )
usethis::use_readme_rmd( open = FALSE )
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" )
usethis::use_news_md( open = FALSE )
usethis::use_git()
golem::use_recommended_tests()
golem::use_recommended_deps()
golem::remove_favicon()
golem::use_favicon("F:/PhD/R/wpm/inst/wpmApp/www/favicon.ico")
golem::use_utils_ui()
golem::use_utils_server()
rstudioapi::navigateToFile( "dev/02_dev.R" )
