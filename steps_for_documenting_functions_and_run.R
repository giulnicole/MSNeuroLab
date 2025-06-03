
# Configure description of functions according to roxygen2

# To automaticly write the man (of all edited functions) and ordering it for future
devtools::document()

roxygen2::roxygenise()

# Installing 
devtools::install()

devtools::load_all()


# Building site
pkgdown::build_site()