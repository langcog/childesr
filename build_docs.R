install.packages("devtools", repos = "http://cran.rstudio.com/")
devtools::install_github("r-lib/pkgdown")
pkgdown::build_site()