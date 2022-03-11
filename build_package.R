# Load, test and build package and documentation
devtools::load_all()
# devtools::test()
devtools::document()
devtools::install()
# rmarkdown::render("./vignettes/pipeline_example.Rmd")
# pkgdown::build_site(examples = FALSE)