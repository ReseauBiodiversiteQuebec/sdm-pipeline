# usethis::create_package("sdmpipeline")
# usethis::use_mit_license()

usethis::use_dev_package("usethis", type = "Suggests", remote = NULL)
usethis::use_dev_package("testthat", type = "Suggests", remote = NULL)
usethis::use_dev_package("devtools", type = "Imports", remote = NULL)
usethis::use_dev_package("pkgdown", type = "Suggests", remote = NULL)
usethis::use_dev_package("rmarkdown", type = "Suggests", remote = NULL)

# Use packages for development in vscode environment
usethis::use_dev_package("languageserver", type = "Suggests", remote = NULL)
usethis::use_dev_package("vscDebugger", type = "Suggests",
    remote = "github::ManuelHentschel/vscDebugger")

# Use packages for development using Jupyter
usethis::use_dev_package("IRkernel", type = "Suggests", remote = NULL)