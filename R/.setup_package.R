### How to build an R package from scratch
remotes::install_github("kwb-r/kwb.pkgbuild")

usethis::create_package(".")
fs::file_delete(path = "DESCRIPTION")


author <- list(name = "Hauke Sonnenberg",
               orcid = "0000-0001-9134-2871",
               url = "https://github.com/hsonne")

pkg <- list(name = "kwb.ucode",
            title = "Interface to Auto-Calibration Software UCODE",
            desc  = paste("Functions enabling to write UCODE input files and to run UCODE."))


kwb.pkgbuild::use_pkg(author,
                      pkg,
                      version = "0.1.0",
                      stage = "experimental")


usethis::use_vignette("tutorial")

### R functions
if(FALSE) {
  ## add your dependencies (-> updates: DESCRIPTION)
  pkg_dependencies <- c('kwb.wtaq')
  
  sapply(pkg_dependencies, usethis::use_package)
  
  desc::desc_add_remotes("github::kwb-r/kwb.wtaq",normalize = TRUE)
}

kwb.pkgbuild::create_empty_branch_ghpages(pkg$name)
