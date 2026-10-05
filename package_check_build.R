require(devtools)
require(tools)

# Package test, check, build & publish to CRAN
options(encoding = "UTF-8")

# Specify the exact path to pdflatex for R (for manual building)
pdflatex_path <- Sys.which("pdflatex")
# If Sys.which("pdflatex") returns "" (empty),
# then use Sys.setenv to add the path to your pdflatex installation
# Example for windows:
#Sys.setenv(PATH = paste("C:/texlive/2025/bin/windows", Sys.getenv("PATH"), sep = ";"))

# Set the texi2dvi command to use pdflatex
options(texi2dvi_cmd = pdflatex_path)
# Set the PDFLATEX environment variable
Sys.setenv(PDFLATEX = pdflatex_path)

package_name <- "surveyplanning"

description <- readLines(paste0(package_name, "/DESCRIPTION"))
ver <- gsub(" |:|[A-z]", "", grep("Version", description, value = T))
ver


# Documentation
devtools::document(package_name, roclets = c("rd", "collate", "namespace"))

# Spell Check
#devtools::spell_check(package_name)


# Check localy with devtools
# Light check
devtools::check(package_name, cran = FALSE)
# Extended check (testing all examples)
devtools::check(package_name, manual = TRUE, cran = TRUE, remote = TRUE,
                run_dont_test = TRUE, args = "--run-dontrun")

# Build source package
devtools::build(package_name)

# Build binary package
devtools::build(package_name, binary = TRUE, args = c('--preclean'))

# Copy manual
# file.copy(from = paste0(package_name, ".Rcheck/", package_name, "-manual.pdf"),
#             to = paste0(package_name, "_", ver, "-manual.pdf"), overwrite = TRUE)
devtools::check_man(package_name)
devtools::build_manual(package_name)

# MD5
md5sums <- md5sum(list.files(pattern = "zip$|tar.gz$|pdf$"))

df <- data.frame(md5 = md5sums, filename = names(md5sums))
data.table::fwrite(df, file = paste0(package_name, "_", ver, "_checksums.md5"),
                                          sep = " ", row.names = F, col.names = F, quote = F)

# Install and load
detach("package:surveyplanning", unload = TRUE)



# Install source package
install.packages(paste0(package_name, "_", ver, ".tar.gz"), repos = NULL)

# Install Windows binary package
if (.Platform$OS.type == "windows") {
  install.packages(paste0(package_name, "_", ver, ".zip"), repos = NULL)
}

# Load package
library(package_name, character.only = TRUE)



# Remote checks
# Use only if local check are OK!!!

# Building and checking R source packages for Windows
# https://win-builder.r-project.org/
# devtools::check_win_oldrelease(package_name) # R previous release
devtools::check_win_release(package_name)    # R current release
devtools::check_win_devel(package_name)      # R devel version

# R-hub builder
# https://builder.r-hub.io/
#devtools::check_rhub is deprecated (This function is deprecated since
# the underlying function rhub::check_for_cran() is now deprecated and defunct.
# See rhub::rhubv2 learn about the new check system, R-hub v2.) (Experimental)
#devtools::check_rhub(package_name, email = "...")
#rhub::rhub_check()

# Publish to CRAN
# https://cran.r-project.org/web/packages/policies.html
# Do this only of all tests and checks are OK
devtools::release(package_name)


