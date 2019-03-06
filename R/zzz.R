.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0("Version: ", utils::packageDescription('bio.shrimp')$Version))
}
.onLoad <- function(libname, pkgname){
  options(stringsAsFactors = FALSE)
}
