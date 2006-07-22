RcppVersion <- function() {
  licenseFile <- file(system.file(".","Rcpp-license.txt",package="RQuantLib"),"r")
  writeLines(readLines(licenseFile))
}
