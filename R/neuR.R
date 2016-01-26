#' neuR: R package for neuroscience data processing and statistical analysis of neuro-data
#'
#' The neuR package provides functions to deal with fMRI, fNIRS, EEG data.
#' 
#' @section neuR functions:
#'
#' @docType package
#' @name neuR
#' @import parallel
NULL
options(mc.cores=min(parallel::detectCores()-1,30))
#> NULL
