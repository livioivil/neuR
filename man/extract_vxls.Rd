% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_vxls.R
\name{extract_vxls}
\alias{extract_vxls}
\alias{extract_tc}
\title{extract_vxls extract_tc}
\usage{
extract_vxls(xyz, D)
}
\arguments{
\item{xyz}{three type of inputs are allowed: 
a) nX3 matrix of n coordinates, 
b) vector of T/F on length equal to the number of voxels,
c) vector of indices of the voxels to keep.}

\item{D}{a neuR-object}

\item{tcs}{any proper array e.g. from D@data$tcs}

\item{mask}{usually D@mask}
}
\value{
a neuR-object
}
\description{
select voxels (or time courses of voxels) 
from a matrix of coordinates
}
