% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reshapeTcs2blocks.R
\name{reshapeTcs2blocks}
\alias{reshapeTcs2blocks}
\title{Reshapes D@data$tcs in a 3D (time X voxel/channel X block)}
\usage{
reshapeTcs2blocks(D, blocks)
}
\arguments{
\item{D}{a neuR-ogject}

\item{blocks}{if scalar, it is the number of volumes in each blocks}
}
\value{
a neuR-object
}
\description{
Reshapes D@data$tcs in a 3D (time X voxel/channel X block)
}
