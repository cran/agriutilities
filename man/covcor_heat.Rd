% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fa.R
\name{covcor_heat}
\alias{covcor_heat}
\title{Correlation Covariance Heatmap}
\usage{
covcor_heat(matrix, corr = TRUE, size = 4, digits = 3)
}
\arguments{
\item{matrix}{A numeric matrix.}

\item{corr}{A logical value indicating if the matrix is in a scaled form
(\code{TRUE} by default, correlation matrix)}

\item{size}{A numeric value to define the letter size.}

\item{digits}{A numeric integer to define the number of digits to plot.}
}
\value{
A ggplot object showing the upper triangular elements of the matrix.
}
\description{
Correlation Covariance Heatmap
}
\examples{
library(agriutilities)
data(iris)
M <- cor(iris[, -5])
covcor_heat(matrix = M, corr = TRUE)
}
