% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sticsy.R
\name{sticsy}
\alias{sticsy}
\title{Statistical total correlation spectroscopy with new dependence measures (distance correlation, distance covariance, MIC, gMIC, TIC)}
\usage{
sticsy(dataset, ppm, ppm_edges, method = "TIC",
  visualization_method = "median")
}
\arguments{
\item{dataset}{Matrix of spectra x bins.}

\item{ppm}{ppm.}

\item{ppm_edges}{Left and right edges (in ppm) of the spectrum region to analyze.}

\item{method}{Dependence measure to use for correlation spectroscopy. Available options are 'pearson', 'spearman', 'dcor' (distance correlation), 'dcov' (distance covariance), 'MIC' (Maximal Information Coefficient), 'GMIC' (Generalized Maximal Information Coefficient) and 'TIC' (Total Information Coefficient).  By default TIC.}

\item{visualization_method}{How to visualize results in spectra dataset analyzed. 'median' shows the median spectrum, a numeric figure shows a random n number of spectra (it is recommended not to plot more than 10). By default 'median'.}
}
\value{
Vector with results of correlation spectroscopy and Plotly interactive figure for visualization of results.
}
\description{
Statistical total correlation spectroscopy with new dependence measures (distance correlation, distance covariance, MIC, gMIC, TIC)
}
\examples{
data=MTBLS1()
results=sticsy(data$dataset,data$ppm,c(7.58,7.54),method='TIC',visualization_method='median')
}
