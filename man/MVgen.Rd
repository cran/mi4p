% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MVgen.R
\name{MVgen}
\alias{MVgen}
\title{Amputation of a dataset}
\usage{
MVgen(dataset, prop_NA)
}
\arguments{
\item{dataset}{dataset to be amputed}

\item{prop_NA}{desired proportion of missing values in the amputed dataset}
}
\value{
A dataset with missing values.
}
\description{
This function is designed to ampute datasets.
}
\examples{
library(mi4p)
data(datasim)
datasim_amp <- MVgen(datasim, .2)
sum(is.na(datasim_amp))/prod(dim(datasim_amp))
}
\references{
M. Chion, Ch. Carapito and F. Bertrand (2021). \emph{Accounting for multiple imputation-induced variability for differential analysis in mass spectrometry-based label-free quantitative proteomics}. \doi{doi:10.1371/journal.pcbi.1010420}.
}
