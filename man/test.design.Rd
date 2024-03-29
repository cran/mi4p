% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test.design.R
\name{test.design}
\alias{test.design}
\title{Check if xxxxxx}
\usage{
test.design(tab)
}
\arguments{
\item{tab}{A data.frame which correspond to xxxxxx}
}
\value{
A list of two items
}
\description{
This function check xxxxx
}
\examples{
\dontrun{
utils::data(Exp1_R25_pept, package='DAPARdata')
test.design(Biobase::pData(Exp1_R25_pept)[,1:3])
}

}
\author{
Thomas Burger, Quentin Giai-Gianetto, Samuel Wieczorek originally in 
the \code{DAPAR} package. Included in this package since \code{DAPAR} was to be removed from 
Bioconductor >= 3.15.
}
