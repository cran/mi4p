% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.design.R
\name{check.design}
\alias{check.design}
\title{Check if the design is valid}
\usage{
check.design(sTab)
}
\arguments{
\item{sTab}{The data.frame which correspond to the pData function of MSnbase}
}
\value{
A boolean
}
\description{
This function checks the validity of the experimental design.

This function was included from the \code{check.design} 
function in the \code{DAPAR} package, since \code{DAPAR} was to be removed from 
Bioconductor >= 3.15.
}
\examples{
\dontrun{
utils::data(Exp1_R25_pept, package='DAPARdata')
check.design(Biobase::pData(Exp1_R25_pept)[,1:3])
}

}
\author{
Thomas Burger, Quentin Giai-Gianetto, Samuel Wieczorek as 
the authors of \code{check.design} in the \code{DAPAR} package.
}
