% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make.design.R
\name{make.design}
\alias{make.design}
\title{Builds the design matrix}
\usage{
make.design(sTab)
}
\arguments{
\item{sTab}{The data.frame which correspond to the pData function of MSnbase}
}
\value{
A design matrix
}
\description{
This function builds the design matrix
}
\examples{
\dontrun{
utils::data(Exp1_R25_pept, package='DAPARdata')
make.design(Biobase::pData(Exp1_R25_pept))
}

}
\author{
Thomas Burger, Quentin Giai-Gianetto, Samuel Wieczorek
}
