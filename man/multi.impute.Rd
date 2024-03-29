% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi_impute.R
\name{multi.impute}
\alias{multi.impute}
\title{Multiple imputation of quantitative proteomics datasets}
\usage{
multi.impute(data, conditions, nb.imp = NULL, method, parallel = FALSE)
}
\arguments{
\item{data}{A quantitative matrix to be imputed, with proteins/peptides in
rows and samples in columns.}

\item{conditions}{A vector of length the number of samples where each element
corresponds to the condition the sample belongs to.}

\item{nb.imp}{The number of imputation to perform.}

\item{method}{A single character string describing the imputation method to 
be used. See details.}

\item{parallel}{Logical, whether or not use parallel computing
(with \code{\link[foreach]{foreach}}).}
}
\value{
A numeric array of dimension c(dim(data),nb.imp).
}
\description{
\code{\link{multi.impute}} performs multiple imputation on a
given quantitative proteomics dataset.
}
\details{
Multiple imputation consists in imputing several times a given
dataset using a given method. Here, imputation methods can be chosen either 
from \code{\link[mice]{mice}}, \code{\link[imp4p]{imp4p-package}} or 
\code{\link[impute]{impute.knn}}:
\itemize{
    \item "pmm", "midastouch", "sample", "cart", "rf","mean", "norm", 
    "norm.nob", "norm.boot", "norm.predict": imputation methods as described 
    in \code{\link[mice]{mice}}.
    \item "RF" imputes missing values using random forests algorithm as 
    described in \code{\link[imp4p]{impute.RF}}.
    \item "MLE" imputes missing values using maximum likelihood estimation
    as described in \code{\link[imp4p]{impute.mle}}.
    \item "PCA" imputes missing values using principal component analysis as 
    described in \code{\link[imp4p]{impute.PCA}}.
    \item "SLSA" imputes missing values using structured least squares 
    algorithm as described in \code{\link[imp4p]{impute.slsa}}.
    \item "kNN" imputes missing values using k nearest neighbors as 
    described in \code{\link[impute]{impute.knn}}.
}
}
\examples{
library(mi4p)
data(datasim)
multi.impute(data = datasim[,-1], conditions = attr(datasim,"metadata")$Condition, method = "MLE")
}
\references{
M. Chion, Ch. Carapito and F. Bertrand (2021). \emph{Accounting for multiple 
imputation-induced variability for differential analysis in mass 
spectrometry-based label-free quantitative proteomics}. arxiv:2108.07086. 
\url{https://arxiv.org/abs/2108.07086}.
}
