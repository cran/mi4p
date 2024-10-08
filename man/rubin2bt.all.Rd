% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rubin2bt_all.R
\name{rubin2bt.all}
\alias{rubin2bt.all}
\title{2nd Rubin's rule Between-Imputation component (all peptides)}
\usage{
rubin2bt.all(
  data,
  funcmean = meanImp_emmeans,
  metacond,
  is.parallel = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{data}{dataset}

\item{funcmean}{function that should be used to compute the mean}

\item{metacond}{a factor to specify the groups}

\item{is.parallel}{should parallel computing be used?}

\item{verbose}{should messages be displayed?}
}
\value{
List of variance-covariance matrices.
}
\description{
Computes the between-imputation component in the 2nd Rubin's rule for all peptides.
}
\examples{
library(mi4p)
data(datasim)
datasim_imp <- multi.impute(data = datasim[,-1], conditions = 
attr(datasim,"metadata")$Condition, method = "MLE")
rubin2bt.all(datasim_imp[1:5,,],funcmean = meanImp_emmeans,
attr(datasim,"metadata")$Condition)
}
\references{
M. Chion, Ch. Carapito and F. Bertrand (2021). \emph{Accounting for multiple imputation-induced variability for differential analysis in mass spectrometry-based label-free quantitative proteomics}.  \doi{doi:10.1371/journal.pcbi.1010420}.
}
\author{
Frédéric Bertrand
}
