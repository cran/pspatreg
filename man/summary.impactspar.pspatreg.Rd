% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.impactspar.pspatreg.R
\name{summary.impactspar.pspatreg}
\alias{summary.impactspar.pspatreg}
\title{Summary method for object of class impactspar.pspatreg.}
\usage{
\method{summary}{impactspar.pspatreg}(object, ...)
}
\arguments{
\item{object}{\emph{impactspar} object fitted using \code{\link{pspatfit}} function.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
An object of class \emph{summary.impactspar.pspatreg}
}
\description{
This method summarizes direct, indirect and total effects (or impacts)
  for continous parametric covariates in semiparametric spatial regression models.
}
\examples{
 # See examples for \code{\link{impactspar}} function.
}
\seealso{
\itemize{
  \item \code{\link{impactspar}} Compute direct, indirect and total
    impacts for continous parametric covariates.
  \item \code{\link{print.summary.impactspar.pspatreg}} print objects of
    class \emph{summary.pspatreg}
}
}
\author{
\tabular{ll}{ 
  Roman Minguez  \tab \email{roman.minguez@uclm.es} \cr
  Roberto Basile \tab \email{roberto.basile@univaq.it} \cr Maria Durban \tab
  \email{mdurban@est-econ.uc3m.es} \cr Gonzalo Espana-Heredia \tab
  \email{gehllanza@gmail.com} \cr 
 }
}
