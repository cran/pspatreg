% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_terms.R
\name{fit_terms}
\alias{fit_terms}
\title{Compute terms of the non-parametric covariates in the
  semiparametric regression models.}
\usage{
fit_terms(object, variables, intercept = FALSE)
}
\arguments{
\item{object}{object fitted using \code{\link{pspatfit}} function.}

\item{variables}{vector including names of non-parametric covariates. 
To fit the terms of non-parametric spatial (2d) or spatio-temporal 
(3d) trend this argument must be set equal to `spttrend`. 
See \code{examples} in this function.}

\item{intercept}{add intercept to fitted term. Default = FALSE.}
}
\value{
A list including:
  \tabular{ll}{
    \emph{fitted_terms} \tab Matrix including terms in columns. \cr
    \emph{se_fitted_terms} \tab Matrix including standard errors of terms 
      in columns. \cr
    \emph{fitted_terms_fixed} \tab Matrix including fixed part of terms 
      in columns. \cr
    \emph{se_fitted_terms_fixed} \tab Matrix including standard errors of 
      fixed part of terms in columns. \cr
    \emph{fitted_terms_random} \tab Matrix including random part of terms 
      in columns. \cr
    \emph{se_fitted_terms_random} \tab Matrix including standard errors of 
      random part of terms in columns.\cr
 }
 This object can be used as an argument of \code{\link{plot_terms}} function
 to make plots of both non-parametric trends and smooth functions of 
 covariates. See \emph{examples} below.
}
\description{
The \code{fit_terms} function compute both:
\itemize{
  \item Non-parametric spatial (2d) or spatio-temporal (3d) trends 
    including the decomposition in main and interaction trends 
    when the model is ANOVA.
    \item Smooth functions \eqn{f(x_i)} for non-parametric covariates 
      in semiparametric models. It also includes standard errors and the 
      decomposition of each non-parametric
      term in fixed and random parts.
}
}
\examples{
###################### Examples using a panel data of rate of unemployment 
###################### in 103 Italian provinces during the period 1996-2014.
library(pspatreg)
data(unemp_it, package = "pspatreg")
lwsp_it <- spdep::mat2listw(Wsp_it)
#######  No Spatial Trend: PSAR including a spatial 
#######  lag of the dependent variable
form1 <- unrate ~ partrate + agri + cons + 
                  pspl(serv, nknots = 15) +
                  pspl(empgrowth, nknots = 20)  
gamsar <- pspatfit(form1, data = unemp_it, 
                    type = "sar", listw = lwsp_it)
summary(gamsar)

######  Fit non-parametric terms 
######  (spatial trend must be name "spttrend")
list_varnopar <- c("serv", "empgrowth")
terms_nopar <- fit_terms(gamsar, list_varnopar)

######################  Plot non-parametric terms
plot_terms(terms_nopar, unemp_it)
 
}
\references{
\itemize{ 
  \item Lee, D. and Durban, M. (2011). P-Spline ANOVA Type Interaction 
    Models for Spatio-Temporal Smoothing. \emph{Statistical Modelling}, 
    (11), 49-69. <doi:10.1177/1471082X1001100104>
    
  \item Eilers, P. and Marx, B. (2021). \emph{Practical Smoothing. 
  The Joys of P-Splines}. Cambridge University Press.
    
  \item Fahrmeir, L.; Kneib, T.;  Lang, S.; and Marx, B. (2021). 
    \emph{Regression. Models, Methods and Applications (2nd Ed.)}.
    Springer.
        
  \item Wood, S.N. (2017). \emph{Generalized Additive Models. 
  An Introduction with \code{R}} (second edition). CRC Press, Boca Raton.
 }
}
\seealso{
\itemize{
  \item \code{\link{pspatfit}} estimate spatial or spatio-temporal 
    semiparametric regression models. The model can be of type \emph{ps-sim},
    \emph{ps-sar}, \emph{ps-slx}, \emph{ps-sem}, \emph{ps-sdm} or 
    \emph{ps-sarar}.
  \item \code{\link{plot_terms}} plot smooth functions of non-parametric
    covariates.
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
