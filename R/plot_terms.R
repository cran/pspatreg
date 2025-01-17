#' @name plot_terms
#' @rdname plot_terms
#'
#' @title Plot terms of the non-parametric covariates in the
#'   semiparametric regression models.
#'        
#' @description For each non-parametric covariate the plot of the term
#'   includes confidence intervals and the decomposition in fixed and 
#'   random part when the term is reparameterized as a mixed model.  
#'
#' @param fitterms object returned from \code{\link{fit_terms}} function.
#' @param data dataframe or sf with the data. 
#' @param type type of term plotted between "global" (Default), 
#'   "fixed" or "random".
#' @param alpha numerical value for the significance level of the pointwise 
#'   confidence intervals of the nonlinear terms. Default 0.05.
#' @param listw used to compute spatial lags for Durbin specifications. 
#'   Default =  `NULL`
#' @param dynamic Logical value to set a dynamic model.
#'   Dynamic models include a temporal lag of the dependent
#'   variable in the right-hand side of the equation.
#'   Default = `FALSE`.
#' @param nt  Number of temporal periods. It is needed
#'   for dynamic models.  
#' @param decomposition Plot the decomposition of term in
#'   random and fixed effects.   
#'
#' @return list with the plots of the terms for each non-parametric 
#'   covariate included in the object returned from \code{\link{fit_terms}}.
#'                                 
#' @author 
#' \tabular{ll}{ 
#'   Roman Minguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Roberto Basile \tab \email{roberto.basile@@univaq.it} \cr Maria Durban \tab
#'   \email{mdurban@@est-econ.uc3m.es} \cr Gonzalo Espana-Heredia \tab
#'   \email{gehllanza@@gmail.com} \cr 
#'  }
#'  
#' @seealso
#' \itemize{
#'   \item \code{\link{fit_terms}} compute smooth functions for non-parametric
#'                                 continuous covariates.
#'   \item \code{\link{impactsnopar}} plot the effects functions 
#'     of non-parametric covariates.
#'   \item \code{\link[mgcv]{vis.gam}} plot the terms fitted by 
#'     \code{\link[mgcv]{gam}} function in \pkg{mgcv} package.   
#' }
#' 
#' @references \itemize{ 
#'   \item Wood, S.N. (2017). \emph{Generalized Additive Models. 
#'   An Introduction with \code{R}} (second edition). CRC Press, Boca Raton.
#'  }
#'         
#' @examples
#' ################################################
#' # Examples using spatial data of Ames Houses.
#' ###############################################
#' # Getting and preparing the data
#' library(pspatreg)
#' library(spdep)
#' library(sf)
#' ames <- AmesHousing::make_ames() # Raw Ames Housing Data
#' ames_sf <- st_as_sf(ames, coords = c("Longitude", "Latitude"))
#' ames_sf$Longitude <- ames$Longitude
#' ames_sf$Latitude <- ames$Latitude
#' ames_sf$lnSale_Price <- log(ames_sf$Sale_Price)
#' ames_sf$lnLot_Area <- log(ames_sf$Lot_Area)
#' ames_sf$lnTotal_Bsmt_SF <- log(ames_sf$Total_Bsmt_SF+1)
#' ames_sf$lnGr_Liv_Area <- log(ames_sf$Gr_Liv_Area)
#' ames_sf1 <- ames_sf[(duplicated(ames_sf$Longitude) == FALSE), ]
#'                     
#' form1 <- lnSale_Price ~ Fireplaces + Garage_Cars +
#'           pspl(lnLot_Area, nknots = 20) + 
#'           pspl(lnTotal_Bsmt_SF, nknots = 20) +
#'           pspl(lnGr_Liv_Area, nknots = 20)    
#' 
#' \donttest{ 
#' ########### Constructing the spatial weights matrix
#' coord_sf1 <- cbind(ames_sf1$Longitude, ames_sf1$Latitude)
#' k5nb <- knn2nb(knearneigh(coord_sf1, k = 5, 
#'                           longlat = TRUE, use_kd_tree = FALSE), sym = TRUE)
#' lw_ames <- nb2listw(k5nb, style = "W", 
#'                   zero.policy = FALSE)
#' gamsar <- pspatfit(form1, data = ames_sf1, 
#'                    type = "sar", listw = lw_ames,
#'                    method = "Chebyshev")
#' summary(gamsar)
#' list_varnopar <- c("lnLot_Area", "lnTotal_Bsmt_SF", 
#' "lnGr_Liv_Area")
#' terms_nopar <- fit_terms(gamsar, list_varnopar)
#' ######################  Plot non-parametric terms
#' plot_terms(terms_nopar, ames_sf1)
#' 
#' ###### Examples using a panel data of rate of
#' ###### unemployment for 103 Italian provinces in period 1996-2014.
#' library(pspatreg)
#' data(unemp_it, package = "pspatreg")
#' lwsp_it <- spdep::mat2listw(Wsp_it)
#' 
#' ########  No Spatial Trend: ps-sar including a spatial 
#' ########  lag of the dependent variable
#' form1 <- unrate ~ partrate + agri + cons + 
#'                   pspl(serv,nknots = 15) +
#'                   pspl(empgrowth,nknots = 20) 
#' gamsar <- pspatfit(form1, data = unemp_it, 
#'                    type = "sar", listw = Wsp_it)
#' summary(gamsar)
#' ########  Fit non-parametric terms (spatial trend must be name "spttrend")
#' list_varnopar <- c("serv", "empgrowth")
#' terms_nopar <- fit_terms(gamsar, list_varnopar)
#' #######  Plot non-parametric terms
#' plot_terms(terms_nopar, unemp_it)
#' }  
#' @export
plot_terms <- function(fitterms, 
                       data, 
                       type = "global",
                       alpha = 0.05, 
                       listw = NULL,  
                       dynamic = FALSE, 
                       nt = NULL,
                       decomposition = FALSE) {
  if (inherits(data, "sf"))
    data <- st_drop_geometry(data)
  nfull <- nrow(data)
  if (dynamic) {
    if (is.null(nt)) 
      stop("plot_impactsnopar function needs nt as argument for dynamic models")
    idxyear1 <- seq(from = 1, to = nfull, by = nt)
    data <- data[-idxyear1, ]
  }
  fit <- fitterms$fitted_terms
  se_fit <- fitterms$se_fitted_terms
  fit_fixed <- fitterms$fitted_terms_fixed
  se_fit_fixed <- fitterms$se_fitted_terms_fixed
  fit_random <- fitterms$fitted_terms_random
  se_fit_random <- fitterms$se_fitted_terms_random
  variables <- colnames(fit)
  crval <- qnorm(alpha/2, mean = 0, 
                 sd = 1, lower.tail = FALSE)
  for (i in 1:length(variables)) {
    name_var <- variables[i]
    fit_var <- matrix(fit[,c(name_var)], ncol = 1)
    colnames(fit_var) <- name_var
    se_fit_var <- matrix(se_fit[,c(name_var)], ncol = 1)
    colnames(se_fit_var) <- name_var
    up_fit_var <- fit_var + crval*se_fit_var
    colnames(up_fit_var) <- name_var
    low_fit_var <- fit_var - crval*se_fit_var
    colnames(low_fit_var) <- name_var
    fit_var_fixed <- matrix(fit_fixed[,c(name_var)], ncol = 1)
    colnames(fit_var_fixed) <- name_var
    se_fit_var_fixed <- matrix(se_fit_fixed[,c(name_var)], ncol = 1)
    colnames(se_fit_var_fixed) <- name_var
    up_fit_var_fixed <- fit_var_fixed + crval*se_fit_var_fixed
    colnames(up_fit_var_fixed) <- name_var
    low_fit_var_fixed <- fit_var_fixed - crval*se_fit_var_fixed
    colnames(low_fit_var_fixed) <- name_var
    fit_var_random <- matrix(fit_random[, c(name_var)], ncol = 1)
    colnames(fit_var_random) <- name_var
    se_fit_var_random <- matrix(se_fit_random[,c(name_var)], ncol = 1)
    colnames(se_fit_var_random) <- name_var
    up_fit_var_random <- fit_var_random + crval*se_fit_var_random
    colnames(up_fit_var_random) <- name_var
    low_fit_var_random <- fit_var_random - crval*se_fit_var_random
    colnames(low_fit_var_random) <- name_var
    # Check for Wlag.var
    if (grepl("Wlag", name_var)) {
      new_name_var <- str_replace(name_var,"Wlag.","")
      var <- as.matrix(data[, c(new_name_var)])
      Wsp <- listw2mat(listw)
      # spatio-temporal data 
      if (nrow(var) > nrow(Wsp)) {
        nt <- nrow(var) %/% nrow(Wsp)
        It <- Diagonal(nt)
        Wspt <- kronecker(Wsp, It)
        listw <- mat2listw(as.matrix(Wspt))
      }
      colnames(var) <- name_var
      var <- create_WX(var, listw = listw, 
                       zero.policy = TRUE, 
                       prefix = "")
    } else var <- as.matrix(data[, c(name_var)])
    colnames(var) <- name_var
    ord <- order(var)
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    if (decomposition) par(mfrow = c(2, 1))
    if (type == "global") {
      termplot <- fit_var
      low_termplot <- low_fit_var
      up_termplot <- up_fit_var
    } else if (type == "fixed") {
      termplot <- fit_var_fixed
      low_termplot <- low_fit_var_fixed
      up_termplot <- up_fit_var_fixed
    } else if (type == "random") {
      termplot <- fit_var_random
      low_termplot <- low_fit_var_random
      up_termplot <- up_fit_var_random
    } else stop("type must be: \"global\", \"fixed\" or \"random\" ")
    # Set maximum and minimum
    miny <- min(low_termplot)
    maxy <- max(up_termplot)
    plot(var[ord], termplot[ord], 
         type = "l",
         ylab = paste("f(", name_var, ")"), 
         xlab = name_var,
         ylim = c(miny, maxy), 
         cex.lab = 1.0, 
         col = 2, 
         lty = 1, 
         lwd = 2, 
         cex.main = 1.0,
         main = paste("Term: ", paste("f(",name_var,")"), 
                                      paste(" type = ", type)),
         sub = "Pointwise confidence intervals in dashed lines")
    lines(var[ord], low_termplot[ord],
          ylim = c(miny, maxy),          
          xlab = "",
          ylab = "", 
          type = "l", 
          col = 2, 
          lty = 2, 
          lwd = 1.5)
    lines(var[ord], up_termplot[ord], 
          ylim = c(miny, maxy),
          xlab = "",  
          ylab = "", 
          type = "l", 
          col = 2, 
          lty = 2, 
          lwd = 1.5)
    abline(a = 0, b = 0)
    if (decomposition) {
      # Set maximum and minimum
      miny <- min(c(fit_var, fit_var_fixed, fit_var_random))
      maxy <- max(c(fit_var, fit_var_fixed, fit_var_random))
      miny <- miny 
      maxy <- maxy 
      plot(var[ord], 
           fit_var[ord], 
           type = "l",
           ylab = paste("f(",name_var,")"), 
           xlab = name_var,
           ylim = c(miny, maxy), 
           cex.lab = 1.0, 
           col = 2, 
           lty = 1, 
           lwd = 2,
           cex.main = 1.0,
           main = paste("Decomposition of ", 
                        paste("f(",name_var,")")),
           sub = paste("Global (red), Fixed (green) and Random (blue) Terms"))
      lines(var[ord], 
            fit_var_fixed[ord], 
            ylim = c(miny, maxy), 
            xlab = "", 
            ylab = "", 
            type = "l", 
            col = 3, 
            lty = 2, 
            lwd = 2)
      lines(var[ord], 
            fit_var_random[ord], 
            ylim = c(miny, maxy),
            xlab = "", 
            ylab = "", 
            type = "l", 
            col = 4, 
            lty = 3, 
            lwd = 2)
      abline(a = 0, b = 0)
    }
    readline(prompt = "Press [enter] to continue")
  }
}




