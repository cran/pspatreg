#' @name plot_sp3d 
#' @rdname plot_sp3d
#'
#' @title Plot and mapping spatio-temporal trends.
#'         
#' @description Make plots and maps of the spatio-temporal trends 
#'   in 3d of the objects fitted with \code{\link{pspatfit}} function.
#'   
#' @param object object returned from \code{\link{pspatfit}} 
#' @param data sf object. 
#' @param time_var name of the temporal variable in data.
#' @param time_index vector of time points to plot. 
#' @param addmain Add f1_main and f2_main plots in psanova case.
#' @param addint Add f12_int in psanova case.
#' 
#' @return plots and maps of the spatial trends                                  
#' @author 
#' \tabular{ll}{ 
#'   Roman Minguez  \tab \email{roman.minguez@@uclm.es} \cr
#'   Roberto Basile \tab \email{roberto.basile@@univaq.it} \cr Maria Durban \tab
#'   \email{mdurban@@est-econ.uc3m.es} \cr Gonzalo Espana-Heredia \tab
#'   \email{gehllanza@@gmail.com} \cr 
#' }
#' 
#' @references \itemize{ 
#'   \item Lee, D. and Durban, M. (2011). P-Spline ANOVA Type Interaction 
#'     Models for Spatio-Temporal Smoothing. \emph{Statistical Modelling}, 
#'     (11), 49-69. <doi:10.1177/1471082X1001100104>
#'     
#'   \item Eilers, P. and Marx, B. (2021). \emph{Practical Smoothing. 
#'   The Joys of P-Splines}. Cambridge University Press.
#'     
#'   \item Fahrmeir, L.; Kneib, T.;  Lang, S.; and Marx, B. (2021). 
#'     \emph{Regression. Models, Methods and Applications (2nd Ed.)}.
#'     Springer.
#'         
#'   \item Wood, S.N. (2017). \emph{Generalized Additive Models. 
#'   An Introduction with \code{R}} (second edition). CRC Press, Boca Raton.
#'  }    
#' 
#' @examples
#' library(pspatreg)
#' library(sf)
#' data(unemp_it, package = "pspatreg")
#' lwsp_it <- spdep::mat2listw(Wsp_it)
#' unemp_it_sf <- st_as_sf(dplyr::left_join(
#'                                unemp_it, map_it,  
#'                         by = c("prov" = "COD_PRO")))
#' ######## FORMULA of the model
#' form3d_psanova_restr <- unrate ~ partrate + agri + cons +
#'                         pspl(serv, nknots = 15) + 
#'                         pspl(empgrowth, nknots = 20) +
#'                         pspt(long, lat, year, 
#'                              nknots = c(18, 18, 8),
#'                              psanova = TRUE, 
#'                              nest_sp1 = c(1, 2, 3), 
#'                              nest_sp2 = c(1, 2, 3),
#'                              nest_time = c(1, 2, 2),
#'                              f1t = FALSE, f2t = FALSE)
#' 
#' \donttest{
#' ####### FIT the model
#' sp3danova <- pspatfit(form3d_psanova_restr, 
#'                       data = unemp_it_sf)
#' summary(sp3danova)                          
#' 
#' ###### Plot spatio-temporal trends for different years
#' plot_sp3d(sp3danova, data = unemp_it_sf, 
#'           time_var = "year", 
#'           time_index = c(1996, 2005, 2019),
#'           addmain = FALSE, addint = FALSE)
#' ###### Plot of spatio-temporal trend, main effects 
#' ######      and interaction effect for a year
#' plot_sp3d(sp3danova, data = unemp_it_sf, 
#'           time_var = "year", 
#'           time_index = c(2019),
#'           addmain = TRUE, addint = TRUE)
#'           
#' #### Plot of temporal trend for each province
#' plot_sptime(sp3danova, 
#'             data = unemp_it, 
#'             time_var = "year", 
#'             reg_var = "prov") 
#' }
#'              
#' @export
plot_sp3d <- function(object, data,
                      time_var,
                      time_index,
                      addmain = TRUE,
                      addint = TRUE) {
  ## VIP: Assumption sf object...
## The function needs this arguments:
  # 1. A fitted object of class "pspatreg"
  # 2. A database that could be an sf object or a dataframe.
  # 3. In the last case (dataframe) the names of the
  ##   spatial coordinates need to be supplied.
 ######################################################################
  if (!(inherits(object, "pspatreg"))) 
    stop("object must be of class pspatreg")
  if (!(inherits(data, "sf"))) 
    stop("data must be an sf object")
  if (!(time_var %in% names(data)))
    stop("time_var must be between the names of data")
  dynamic <- object$dynamic
  nfull <- nrow(data)
  nt <- object$nt
  if (dynamic) {
    idxyear1 <- seq(from = 1, to = nfull, by = nt)
    data <- data[-idxyear1, ]
  }  
  pos_time_var <- which(time_var == names(data))
  time_var <- data[, pos_time_var]
  if (inherits(time_var, "sf"))
    time_var <- st_drop_geometry(time_var)
  sp3dfitl <- fit_terms(object, "spttrend") # list object
  data$sp3dtrend <- sp3dfitl$fitted_terms[, "spttrend"]
  if (object$psanova) {
    data$f1_main <- sp3dfitl$fitted_terms[, "f1_main"]
    data$f1_main <- data$f1_main - mean(data$f1_main)
    data$f2_main <- sp3dfitl$fitted_terms[, "f2_main"]
    data$f2_main <- data$f2_main - mean(data$f2_main)
    data$f12_int <- sp3dfitl$fitted_terms[, "f12_int"]
    data$f12_int <- data$f12_int - mean(data$f12_int)
    data$intercept <- sp3dfitl$fitted_terms[, "Intercept"]
  } 
  for (i in seq_along(time_index)) {
    year_i <- time_index[i]
    data_i <- data[time_var == year_i, ]
    df_i <- data_i[, c("sp3dtrend")]
    df_i$sp3dtrend <- df_i$sp3dtrend - mean(df_i$sp3dtrend)
    min_i_sp3dtrend <- min(df_i$sp3dtrend)
    max_i_sp3dtrend <- max(df_i$sp3dtrend)
    range_i_sp3dtrend <- c(min_i_sp3dtrend - 0.01, 
                         max_i_sp3dtrend + 0.01)
    breaks_i_sp3dtrend <- seq(min_i_sp3dtrend - 0.01, 
                            max_i_sp3dtrend + 0.01, 
                            by = diff(range(range_i_sp3dtrend))/5)
    min_i <- min_i_sp3dtrend 
    max_i <- max_i_sp3dtrend 
    if (object$psanova && addmain) {
      min_i <- min(c(min_i, data$f1_main, 
                     data$f2_main))
      max_i <- max(c(max_i, data$f1_main, 
                     data$f2_main))
    }
    if (object$psanova && addint) {
      min_i <- min(c(min_i, data$f12_int))
      max_i <- max(c(max_i, data$f12_int))
    }
    range_i <- c(min_i - 0.01, max_i + 0.01)
    breaks_i <- seq(min_i - 0.01, max_i + 0.01, 
                    by = diff(range(range_i))/5) 
    plot(df_i, main = paste("Spatial Trend (centered) for : ", 
                            year_i), 
        breaks = breaks_i_sp3dtrend)
    if (object$psanova && addmain) {
      readline(prompt="Press [enter] to continue")
      df_i <- data_i[, c("f1_main")]
      plot(df_i, breaks = breaks_i,
           main = paste("Spat. Trend: f1_main for : ", year_i))
      readline(prompt="Press [enter] to continue")
      df_i <- data_i[, c("f2_main")]
      plot(df_i, breaks = breaks_i,
           main =  paste("Spat. Trend: f2_main for : ", year_i))
    }
    if (object$psanova && addint) {
      readline(prompt="Press [enter] to continue")
      df_i <- data_i[, c("f12_int")]
      plot(df_i, breaks = breaks_i,
           main = paste("Spat. Trend: f12_int for : ", year_i))
    }
    readline(prompt = "Press [enter] to continue")
  }
}