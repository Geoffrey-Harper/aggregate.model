#' Estimate the specific module using indicator saturation
#'
#' @param clean_data An input data.frame or tibble. Must be the output of clean_data() to fit all requirements.
#' @param dep_var_basename A character string of the name of the dependent variable as contained in clean_data() in a level form (i.e. no ln or D in front of the name).
#' @param x_vars_basename A character vector of the name(s) of the independent variable(s) as contained in clean_data() in a level form (i.e. no ln or D in front of the name).
#' @param use_logs To decide whether to log any variables. Must be one of "both", "y", or "x". Default is "both".
#' @param ardl_or_ecm Either 'ardl' or 'ecm' to determine whether to estimate the model as an Autoregressive Distributed Lag Function (ardl) or as an Equilibrium Correction Model (ecm).
#' @param max.lag The maximum number of lags to use for both the AR terms as well as for the independent variables.
#' @param saturation Carry out Indicator Saturation using the 'isat' function in the 'gets' package. Needes is a character vector or string. Default is 'c("IIS","SIS")' to carry out Impulse Indicator Saturation and Step Indicator Saturation. Other possible values are 'NULL' to disable or 'TIS' or Trend Indicator Saturation. When disabled, estimation will be carried out using the 'arx' function from the 'gets' package.
#' @param saturation.tpval The target p-value of the saturation methods (e.g. SIS and IIS, see the 'isat' function in the 'gets' package). Default is 0.01.
#'
#' @return
#' @export
#'
#' @examples
#' sample_data <- tibble(time = rep(seq.Date(from = as.Date("2000-01-01"), to = as.Date("2000-12-31"),by = 1),each = 2), na_item = rep(c("yvar","xvar"),366), values = rnorm(366*2,mean = 100))
#' sample_data_clean <- clean_data(sample_data, max.lag = 4)
#' estimate_module(sample_data_clean, "yvar","xvar")

estimate_module <- function(clean_data,
                            dep_var_basename = "imports_of_goods_and_services",
                            x_vars_basename = c("gross_capital_formation",
                                                "household_and_npish_final_consumption_expenditure"),
                            use_logs = c("both","y","x"),
                            ardl_or_ecm = "ardl",
                            max.lag = 4,
                            saturation = c("IIS","SIS"),
                            saturation.tpval = 0.01){

  log_opts <- match.arg(use_logs)

  if(!ardl_or_ecm %in% c("ardl","ecm")){stop("The variable 'ardl_or_ecm' in the 'estimate_module' function must be either 'ecm' or 'ardl'. You have supplied a different value.")}

  isat_list <- tibble(ar = 0:max.lag,
                      BIC = 0,
                      isat_object = list(NA_complex_))
  for(i in 0:max.lag){

    if(ardl_or_ecm == "ardl"){
      if(log_opts %in% c("both","x")){
        xvars_names <- grep("L[0-9]\\.ln",
                            grep(paste0(x_vars_basename,collapse = "|"),names(clean_data), value = TRUE), value = TRUE)
      } else {
        xvars_names <- grep("L[0-9]\\.",
                            grep(paste0(x_vars_basename,collapse = "|"),names(clean_data), value = TRUE), value = TRUE)
      }

      yvar <- clean_data %>%
        select(all_of(paste0(ifelse(log_opts %in% c("both","y"),"ln.",""),dep_var_basename))) %>%
        pull

      xvars <- clean_data %>%
        select(all_of(paste0(ifelse(log_opts %in% c("both","x"),"ln.",""),x_vars_basename)),
               if(i != 0) {all_of(xvars_names[grepl(paste0(1:i,collapse = "|"),xvars_names)])} else {NULL},
               q_2,q_3,q_4)
    }
    if(ardl_or_ecm == "ecm"){
      yvar <- clean_data %>%
        select(all_of(paste0(ifelse(log_opts %in% c("both","y"),"D.ln.","D."),dep_var_basename))) %>%
        pull

      xvars_names <- grep("L[0-9]\\.D.",
                          grep(paste0(x_vars_basename,collapse = "|"),names(clean_data), value = TRUE), value = TRUE)

      xvars <- clean_data %>%
        select(all_of(paste0(ifelse(log_opts %in% c("both","y"),"L1.ln.","L1."),dep_var_basename)),
               all_of(paste0(ifelse(log_opts %in% c("both","x"),"L1.ln.","L1."),x_vars_basename)),
               all_of(paste0(ifelse(log_opts %in% c("both","x"),"D.ln.","D."),x_vars_basename)),
               if(i != 0) {all_of(xvars_names[grepl(paste0(1:i,collapse = "|"),xvars_names)])} else {NULL},
               q_2,q_3,q_4)
    }

    if(!is.null(saturation)){
      debug_list <- list(yvar = yvar, xvars = xvars,i = i,saturation.tpval = saturation.tpval)
      save(debug_list, file = "debug_list.RData")
      intermed.model <- isat(y = yvar,
                             mxreg = xvars,
                             ar = if (i != 0) {1:i} else{NULL},
                             plot = FALSE,
                             print.searchinfo = FALSE,
                             iis = TRUE,
                             sis = TRUE,
                             t.pval = saturation.tpval)


    } else {
      intermed.model <- arx(y = yvar,
                            mxreg = xvars,
                            ar = if (i != 0) {1:i} else{NULL},
                            plot = FALSE)
    }


    isat_list[i + 1,"BIC"] <- BIC(intermed.model)
    isat_list[i + 1,"isat_object"] <- tibble(isat_object = list(intermed.model))

  }

  out <- list()
  out$isat_list <- isat_list
  out$best_model <- isat_list %>% filter(BIC == min(BIC)) %>% pull(isat_object) %>% first
  out$args <- list(clean_data,dep_var_basename,x_vars_basename, use_logs, ardl_or_ecm, max.lag)

  return(out)
}