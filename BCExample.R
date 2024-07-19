library(statcanR)
library(tidyverse)
library(readr)
library(magrittr)

#toy specs for example
spec <- dplyr::tibble(
  type = c(
    "d"
  ),
  dependent = c(
    "EmiCO2Industry"
  ),
  independent = c(
    "HICP_GAS + HICP_Energy + IndProdGDP"

  )
)

#toy dictionary for example

dict_statCanBC <- tibble::tribble(
  ~model_varname, ~full_name, ~database, ~variable_code, ~dataset_id, ~var_col ,~freq, ~GEO, ~geo, ~unit, ~s_adj, ~`Seasonal adjustment`, ~nace_r2, ~`North American Industry Classification System (NAICS)`, ~`North American Product Classification System (NAPCS)`,~Prices, ~`Type of fuel`, ~`Products and product groups`,~found, ~ipcc_sector, ~cpa2_1, ~siec,
  "HICP_Energy", "Harmonised Index of Consumer Prices, Energy, index 100 = 2002", "statcan", NA,"18-10-0004-01","na_item","m","British Columbia", NA, "units", NA, NA, NA, NA, NA, NA, NA, "Energy", NA, NA, NA, NA,
  "HICP_GAS", "Harmonised Index of Consumer Prices, Gas, index 100 = 2002", "statcan", NA, "18-10-0004-01", "na_item", "m", "British Columbia", NA, "units", NA, NA, NA, NA, NA, NA, NA, "Gasoline", NA, NA, NA, NA,
  "GAS", "Monthly Average Retail Price for gas", "statcan", NA, "18-10-0001-01", "na_item", "m", "Vancouver, British Columbia", NA, NA, NA, NA, NA, NA, NA, NA, "Regular unleaded gasoline at full service filling stations", NA, NA, NA, NA, NA,
  "IndProdGDP", "Industrial production [T010] in 2017 constant prices", "statcan", NA, "36-10-0434-01", "na_item" ,"m", "Canada", NA, NA, NA, "Seasonally adjusted at annual rates", NA, "Industrial production [T010]", NA, "2017 constant prices", NA, NA, NA, NA, NA, NA,
  "IndProd", "Total, Industrial product price index (IPPI)", "statcan", NA, "18-10-0266-01", "na_item", "m", "Canada", NA, NA, NA, NA, NA, NA, "Total, Industrial product price index (IPPI)", NA, NA, NA, NA, NA, NA, NA



)
#dict_statCan$variable_code <- NA
dict_statCanBC <- as.data.frame(dict_statCanBC)



dictionary <- dict_statCanBC #obtain dictionary


# determine whether user has added additional filters
default_cols <- colnames(dict_statCanBC)
actual_cols <- colnames(dictionary)
additional_filters <- setdiff(actual_cols, default_cols)

browser()
#extract variables, so that we can search for them in the statsCan database
to_obtain <- determine_variables(specification=spec,dictionary=dictionary)


model_result <- run_model(specification = spec, dictionary = dict_statCanBC, primary_source = "download")
browser()
model_forecast <- forecast_model(model_result, n.ahead = 10, exog_fill_method = "AR", plot.forecast = FALSE)

plot <- plot.aggmod.forecast(model_forecast,order.as.run = TRUE)

hind_cast <- forecast_insample(
  model_result,
  sample_share = 0.5,
  uncertainty_sample = 100,
  exog_fill_method = "AR",
  plot.forecast = TRUE
)



