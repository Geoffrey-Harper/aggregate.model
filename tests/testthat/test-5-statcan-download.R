test_that('statCan_load_or_download works', {

  skip_on_cran()
  skip_on_ci()

  spec <- dplyr::tibble(
    type = c(
      "d",
      "n"
    ),
    dependent = c(
      "EmiCO2Industry",
      "HICP_Energy"
    ),
    independent = c(
      "HICP_GAS + HICP_Energy + IndProdGDP + WORLD_OIL",
      "IndProd"
    )
  )

  dict_statCan <- tibble::tribble(
    ~model_varname, ~full_name, ~database, ~variable_code, ~dataset_id, ~var_col ,~freq, ~GEO, ~geo, ~unit, ~s_adj, ~`Seasonal adjustment`, ~nace_r2, ~`North American Industry Classification System (NAICS)`, ~`North American Product Classification System (NAPCS)`,~Prices, ~`Type of fuel`, ~`Products and product groups`,~found, ~ipcc_sector, ~cpa2_1, ~siec,~ref_area,~commodity,~unit_measure,~start_period,~end_period,
    "HICP_Energy", "Harmonised Index of Consumer Prices, Energy, index 100 = 2002", "statcan", NA,"18-10-0004-01","na_item","m","Canada", NA, "units", NA, NA, NA, NA, NA, NA, NA, "Energy", NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "HICP_GAS", "Harmonised Index of Consumer Prices, Gas, index 100 = 2002", "statcan", NA, "18-10-0004-01", "na_item", "m", "Canada", NA, "units", NA, NA, NA, NA, NA, NA, NA, "Gasoline", NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "GAS", "Monthly Average Retail Price for gas", "statcan", NA, "18-10-0001-01", "na_item", "m", "Canada", NA, NA, NA, NA, NA, NA, NA, NA, "Regular unleaded gasoline at self service filling stations", NA, NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "IndProdGDP", "Industrial production [T010] in 2017 constant prices", "statcan", NA, "36-10-0434-01", "na_item" ,"m", "Canada", NA, NA, NA, "Seasonally adjusted at annual rates", NA, "Industrial production [T010]", NA, "2017 constant prices", NA, NA, NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "IndProd", "Total, Industrial product price index (IPPI)", "statcan", NA, "18-10-0266-01", "na_item", "m", "Canada", NA, NA, NA, NA, NA, NA, "Total, Industrial product price index (IPPI)", NA, NA, NA, NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "WORLD_OIL", "World Oil Price USD", "imf", NA, "PCPS", "na_item","M", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,"W00","POILAPSP","USD",NA,NA
  )
  dict_statCan <- as.data.frame(dict_statCan)

  module_order <- aggregate.model:::check_config_table(spec)
  dictionary <- dict_statCan #aggregate.model::statcan_dict
  to_obtain <- aggregate.model:::determine_variables(specification = module_order,
                                                     dictionary = dictionary)

  actual_cols = colnames(dictionary)
  # basic functionality
  data <- aggregate.model:::download_statcan(to_obtain = to_obtain,
                                          #column_filters = additional_filters,
                                          column_filters = actual_cols,
                                          quiet = FALSE)

  expect_length(data, 2)
  expect_type(data, "list")
  expect_named(data, c("df", "to_obtain"))
  expect_true(all(data$to_obtain[which(to_obtain$database == "statcan"), "found"]))
  expect_identical(data$df %>% dplyr::filter(na_item == "HICP_GAS") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
  expect_identical(data$df %>% dplyr::filter(na_item == "HICP_Energy") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
  expect_identical(data$df %>% dplyr::filter(na_item == "IndProd") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))


})

test_that('statcan_load_and_download_forecasting_functionality',{
  #library("tinytest")
  #using("tinysnapshot")
  library("readr")
  skip_on_cran()
  skip_on_ci()

  #setup
  spec <- dplyr::tibble(
    type = c(
      "d",
      "n"
    ),
    dependent = c(
      "EmiCO2Industry",
      "HICP_Energy"
    ),
    independent = c(
      "HICP_GAS + HICP_Energy + IndProdGDP + WORLD_OIL",
      "IndProd"
    )
  )

  dict_statCan <- tibble::tribble(
    ~model_varname, ~full_name, ~database, ~variable_code, ~dataset_id, ~var_col ,~freq, ~GEO, ~geo, ~unit, ~s_adj, ~`Seasonal adjustment`, ~nace_r2, ~`North American Industry Classification System (NAICS)`, ~`North American Product Classification System (NAPCS)`,~Prices, ~`Type of fuel`, ~`Products and product groups`,~found, ~ipcc_sector, ~cpa2_1, ~siec,~ref_area,~commodity,~unit_measure,~start_period,~end_period,
    "HICP_Energy", "Harmonised Index of Consumer Prices, Energy, index 100 = 2002", "statcan", NA,"18-10-0004-01","na_item","m","Canada", NA, "units", NA, NA, NA, NA, NA, NA, NA, "Energy", NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "HICP_GAS", "Harmonised Index of Consumer Prices, Gas, index 100 = 2002", "statcan", NA, "18-10-0004-01", "na_item", "m", "Canada", NA, "units", NA, NA, NA, NA, NA, NA, NA, "Gasoline", NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "GAS", "Monthly Average Retail Price for gas", "statcan", NA, "18-10-0001-01", "na_item", "m", "Canada", NA, NA, NA, NA, NA, NA, NA, NA, "Regular unleaded gasoline at self service filling stations", NA, NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "IndProdGDP", "Industrial production [T010] in 2017 constant prices", "statcan", NA, "36-10-0434-01", "na_item" ,"m", "Canada", NA, NA, NA, "Seasonally adjusted at annual rates", NA, "Industrial production [T010]", NA, "2017 constant prices", NA, NA, NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "IndProd", "Total, Industrial product price index (IPPI)", "statcan", NA, "18-10-0266-01", "na_item", "m", "Canada", NA, NA, NA, NA, NA, NA, "Total, Industrial product price index (IPPI)", NA, NA, NA, NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "WORLD_OIL", "World Oil Price USD", "imf", NA, "PCPS", "na_item","M", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,"W00","POILAPSP","USD",NA,NA
  )

  #as.data.frame(dict_statCan)

  module_order <- aggregate.model:::check_config_table(spec)
  dictionary <- dict_statCan #aggregate.model::statcan_dict
  to_obtain <- aggregate.model:::determine_variables(specification = module_order,
                                                     dictionary = dictionary)


  df = read_csv('canada_imf_stable_data.csv')

  #run the model
  expect_warning( model_run <- run_model(specification = spec,
                                     dictionary = dictionary,
                                     inputdata_directory = df,
                                     primary_source = "local")
    , "Unbalanced panel")

  #forcast the model
  model_forecast <- forecast_model(model_run, exog_fill_method = "AR", plot.forecast = FALSE)

  #plot model
  expect_is(plot.aggmod.forecast(model_forecast,order.as.run = TRUE),class = c("gg","ggplot"))

  #hindcast model
  hind_cast <- forecast_insample(
    model_run,
    sample_share = 0.5,
    uncertainty_sample = 100,
    exog_fill_method = "last",
    plot.forecast = TRUE
  )
  expect_is(hind_cast$plot,class = c("gg","ggplot"))


})

##TODO
##STRESS TESTS
