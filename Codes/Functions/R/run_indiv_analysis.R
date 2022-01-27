# /*===========================================================
#' # GWR analysis
# /*===========================================================
# ! Run GWR and do economic analysis

run_indiv_analysis <- function(model, train_data, test_data, x_vars, prices) {
  if (model == "lm") { # linear quadratic
    results <-
      run_linear_analysis(
        train_data,
        test_data,
        x_vars,
        prices
      )
  } else if (model == "gwr_t") { # gwr-trevisan
    results <-
      run_GWR_analysis(
        train_data,
        prices
      )
  } else if (model == "gwr_semi") { # gwr-semiparametric
    results <-
      run_GWR_semi_analysis(
        gam_formula = formula(yield ~ s(N, k = 4), m = 2),
        train_data,
        prices
      )
  } else if (model == "gwr_zone_scam") { # scam by zone
    gwr_results <-
      run_GWR_analysis(
        train_data,
        prices
      )
    results <-
      run_gwr_zone_scam_analysis(
        gwr_results = gwr_results,
        reg_results = reg_results,
        num_zones = 3,
        train_data = train_data,
        prices = prices,
      )
  } else if (model == "ma_cf") { # multiarm-CF
    results <-
      run_macf_analysis(
        train_data,
        test_data,
        x_vars,
        prices
      )
  } else if (model == "brf") { # brf
    results <-
      run_brf_analysis(
        train_data,
        test_data,
        x_vars,
        prices
      )
  } else if (model == "dmlof_semi") { # semiparmetric DML-OF
    results <-
      run_DML_OF_c_analysis(
        gam_formula = formula(yield ~ s(N, k = 4, m = 2)),
        x_vars = x_vars,
        w_vars = c(x_vars, "x", "y"),
        train_data = train_data,
        test_data = test_data,
        prices = prices
      )
  } else if (model == "drof") { # DR-OF
    results <-
      run_drof_analysis(
        train_data,
        x_vars,
        prices
      )
  }
}