#' Title
#'
#' @param ... parameters to vary
#' @return
#' @export
#'
#' @importFrom dplyr select any_of filter row_number bind_rows bind_cols
#' @importFrom janitor remove_constant
#' @importFrom tidyr expand_grid
gen_tab1 = function(
  ...
)
{

  set.seed(1)

  tab1 =
    tidyr::expand_grid(...)

  theoretical_results =
    do.call(reporting_multiplier,
            tab1)

  tab1 %<>% bind_cols(
    theoretical_results %<>% select(-any_of(names(tab1)))
  )

  all_results = NULL
  #TODO: decomp into `run_sims_and_summarize()`
  for (cur in 1:nrow(tab1))
  {

    call_args =
      tab1 %>%
      dplyr::select(dplyr::any_of(names(formals(sim_data_binom)))) %>%
      dplyr::filter(dplyr::row_number() == cur)

    results =
      do.call(sim_data_binom, call_args) %>%
      sim_results_table(
        theoretical_results = tab1[cur,]
      )

    all_results %<>% dplyr::bind_rows(results)



  }

  all_results =
    dplyr::bind_cols(
      tab1 %>%
        select(
          all_of(names(list(...))),
          K = `R*/R`,
          `R^*` = `R*`) %>%
        remove_constant(),
      all_results %>% dplyr::select(
        # `\\bar{\\hat{K}}`,
        # `SD(\\hat{K})`,
        `\\bar{\\hat{R}}`,
        `SD(\\hat{R})`,
        `\\% \\{\\hat{R} < R_{\\text{true}}\\}`,
        # `\\% \\{\\hat{R}^* < \\hat{R}\\}`,
        `\\bar{\\hat{R}}_{\\text{adj}}`,
        `SD(\\hat{R}_{\\text{adj}})`
      )
    )
  names_to_replace = names(all_results) %in% varmap_sl
  names(all_results)[names_to_replace] = varmap_ls[names(all_results)[names_to_replace]]

  names(all_results) = paste("$", names(all_results), "$", sep = "")

  all_results %<>%
    mutate(
      across(where(function(x) is.numeric(x) && max(x) <= 1), formatC, digits = 3, format = "f")
    )
  return(all_results)
}
