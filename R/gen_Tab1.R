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
    tidyr::expand_grid(...) %>%
    reporting_multiplier()

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
          K = `R*/R`) %>%
        remove_constant(),
      all_results %>% dplyr::select(
        `\\bar{\\hat{K}}`,
        `SD(\\hat{K})`,
        `\\% \\{\\hat{R^*} < R\\}`,
        `\\% \\{\\hat{R^*} < \\hat{R}\\}`
      )
    )

  names(all_results) = paste("$", names(all_results), "$", sep = "")

  return(all_results)
}
