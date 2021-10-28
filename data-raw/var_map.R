library(dplyr)

var_map = tribble(
  ~short, ~long,
  "p_L", "p(L|V*E*)",
  "p_V", "p(V)",
  "r_V", "p(V*|V)",
  "r_E", "p(E*|E)"
)

varmap_sl =
  var_map[["long"]]
names(varmap_sl) = var_map[["short"]]

varmap_ls =
  var_map[["short"]]
names(varmap_ls) = var_map[["long"]]

usethis::use_data(var_map, overwrite = TRUE)
usethis::use_data(varmap_ls, overwrite = TRUE)
usethis::use_data(varmap_sl, overwrite = TRUE)
