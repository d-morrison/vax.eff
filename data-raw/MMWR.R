library(tidyverse)

age_groups = c("18-49", "50-64", "65+")

mmwr = data.frame(
  T = rep(c("4/4-6/19", "6/20-7/17"), each = 6),
  V = rep(c("not full", "full"), each =  3),
  A = age_groups,
  C = c(
   331151,
    93474,
    42884,
    10346,
     5850,
     7307,
    76237,
    17303,
     8093,
    13030,
     5027,
     4752
    ),
  H = c(
    10526,
     9158,
     9199,
    295,
    444,
    1286,
    2666,
    1755,
    1668,
    146,
    234,
    571),

  D = c(
    609,
    1380,
    3137,
    7,
    58,
    363,
    155,
    290,
    561,
    7,
    23,
    158

  )

) %>% tibble()


usethis::use_data(mmwr, overwrite = TRUE)
