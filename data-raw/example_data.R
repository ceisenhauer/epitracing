example_cases <- rio::import(here::here('data-raw', 'example_cases.csv'))
example_pairs <-  rio::import(here::here('data-raw', 'example_pairs.csv'))

usethis::use_data(example_cases, overwrite = TRUE)
usethis::use_data(example_pairs, overwrite = TRUE)

