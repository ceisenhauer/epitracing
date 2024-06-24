map_case <- function(case_id, order_id, cases, verbose = FALSE) {
  if (verbose) {
    writeLines(paste('mapping case', case_id))
  }

  cases[cases$id == case_id, 'order_id'] <- order_id
  order_id <- order_id + 1

  secondaries <- cases[cases$infector_id == case_id, 'id']

  if (verbose) {
    writeLines(paste('mapping secondaries:', paste0(secondaries, collapse = ' ')))
  }

  for (s in secondaries) {
    out <- map_case(case_id = s,
                    order_id = order_id,
                    cases = cases)
    
    cases <- out$cases
    order_id <- out$order_id
  }

  return(list(cases = cases,
              order_id = order_id))
}


map_cases <- function(cases, pairs) {
  order_id <- 1

  cases <- cases[order(cases$date_symptoms), ]

  index_cases <- cases[which(cases$infector_id == "" | is.na(cases$infector_id)), 'id']
  for (case in index_cases) {
    out <- map_case(case_id = case,
                    order_id = order_id,
                    cases = cases)
    
    cases <- out$cases
    order_id <- out$order_id
  }

  cases <- cases[order(as.numeric(row.names(cases))), ]
  return(cases)
}

