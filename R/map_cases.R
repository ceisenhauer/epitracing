#' Map Order of Cases
#' 
#' @description Internal function that calculates visual order to position cases in linear and excel
#' output.
#'
#' @param cases `df` Data frame of case data.
#' @param var_id `chr` Column name for case id.
#' @param var_infector `chr` Column name for infector id.
#' @param var_order `chr` Column name for the variable used to visually order cases.
#' @param verbose `bool` Whether to print output on cases being mapped.
map_cases <- function(cases, var_id = 'id', var_infector = 'infector_id', 
                      var_order = 'date_symptoms', verbose = FALSE) {
  order_id <- 1
  cases$order_id <- NA_integer_

	cases <- as.data.frame(cases)
  cases <- cases[order(cases[[var_order]]), ]

  index_cases <- cases[which(cases[[var_infector]] == "" | is.na(cases[[var_infector]])), var_id]
  for (case in index_cases) {
    out <- map_case(case_id = case,
										var_id = var_id,
									  var_infector = var_infector,
                    order_id = order_id,
                    cases = cases,
                    verbose = verbose)
    
    cases <- out$cases
    order_id <- out$order_id
  }

  cases <- cases[order(as.numeric(row.names(cases))), ]
  return(cases)
}


#' Map Secondaries of a Case
#' 
#' @description Internal function that calculates visual order for the secondaries of a single case.
#'
#' @param case_id `chr` Case to map.
#' @param var_infector `chr` Column name for infector id.
#' @param order_id `int` Case order in dataset.
#' @param cases `df` Data frame of case data.
#' @param var_id `chr` Column name for case id.
#' @param verbose `bool` Whether to print output on cases being mapped.
map_case <- function(case_id, order_id, cases, var_id = 'id', var_infector = 'infector_id',
                     verbose = FALSE) {
  if (verbose) {
    writeLines(paste('mapping case', case_id))
  }

  cases[cases[[var_id]] == case_id, 'order_id'] <- order_id
  order_id <- order_id + 1

  secondaries <- cases[cases[[var_infector]] == case_id, var_id]
  secondaries <- secondaries[which(!is.na(secondaries))]

  if (verbose) {
    writeLines(paste('secondaries:', paste0(secondaries, collapse = ' ')))
  }

  for (s in secondaries) {
    out <- map_case(case_id = s,
										var_id = var_id,
									  var_infector = var_infector,
                    order_id = order_id,
                    cases = cases,
                    verbose = verbose)
    
    cases <- out$cases
    order_id <- out$order_id
  }

  return(list(cases = cases,
              order_id = order_id))
}

