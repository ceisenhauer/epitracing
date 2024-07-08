#library(dplyr)
#library(openxlsx)


#colors <- epiplots::colors

#fn <- 'example_trees.xlsx'


#gap <- 1
#offset <- 1


#pairs <- rio::import(here::here('data', 'example_pairs.rda')) |>
  #dplyr::rename(id = infectee,
                #infector_id = infector)

#cases <- rio::import(here::here('data', 'example_cases.rda')) |>
  #dplyr::mutate(across(starts_with('date'), as.Date)) |>
  #dplyr::left_join(pairs)


#tmp <- map_cases(cases, pairs)

#date_min <- as.Date(min(tmp$date_symptoms, na.rm = TRUE))
#date_max <- as.Date(max(tmp$date_outcome, na.rm = TRUE))

#dates <- seq.Date(from = date_min,
                  #to = date_max, 
                  #by = 1)

  #mutate(
    ## x values
    #across(starts_with('date'), as.Date),
    #x_1 = date_symptoms,
    #x_2 = date_hospital,
    #x_3 = date_outcome,
    ## y values
    #y_1 = (nrow(tmp) - row_number() + 1) * (height + gap),
    #y_2 = y_1 - height,
    #y_mid = y_1 - height / 2,
    ## labels
    #label = paste0('  ', id))


#tmp <- tmp |>
  #mutate(row = order_id * 2 + 1,
         #col_id = match(date_symptoms, dates),
         #col_hospital = match(date_hospital, dates),
         #col_outcome = match(date_outcome, dates) - 1)
  
#out <- matrix(NA,
              #byrow = TRUE,
              #nrow = nrow(tmp) * 2 + 1,
              #ncol = length(dates))

##out[1, ] <- as.character(dates)
#out[tmp$row, tmp$col_id] <- tmp$id

#Matrix::sparseMatrix(i = tmp$row, j = tmp$col_id, x = tmp$id)

