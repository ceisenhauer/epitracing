pairs <- rio::import(here::here('data', 'example_pairs.rda')) |>
  dplyr::rename(id = infectee,
                infector_id = infector)

cases <- rio::import(here::here('data', 'example_cases.rda')) |>
  dplyr::left_join(pairs)

cases$order_id <- NA_integer_
order_id <- 1

plot_chains_linear <- function(cases, pairs, height = 5, gap = height / 2, offset = 1) {
  tmp <- map_cases(cases, pairs)
  tmp <- tmp |>
    arrange(order_id) |>
    mutate(
      # x values
      across(starts_with('date'), as.Date),
      x_1 = date_symptoms,
      x_2 = date_hospital,
      x_3 = date_outcome,
      # y values
      y_1 = (nrow(tmp) - row_number() + 1) * (height + gap),
      y_2 = y_1 - height,
      y_mid = y_1 - height / 2,
      # labels
      label = paste0('  ', id))

  # add connections
  tmp <- tmp |>
    filter(id %in% unique(infector_id)) |>
    mutate(horizontal_1 = x_1 + offset,
           vertical_1 = y_2) |>
    select(infector_id = id, vertical_1, horizontal_1) |>
    right_join(pairs) |>
    right_join(tmp) |>
    mutate(horizontal_2 = case_when(!is.na(horizontal_1) ~ x_1),
           vertical_2 = case_when(!is.na(vertical_1) ~ y_mid))
    

  line_color <- 'black'
  fill_infection <- epiplots::colors$grey_lighter

  tmp |>
    ggplot() +
    geom_rect(aes(xmin = x_1,
                  xmax = x_2,
                  ymin = y_1,
                  ymax = y_2),
              fill = fill_infection,
              color = line_color) +
    geom_rect(aes(xmin = x_2,
                  xmax = x_3,
                  ymin = y_1,
                  ymax = y_2,
                  fill = outcome),
              color = line_color) +
    geom_segment(aes(x = horizontal_1,
                     xend = horizontal_2,
                     y = vertical_2,
                     yend = vertical_2),
                 color = line_color) +
    geom_segment(aes(x = horizontal_1,
                     xend = horizontal_1,
                     y = vertical_1,
                     yend = vertical_2),
                 color = line_color) +
    geom_text(aes(x = x_1,
                  y = y_mid,
                  label = label),
              hjust = 0) +
    epiplots::date_axis(date_breaks = '1 week') +
    scale_fill_manual(name = '',
                      values = c('black', epiplots::colors$blue_light)) +
    epiplots::theme_clean() +
    theme(axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank())

}
