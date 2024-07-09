#' Plot Linear Chains (ggplot2)
#'
#' @description Plots transmission trees with a linear layout, showing infections and case
#' progression over time. By default cases are ordered by date of symptom onset, starting from
#' top left to bottom right. **Note** the function expects case data to have a column indicating the
#' id of the infector, if your pairs are stored in a separate dataset, you must join them first.
#'
#' @param cases `df` Dataframe of case information. This dataset should contain information on 
#'   outcome, onset date, transition (hospitalization) date, and outcome date, and infector.
#' @param pairs `df` Dataframe with infector infectee pairs.
#' @param height `num` Height of bars representing cases.
#' @param gap `num` Gap between bars.
#' @param offset `num` Offset between the beginning of a bar and the link connecting it to any
#'   secondary cases.
#' @param var_id `chr` Column name for case id.
#' @param var_infector `chr` Column name for infector id.
#' @param var_order `chr` Column name for the variable used to visually order cases.
#' @param var_outcome_status `chr` Column name for outcome status.
#' @param var_symptoms `chr` Column name for symptom onset date.
#' @param var_transition `chr` Column name for transition (hospitalization) date. 
#' @param var_outcome `chr` Colun name for outcome date.
#' @param fill_infected `chr` Color to use for initial infected period of the bar.
#' @param fills_outcome `chr vct` Colors to use for the second part of the bar, indicating outcome.
#' @param color_links `chr` Color for lines linking primary and secondary cases.
#' @param font_face `chr` Font face of id labels.
#' @param font_size `chr` Text size of id labels.
#' @param font_color `chr` Text color of id labels.
#' @param date_breaks `chr` Breaks to use for date axis.
#' @param verbose `bool` Whether to print output on cases being mapped.
#' 
#' @examples
#' example_cases
#' example_pairs
#' plot_chains_linear(example_cases, example_pairs)
#'
#' @importFrom dplyr across arrange mutate row_number filter select right_join any_of case_when
#' @importFrom epiplots date_axis theme_clean
#' @importFrom ggplot2 ggplot geom_rect aes geom_segment geom_text
#' @importFrom rlang .data
#' @export
plot_chains_linear <- function(
  cases, pairs, height = 5, gap = height / 2, offset = 1, 
  var_id = 'id', var_infector = 'infector_id', var_order = var_symptoms, 
  var_outcome_status = 'outcome',
  var_symptoms = 'date_symptoms', var_transition = 'date_hospital', var_outcome = 'date_outcome', 
  fill_infected = '#EEEEEE', fills_outcome = c('#000000', '#819CBB'), color_links = '#000000',
  font_face = 'bold', font_size = 5, font_color = '#000000',
  date_breaks = '1 week', verbose = FALSE) {

  tmp <- map_cases(cases,
                   pairs,
                   var_id = var_id,
                   var_infector = var_infector,
                   var_order = var_order,
                   verbose = verbose)

  tmp <- tmp |>
    dplyr::arrange(.data$order_id) |>
    dplyr::mutate(
      # x values
      dplyr::across(c(.data[[var_symptoms]], .data[[var_transition]], .data[[var_outcome]]),
                      as.Date),
      x_1 = .data[[var_symptoms]],
      x_2 = .data[[var_transition]],
      x_3 = .data[[var_outcome]],

      # y values
      y_1 = (nrow(tmp) - dplyr::row_number() + 1) * (height + gap),
      y_2 = .data$y_1 - height,
      y_mid = .data$y_1 - height / 2,

      # labels
      label = paste0('  ', .data[[var_id]]))

  # add connections
  #pairs <- cases[ , c(var_id, var_infector)]

  tmp <- tmp |>
    dplyr::filter(.data[[var_id]] %in% unique(pairs[[var_infector]])) |>
    dplyr::mutate(horizontal_1 = .data$x_1 + offset,
                  vertical_1 = .data$y_2) |>
    dplyr::select('{var_infector}' := dplyr::any_of(var_id), 'vertical_1', 'horizontal_1') |>
    dplyr::right_join(pairs, relationship = 'many-to-many') |>
    dplyr::right_join(tmp) |>
    dplyr::mutate(horizontal_2 = dplyr::case_when(!is.na(.data$horizontal_1) ~ .data$x_1),
                  vertical_2 = dplyr::case_when(!is.na(.data$vertical_1) ~ .data$y_mid))
    

  tmp |>
    ggplot2::ggplot() +
    ggplot2::geom_segment(ggplot2::aes(x = .data$horizontal_1,
                                       xend = .data$horizontal_2,
                                       y = .data$vertical_2,
                                       yend = .data$vertical_2),
                          alpha = 0.5,
                          color = color_links) +
    ggplot2::geom_segment(ggplot2::aes(x = .data$horizontal_1,
                                       xend = .data$horizontal_1,
                                       y = .data$vertical_1,
                                       yend = .data$vertical_2),
                          alpha = 0.5,
                          color = color_links) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$x_1,
                                    xmax = .data$x_2,
                                    ymin = .data$y_1,
                                    ymax = .data$y_2),
                       fill = fill_infected,
                       color = color_links) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$x_2,
                                    xmax = .data$x_3,
                                    ymin = .data$y_1,
                                    ymax = .data$y_2,
                                    fill = .data[[var_outcome_status]]),
                       color = color_links) +
    ggplot2::geom_text(ggplot2::aes(x = .data$x_1,
                                    y = .data$y_mid,
                                    label = .data$label),
                       fontface = font_face,
                       color = font_color,
                       size = font_size,
                       hjust = 0) +
    epiplots::date_axis(date_breaks = date_breaks) +
    ggplot2::scale_fill_manual(name = '',
                               values = fills_outcome) +
    epiplots::theme_clean() +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank())

}
