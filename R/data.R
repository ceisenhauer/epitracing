#' Example Case Data
#' 
#' An example (minimal) linelist of individual case data. This data is fake and is not intended 
#' to represent any particular pathogen.
#'
#' @format
#' A data frame with 13 rows and 6 columns:
#' \describe{
#'   \item{id}{Case id}
#'   \item{adm3}{Administrative district}
#'   \item{outcome}{Exit status}
#'   \item{date_symptoms}{Date of symptom onset}
#'   \item{date_hospital}{Date of hospitalization}
#'   \item{date_outcome}{Date of case outcome (discharged or died)}
#' }
'example_cases'


#' Example Infector-Infectee Pair Data
#' 
#' An example (minimal) dataset of infector-infectee pairs corresponding to `casees`. 
#'
#' @format
#' A data frame with 13 rows and 2 columns:
#' \describe{
#'   \item{id}{Case id}
#'   \item{infector_id}{Infector id}
#' }
'example_pairs'
