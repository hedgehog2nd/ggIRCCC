#' demo data
#'
#' This dataset contains simulated responses from 100 individuals on 10 items,
#' each rated on a 5-point likert scale (1 to 5). The data were generated using
#' a graded response model with item discrimination parameters ranging from 0.1 to 3,
#' and difficulty thresholds increasing monotonically for each item.
#'
#' This is a demonstration data. Any resemblance to actual persons, living or dead,
#' is purely coincidental.
#'
#' @format A data frame with 200 rows and 10 variables:
#' \describe{
#'   \item{Item_1}{Response to item 1}
#'   \item{Item_2}{Response to item 2}
#'   \item{Item_3}{Response to item 3}
#'   \item{Item_4}{Response to item 4}
#'   \item{Item_5}{Response to item 5}
#'   \item{Item_6}{Response to item 6}
#'   \item{Item_7}{Response to item 7}
#'   \item{Item_8}{Response to item 8}
#'   \item{Item_9}{Response to item 9}
#'   \item{Item_10}{Response to item 10}
#' }
#' @usage data(demo_data)
#' @examples
#' data(demo_data)
#' head(demo_data)
"demo_data"
