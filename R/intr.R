#' @noRd
bin <- function(first, last, length) {

  diff <- as.integer(last - first)

  n_int <-
    ifelse(
      diff == 0L,
      1L,
      as.integer(ceiling(diff / length))
    )

  start_bin <- first + lubridate::days(length) * 0:(n_int - 1)

  end_bin <- start_bin + lubridate::days(length - 1)

  interval <- start_bin %--% end_bin

  start <- as.integer(start_bin - start_bin[[1]])

  end <- as.integer(end_bin - start_bin[[1]])

  end_chr <- paste("t", end, sep = "_")

  list(
    tbl = tibble::tibble(interval, start, end, end_chr),
    n_int = n_int
  )

}


#' Expand a time-to-event dataset to a discrete time dataset
#'
#' @param data a time-to-event data
#' @param id the name of an ID variable
#' @param first the name of a variable for when a participant enters the cohort
#' @param last the name of a variable for when a participant exits the cohort
#' @param length an integer for the length of a bin
#'
#' @return
#' @import dplyr
#' @import purrr
#' @import tidyselect
#' @importFrom tibble tibble
#' @importFrom lubridate %--%
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @export
#'
#' @examples
intervalize <- function(data, id, first, last, length) {

  binned_list <-
    data %>%
    dplyr::group_by(!!rlang::ensym(id)) %>%
    dplyr::group_map(
      ~ bin(dplyr::pull(.x, {{ first }}), dplyr::pull(.x, {{ last }}), length = length)
    )

  binned_tbl <-
    binned_list %>%
    purrr::map(~ .x[["tbl"]]) %>%
    dplyr::bind_rows()

  n_int <-
    binned_list %>%
    purrr::map_int(~ .x[["n_int"]])

  id_aug <-
    purrr::map2(dplyr::pull(data, {{ id }}), n_int, ~ rep(.x, .y)) %>%
    unlist()

  binned_tbl %>%
    dplyr::mutate({{ id }} := id_aug) %>%
    dplyr::select({{ id }}, tidyselect::everything())

}
