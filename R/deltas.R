#' CRU-based climatologies and delta change
#'
#' Compute CRU-based monthly or seasonal climatologies using a specific historical window.
#' Use the climatologies to compute delta change from raw climate values.
#'
#' \code{cru_clim} is used internally by \code{deltas}. It does not need to be used directly in this case.
#'
#' @param x a data frame such as returned using the \code{snapclim} package.
#' @param limits numeric, length-2 vector giving the range of years in the climatological window.
#'
#' @return a data frame.
#' @name deltas
#'
#' @examples
#' \dontshow{library(dplyr)}
#' library(snapclim)
#' library(dplyr)
#' x <- climdata("ar5stats", "Anchorage") %>%
#'   filter(Var %in% c("pr", "tas"))
#' cru_clim(x)
#' deltas(x)
NULL

#' @rdname deltas
#' @export
cru_clim <- function(x, limits = c(1960, 1989)){
  x <- dplyr::filter(x, .data[["Model"]] == "CRU 4.0" & .data[["Year"]] >= limits[1] & .data[["Year"]] <= limits[2])
  if("Month" %in% names(x)) x <- dplyr::group_by(x, .data[["Var"]], .data[["Month"]])
  if("Season" %in% names(x)) x <- dplyr::group_by(x, .data[["Var"]], .data[["Season"]])
  dplyr::summarise(x, ClimMean = mean(.data[["Mean"]]))
}

#' @rdname deltas
#' @export
deltas <- function(x, limits = c(1960, 1989)){
  suppressMessages(dplyr::left_join(x, cru_clim(x, limits))) %>%
    dplyr::mutate(Mean = ifelse(.data[["Var"]] == "pr",
                                .data[["Mean"]] / .data[["ClimMean"]],
                                .data[["Mean"]] - .data[["ClimMean"]])) %>%
    dplyr::select(-.data[["ClimMean"]])
}
