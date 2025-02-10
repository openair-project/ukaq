
#' Get palettes of colours related to UK Air Quality legislation
#'
#' These functions are convenient ways to obtain vectors of colours for charts
#' and tables. [palette_daqi] has options `"index"` or `"bands"` and returns the
#' colours used for the DAQI on [UK
#' AIR](https://uk-air.defra.gov.uk/air-pollution/daqi). [palette_gaf] has
#' options `"categorical"`, `"duo"`, `"focus"`, and `"sequential"` and returns
#' the suggested colour palettes by the [UK Government Analysis
#' Function](https://analysisfunction.civilservice.gov.uk/policy-store/data-visualisation-colours-in-charts/).
#'
#' @param option *The palette of interest.*
#'
#'   One of the following palette options:
#'
#'   - **daqi:** `"index"` (the default) or `"band"`
#'   - **gaf:** `"categorical"` (the default), `"duo"`, `"focus"`, or `"sequential"`
#'
#' @param named *Return a named vector?*
#'
#'   Defaults to `FALSE`, which will return an unnamed vector. Named vectors may
#'   be of interest if you need to index specific colours rather than use the
#'   whole palette. The specific names vary with the specific function and
#'   `option`.
#'
#' @seealso <https://uk-air.defra.gov.uk/air-pollution/daqi>
#'
#' @seealso
#' <https://analysisfunction.civilservice.gov.uk/policy-store/data-visualisation-colours-in-charts/>
#'
#' @rdname ukaq-colours
#' @order 1
#' @export
#'
#' @examples
#' \dontrun{
#' # Preview the colours with the 'scales' package
#'
#' palette_daqi() |> scales::show_col()
#'
#' palette_gaf() |> scales::show_col()
#' }
palette_daqi <- function(option = c("index", "bands"),
                         named = FALSE) {
  option <- rlang::arg_match(option)

  pals <- list("index" = stats::setNames(
    list(
      "#9CFF9C",
      "#31FF00",
      "#31CF00",
      "#FFFF00",
      "#FFCF00",
      "#FF9A00",
      "#FF6464",
      "#FF0000",
      "#990000",
      "#CE30FF"
    ),
    1:10
  ),
  "bands" = stats::setNames(
    list("#009900", "#ff9900", "#ff0000", "#990099"),
    c("Low", "Moderate", "High", "Very High")
  ))

  pal <- pals[[option]]

  pal <- unlist(pal, use.names = named)

  return(pal)
}

#' @rdname ukaq-colours
#' @order 2
#' @export
palette_gaf <- function(option = c("categorical", "duo", "focus", "sequential"),
                        named = FALSE) {
  option <- rlang::arg_match(option)

  pals <- list(
    "categorical" = list(
      "blue" = "#12436D",
      "turquoise" = "#28A197",
      "pink" = "#801650",
      "orange" = "#F46A25",
      "grey" = "#3D3D3D",
      "purple" = "#A285D1"
    ),
    "duo" = list("blue" = "#12436D", "orange" = "#F46A25"),
    "focus" = list("blue" = "#12436D", "grey" = "#BFBFBF"),
    "sequential" = list(
      "darkblue" = "#12436D",
      "midblue" = "#2073BC",
      "lightblue" = "#6BACE6"
    )
  )

  pal <- pals[[option]]

  pal <- unlist(pal, use.names = named)

  return(pal)
}
