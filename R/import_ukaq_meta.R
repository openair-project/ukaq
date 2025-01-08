#' Import UK Air Quality Metadata
#'
#' [import_ukaq_meta()] imports metadata for UK air quality monitoring sites. By
#' default this function returns one row per monitoring site, including the site
#' opening and closing dates and other useful meta-data. The `by_pollutant`
#' option will return one row per monitoring site *and pollutant*; see
#' [import_ukaq_pollutants()] for definitions. The `"site"` column can be used
#' in other functions to import data.
#'
#' @param source *One or more UK Monitoring networks from which to import
#'   metadata*
#'
#'    *default:* `"ukaq"`
#'
#'   The default, `"ukaq"`, will import metadata from the AURN, the four
#'   'devolved' networks, and locally managed English monitoring networks. Any
#'   combination of `"aurn"`, `"aqe"`, `"saqn"`, `"waqn"`, `"niaqn"` and ,
#'   `"lmam"` will *only* import data from those specific monitoring networks.
#'
#' @param year *A year, or range of years, with which to filter data.*
#'
#'    *default:* `NA`
#'
#'   By default all monitoring stations are returned regardless of their
#'   open/closed status. `year` allows users to filter sites open in a specific
#'   year, or over a range of years. For example, `year = c(2010, 2020)` (or
#'   `year = 2010:2020`) will return sites that were open between 2010 and 2020.
#'
#' @param by_pollutant *Return site-pollutant combinations?*
#'
#'    *default:* `FALSE`
#'
#'   If `TRUE`, the rows of the resulting `data.frame` will each represent a
#'   monitoring site-pollutant pair, with the start, end, and ratification date
#'   reflecting each specific pollutant. When `FALSE`, the default, each row is
#'   a unique monitoring site, and the start and end dates represent the opening
#'   and closing dates of the site as a whole.
#'
#' @param ... *Not used.*
#'
#' @param .return *Signifier for the dataframe class.*
#'
#'    *default:* `NULL`
#'
#'   [ukaq][ukaq-package] functions, by default, will return `tbl_df`s if the
#'   [tibble][tibble::tibble-package] package is installed, but will otherwise
#'   return `data.frame`s. `.return` can override this behaviour, and takes
#'   either `"tbl"` or `"df"`, which sets the return class to be `tbl_df` or
#'   `data.frame` respectively.
#'
#' @return a `data.frame`
#'
#' @rdname import_ukaq_meta
#' @order 1
#'
#' @author Jack Davison, David Carslaw
#'
#' @references With thanks to Trevor Davies and [Ricardo
#'   Plc](https://www.ricardo.com/en) for preparing and hosting the data
#'
#' @export
import_ukaq_meta <-
  function(source = "ukaq",
           year = NA,
           by_pollutant = FALSE,
           ...,
           .return = NULL) {
    rlang::check_dots_empty()

    # read aurn metadata
    aurn_meta <-
      load_file("https://uk-air.defra.gov.uk/openair/R_data/AURN_metadata.RData")
    names(aurn_meta) <- tolower(names(aurn_meta))

    # rename columns
    dict <-
      list(
        "site" = "site_id",
        "site_type" = "location_type",
        "pollutant" = "parameter"
      )

    for (i in seq_along(dict)) {
      names(aurn_meta)[names(aurn_meta) == dict[[i]]] <- names(dict)[[i]]
    }

    # drop parameter name
    aurn_meta$parameter_name <- NULL

    # deal with dates
    aurn_meta$end_date[aurn_meta$end_date == "ongoing"] <- NA
    aurn_meta$ratified_to[aurn_meta$ratified_to == "Never"] <- NA
    aurn_meta$start_date <- as.Date(aurn_meta$start_date, tz = "GMT")
    aurn_meta$end_date <- as.Date(aurn_meta$end_date, tz = "GMT")
    aurn_meta$ratified_to <-
      as.Date(aurn_meta$ratified_to, tz = "GMT")

    # create zagglom column
    aurn_meta$zagglom <-
      coalesce(aurn_meta$agglomeration, aurn_meta$zone)
    aurn_meta <- aurn_meta[, c(
      "code",
      "site",
      "site_type",
      "latitude",
      "longitude",
      "pollutant",
      "start_date",
      "end_date",
      "ratified_to",
      "zone",
      "agglomeration",
      "zagglom",
      "local_authority"
    )]

    if (!by_pollutant) {
      newtbl <-
        do.call(rbind, lapply(split(aurn_meta, aurn_meta$site_name), function(df) {
          df[1, "start_date"] <- min(df$start_date)
          df[1, "end_date"] <- max(df$end_date)
          df <- df[1, ]
          df$ratified_to <- df$pollutant <- NULL
          df
        }))

      # Drop unused columns
      newtbl$pollutant <- newtbl$ratified_to <- NULL
      aurn_meta <- newtbl
    }

    if (!anyNA(year)) {
      aurn_meta$start_year <- as.integer(format(aurn_meta$start_date, "%Y"))
      aurn_meta$end_year <- as.integer(format(aurn_meta$end_date, "%Y"))
      aurn_meta$end_year[is.na(aurn_meta$end_year)] <-
        as.integer(format(Sys.Date(), "%Y"))

      aurn_meta <-
        aurn_meta[aurn_meta$start_year <= min(year) &
                    aurn_meta$end_year >= max(year),]

      aurn_meta$start_year <- aurn_meta$end_year <- NULL
    }

    return(tbl(aurn_meta, .return))
  }

#' @rdname import_ukaq_meta
#' @order 2
#' @export
import_ukaq_pollutants <-
  function(source = "ukaq",
           ...,
           .return = NULL) {
    aurn_meta <-
      load_file("https://uk-air.defra.gov.uk/openair/R_data/AURN_metadata.RData")

    meta <- unique(aurn_meta[c("parameter", "Parameter_name")])

    names(meta) <- c("pollutant", "pollutant_name")

    return(tbl(meta, .return))
  }

