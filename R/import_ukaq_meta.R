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

    if (any(source == "ukaq")) {
      source <- ukaq_network_names
    } else {
      source <- rlang::arg_match(source, ukaq_network_names, multiple = TRUE)
    }

    meta <-
      lapply(source, function(x) {
        df <- loadRData(meta_url(x))
        df$source <- toupper(x)
        if (x != "lmam") {
          df$provider <- df$pcode <- NA_character_
        }
        if (x == "lmam") {
          df$ratified_to <- as.Date(NA, tz = "GMT")
          df$local_authority <- NA_character_
        }
        return(df)
      })

    meta <- do.call(rbind, meta)

    names(meta) <- tolower(names(meta))

    # rename columns
    dict <-
      list(
        "code" = "site_id",
        "site" = "site_name",
        "site_type" = "location_type",
        "pollutant" = "parameter",
        "lmam_provider" = "provider",
        "lmam_code" = "pcode"
      )

    for (i in seq_along(dict)) {
      names(meta)[names(meta) == dict[[i]]] <- names(dict)[[i]]
    }

    # drop parameter name
    meta$parameter_name <- NULL

    # deal with dates
    meta$end_date[meta$end_date == "ongoing"] <- NA
    meta$start_date <- as.Date(meta$start_date, tz = "GMT")
    meta$end_date <- as.Date(meta$end_date, tz = "GMT")
    if (any(!is.na(meta$ratified_to))) {
      meta$ratified_to[meta$ratified_to == "Never"] <- NA
      meta$ratified_to <- as.Date(meta$ratified_to, tz = "GMT")
    }

    # create zagglom column
    meta$zagglom <-
      coalesce(meta$agglomeration, meta$zone)

    meta <- meta[, c(
      "source",
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
      "local_authority",
      "lmam_provider",
      "lmam_code"
    )]

    if (!by_pollutant) {
      newtbl <-
        do.call(rbind, lapply(split(meta, meta$site), function(df) {
          df[1, "start_date"] <- min(df$start_date)
          df[1, "end_date"] <- max(df$end_date)
          df <- df[1, ]
          df$ratified_to <- df$pollutant <- NULL
          df
        }))

      # Drop unused columns
      newtbl$pollutant <- newtbl$ratified_to <- NULL
      meta <- newtbl
    }

    if (!anyNA(year)) {
      meta$start_year <- as.integer(format(meta$start_date, "%Y"))
      meta$end_year <- as.integer(format(meta$end_date, "%Y"))
      meta$end_year[is.na(meta$end_year)] <-
        as.integer(format(Sys.Date(), "%Y"))

      meta <-
        meta[meta$start_year <= min(year) &
                    meta$end_year >= max(year),]

      meta$start_year <- meta$end_year <- NULL
    }

    return(tbl(meta, .return))
  }

#' @rdname import_ukaq_meta
#' @order 2
#' @export
import_ukaq_pollutants <-
  function(source = "ukaq",
           ...,
           .return = NULL) {
    rlang::check_dots_empty()

    if (any(source == "ukaq")) {
      source <- ukaq_network_names
    } else {
      source <- rlang::arg_match(source, ukaq_network_names, multiple = TRUE)
    }

    meta <-
      lapply(source, function(x) {
        df <- loadRData(meta_url(x))
        df$source <- toupper(x)
        if (x != "lmam") {
          df$provider <- df$pcode <- NA_character_
        }
        if (x == "lmam") {
          df$ratified_to <- as.Date(NA, tz = "GMT")
          df$local_authority <- NA_character_
        }
        return(df)
      })

    meta <- do.call(rbind, meta)

    meta <- unique(meta[c("parameter", "Parameter_name")])
    names(meta) <- c("pollutant", "pollutant_name")

    return(tbl(meta, .return))
  }

meta_url <- function(source){
  switch(source,
         aurn = "https://uk-air.defra.gov.uk/openair/R_data/AURN_metadata.RData",
         saqn = "https://www.scottishairquality.scot/openair/R_data/SCOT_metadata.RData",
         niaqn = "https://www.airqualityni.co.uk/openair/R_data/NI_metadata.RData",
         waqn = "https://airquality.gov.wales/sites/default/files/openair/R_data/WAQ_metadata.RData",
         aqe = "https://airqualityengland.co.uk/assets/openair/R_data/AQE_metadata.RData",
         lmam = "https://uk-air.defra.gov.uk/openair/LMAM/R_data/LMAM_metadata.RData"
  )
}
