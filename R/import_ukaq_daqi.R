#' Import UK Daily Air Quality Index Data
#'
#' Imports [UK Daily Air Quality Index
#' (DAQI)](https://uk-air.defra.gov.uk/air-pollution/daqi) data for selected
#' pollutants, years, and sites. Returned information includes the relevant
#' daily statistic (e.g., daily average for PM10), the index (1:10) and the band
#' (Low, Moderate, High, or Very High).
#'
#' @param year *A year, or range of years, from which to import data.*
#'
#'    **required**
#'
#'   `year` expects a vector of integer values. It can be a single year
#'   (`2020`), a selection of specific years (c(`2020, 2021, 2022`)), or a range
#'   of years (`2020:2025`).
#'
#' @param code *Specific site codes to import.*
#'
#'    *default:* `NA`
#'
#'   When importing DAQI data, the default behaviour is for all sites in a given
#'   network to be imported. Specifying `code` will filter the result for
#'   specific sites based on their site codes, available through
#'   [import_ukaq_meta()].
#'
#' @param source *One or more UK Monitoring networks from which to import data.*
#'
#'   *default:* `"ukaq"`
#'
#'   The default, `"ukaq"`, will import data from the AURN, the four 'devolved'
#'   networks, and locally managed English monitoring networks. Any combination
#'   of `"aurn"`, `"aqe"`, `"saqn"`, `"waqn"`, and `"niaqn"` will
#'   *only* import data from those specific monitoring networks. Note that a
#'   mismatch between `code` and `source` may result in no data being imported.
#'
#' @param pollutant *One or more DAQI pollutants for which to import data.*
#'
#'  *default:* `r daqi_pollutant_names`
#'
#'   By default, all `r length(daqi_pollutant_names)` DAQI pollutants are
#'   imported by [import_ukaq_daqi()]. `pollutant` allows any specific
#'   combination of the DAQI pollutants to be returned instead.
#'
#' @param append_metadata *Append site metadata to the dataframe?*
#'
#'  *default:* `FALSE`
#'
#'   When `TRUE`, the resulting `data.frame` will have site metadata appended.
#'   The specific columns are selected using `metadata_columns`.
#'
#' @param metadata_columns *Specific metadata columns to append to the
#'   data.frame.*
#'
#'  *default:* `c("site_type", "latitude", "longitude")`
#'
#'   When `append_metadata = TRUE`, the columns selected here will be appended
#'   to the data. Columns names should match those in [import_ukaq_meta()],
#'   excluding pollutant-specific columns (e.g., ratification date).
#'
#' @param pivot *Should the dataframe be 'wide' or 'long'?*
#'
#'  *default:* `"long"`
#'
#'   There are two main ways to store air quality data; "long" (with 'pollutant'
#'   and 'value' columns) or "wide" (with each pollutant value being stored in
#'   its own column). `pivot` allows users to define which format they would
#'   prefer their data in.
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
#' @author Jack Davison, David Carslaw
#'
#' @references With thanks to Trevor Davies and [Ricardo
#'   Plc](https://www.ricardo.com/en) for preparing and hosting the data
#'
#' @seealso <https://uk-air.defra.gov.uk/air-pollution/daqi> for more
#'   information about the DAQI.
#'
#' @export
import_ukaq_daqi <-
  function(year,
           code = NULL,
           source = "ukaq",
           pollutant = c("no2", "o3", "pm10", "pm2.5", "so2"),
           append_metadata = FALSE,
           metadata_columns = c("site_type", "latitude", "longitude"),
           pivot = "long",
           ...,
           .return = NULL) {
    pollutant <-
      rlang::arg_match(pollutant, daqi_pollutant_names, multiple = TRUE)
    pivot <- rlang::arg_match(pivot, c("wide", "long"))
    metadata_columns <-
      rlang::arg_match(metadata_columns, metadata_column_names, multiple = TRUE)

    # load daqi data
    daqi <-
      loadRDS(
        paste0(
          "https://uk-air.defra.gov.uk/openair/R_data/annual_DAQI_AURN_",
          year,
          ".rds"
        )
      )

    # deal with data types & names
    names(daqi) <- tolower(names(daqi))
    daqi$date <- as.Date(as.character(daqi$date), tz = "GMT")
    daqi$code <- as.character(daqi$code)
    daqi$site <- as.character(daqi$site)
    daqi$pollutant <- as.character(daqi$pollutant)
    daqi$measurement_period <- as.character(daqi$measurement_period)

    # create poll band column
    daqi$poll_band <- ifelse(
      daqi$poll_index %in% 1:3,
      "Low",
      ifelse(
        daqi$poll_index %in% 4:6,
        "Moderate",
        ifelse(daqi$poll_index %in% 7:9, "High", "Very High")
      )
    )
    daqi$poll_band <- factor(daqi$poll_band, c("Low", "Moderate", "High", "Very High"))
    daqi <- daqi[, c(names(daqi)[names(daqi) != "measurement_period"], "measurement_period")]

    # filter for site/pollutant
    daqi <- daqi[daqi$pollutant %in% pollutant, ]
    if (!is.null(code)) {
      daqi <- daqi[daqi$code %in% code, ]
    }

    # pivot, if required
    if (pivot == "wide") {
      names(daqi)[names(daqi) == "concentration"] <- "value"
      names(daqi)[names(daqi) == "poll_index"] <- "index"
      names(daqi)[names(daqi) == "poll_band"] <- "band"

      daqi <-
        reshape(
          as.data.frame(daqi)[c("date",
                                "code",
                                "site",
                                "pollutant",
                                "value",
                                "index",
                                "band")],
          timevar = "pollutant",
          idvar = c("date", "code", "site"),
          direction = "wide",
          v.names = c("value", "index", "band"),
          sep = "_"
        )

      names(daqi) <- gsub("value_", "", names(daqi))

      grid <- expand.grid(a = c("index", "band"), b = daqi_pollutant_names)
      old <- paste(grid$a, grid$b, sep = "_")
      new <- paste(grid$b, grid$a, sep = "_")

      for (i in seq_along(old)) {
        names(daqi)[names(daqi) == old[i]] <- new[i]
      }
    }

    # append metadata, if requested
    if (append_metadata) {
      daqi$source <- "AURN"
      meta <- import_ukaq_meta(source = source, by_pollutant = FALSE)
      meta <- meta[c(metadata_columns, "code", "source")]
      daqi <- merge(daqi, meta, by = c("code", "source"))
    }

    daqi <- daqi[order(daqi$site, daqi$date), ]

    return(tbl(daqi, .return))
  }
