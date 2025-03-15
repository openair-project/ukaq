#' Import UK Daily Air Quality Index Data
#'
#' Imports [UK Daily Air Quality Index
#' (DAQI)](https://uk-air.defra.gov.uk/air-pollution/daqi) data for selected
#' pollutants, years, and sites. Returned information can include the relevant
#' daily statistic (e.g., daily average for PM10), the index (1:10), the band
#' (Low, Moderate, High, or Very High), and the colour associated with the index
#' and/or band.
#'
#' @param source *One or more UK Monitoring networks from which to import data.*
#'
#'   *default:* `"ukaq"`
#'
#'   The default, `"ukaq"`, will look for sites from any of the available
#'   networks. Any combination of `"aurn"`, `"aqe"`, `"saqn"`, `"waqn"`, or
#'   `"niaqn"` will *only* import data from those specific monitoring networks.
#'   Note that a mismatch between `code` and `source` may result in no data
#'   being imported. The DAQI is not pre-calculated for `"lmam"` sites.
#'
#' @param pollutant *One or more DAQI pollutants for which to import data.*
#'
#'  *default:* `NULL`
#'
#'   By default, all available pollutants are imported. `pollutant` allows any
#'   specific combination of the pollutants to be returned instead. Note that
#'   this should be a DAQI pollutant - i.e., one or more of `"no2"`, `"pm10"`,
#'   `"pm2.5"`, `"o3"` or `"so2"`.
#'
#' @param daqi_columns *Specific DAQI columns to include.*
#'
#'  *default:* `c("concentration", "poll_index", "poll_band")`
#'
#'   There are numerous DAQI-related data which can be returned within the
#'   `data.frame`. This option expects a vector of any combination of:
#'
#'   - `"concentration"`: The measured daily concentration.
#'
#'   - `"poll_index"`: The daily pollution index (1-10).
#'
#'   - `"poll_band"`: The pollution band (Low, Moderate, High, Very High).
#'
#'   - `"colour_index"`: The colour associated with the pollution index.
#'
#'   - `"colour_band"`: The colour associated with the pollution band.
#'
#'   - `"measurement_period"`: The name of the statistic presented in `"concentration"`.
#'
#' @inheritParams import_ukaq_summaries
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
  function(
    code = NULL,
    year,
    source = "aurn",
    pollutant = NULL,
    append_metadata = FALSE,
    metadata_columns = c("site_type", "latitude", "longitude"),
    daqi_columns = c(
      "concentration",
      "poll_index",
      "poll_band"
    ),
    pivot = "long",
    progress = NA,
    ...,
    .class = NULL
  ) {
    rlang::check_dots_empty()

    if (!is.null(pollutant)) {
      pollutant <- rlang::arg_match(
        pollutant,
        daqi_pollutant_names,
        multiple = TRUE
      )
    }
    pivot <- match_pivot(pivot)
    metadata_columns <-
      rlang::arg_match(metadata_columns, metadata_column_names, multiple = TRUE)
    source <-
      match_source(source = source, network_names = ukaq_network_names_nolocal)
    daqi_columns <-
      rlang::arg_match(daqi_columns, daqi_column_opts, multiple = TRUE)

    # import data
    daqi <- importDAQI(year = year, source = source, progress = progress)

    # format
    daqi <-
      formatDAQI(
        daqi = daqi,
        pollutant = pollutant,
        code = code,
        pivot = pivot,
        append_metadata = append_metadata,
        metadata_columns = metadata_columns,
        daqi_columns = daqi_columns
      )

    return(tbl_out(daqi, .class))
  }

#' Import DAQI data
#' @noRd
importDAQI <- function(year, source, progress) {
  grid <-
    expand.grid(year = year, source = source, stringsAsFactors = FALSE)

  # deal w/ progress opt if NA
  if (is.na(progress)) {
    progress <- FALSE
    if (nrow(grid) > 1L && rlang::is_interactive()) {
      progress <- TRUE
    }
  }

  if (progress) {
    pb <- pb_init(name = "Importing DAQI", x = nrow(grid))
    on.exit(pb_close(pb))
  }

  # load daqi data
  daqi <-
    lapply(1:nrow(grid), function(x) {
      df <- tryCatch(
        suppressWarnings(loadRDS(daqi_url(
          grid$source[x],
          grid$year[x]
        ))),
        error = function(e) {
          NULL
        }
      )
      if (!is.null(df)) {
        df$source <- grid$source[x]
        df <- df[, c("source", names(df)[names(df) != "source"])]
      }
      if (progress) {
        pb_increment(pb, x = x)
      }
      return(df)
    })

  daqi <- do.call(rbind, daqi)

  # abort if there's no data
  if (is.null(daqi)) {
    stop("No data has been returned.")
  }

  return(daqi)
}

#' Format DAQI data
#' @noRd
formatDAQI <- function(
  daqi,
  pollutant,
  code,
  pivot,
  append_metadata,
  metadata_columns,
  daqi_columns
) {
  # deal with data types & names
  names(daqi) <- tolower(names(daqi))
  daqi$date <- as.Date(as.character(daqi$date), tz = "GMT")

  # remove factors
  daqi <- factor_to_char(daqi)

  # create poll band column
  if ("poll_band" %in% daqi_columns) {
    daqi <- append_daqi_bands(daqi)
  }

  # filter for site/pollutant
  if (!is.null(pollutant)) {
    daqi <- daqi[tolower(daqi$pollutant) %in% tolower(pollutant), ]
  }

  if (!is.null(code)) {
    daqi <- daqi[tolower(daqi$code) %in% tolower(code), ]
  }

  # assign colours
  if ("colour_index" %in% daqi_columns) {
    daqi$colour_index <- daqi_index_cols[daqi$poll_index]
  }
  if ("colour_band" %in% daqi_columns) {
    daqi$colour_band <- daqi_band_cols[daqi$poll_band]
  }

  # deal with daqi column selection
  for (i in setdiff(daqi_column_opts, daqi_columns)) {
    daqi[i] <- NULL
  }

  # pivot, if required
  if (pivot == "wide") {
    daqi <- pivotDAQI(daqi, opts = daqi_columns)
  }

  # append metadata, if requested
  if (append_metadata) {
    daqi <-
      append_metadata_cols(
        daqi,
        source = source,
        metadata_columns = metadata_columns
      )
  }

  # order by site/date
  daqi <- daqi[order(daqi$site, daqi$date), ]

  # return
  return(daqi)
}

#' Reshape DAQI
#' @noRd
pivotDAQI <- function(daqi, opts) {
  opts[opts == "concentration"] <- "value"
  opts[opts == "poll_index"] <- "index"
  opts[opts == "poll_band"] <- "band"
  opts[opts == "colour_index"] <- "col_index"
  opts[opts == "colour_band"] <- "col_band"
  opts[opts == "measurement_period"] <- "period"

  names(daqi)[names(daqi) == "concentration"] <- "value"
  names(daqi)[names(daqi) == "poll_index"] <- "index"
  names(daqi)[names(daqi) == "poll_band"] <- "band"
  names(daqi)[names(daqi) == "colour_index"] <- "col_index"
  names(daqi)[names(daqi) == "colour_band"] <- "col_band"
  names(daqi)[names(daqi) == "measurement_period"] <- "period"

  daqi <-
    stats::reshape(
      as.data.frame(daqi)[c("date", "code", "site", "pollutant", opts)],
      timevar = "pollutant",
      idvar = c("date", "code", "site"),
      direction = "wide",
      v.names = opts,
      sep = "_"
    )

  names(daqi) <- gsub("value_", "", names(daqi))

  grid <-
    expand.grid(a = opts[opts != "value"], b = daqi_pollutant_names)
  old <- paste(grid$a, grid$b, sep = "_")
  new <- paste(grid$b, grid$a, sep = "_")

  for (i in seq_along(old)) {
    names(daqi)[names(daqi) == old[i]] <- new[i]
  }

  return(daqi)
}

#' Format DAQI URL using source/year
#' @noRd
daqi_url <- function(source, year) {
  url <- switch(
    source,
    "aurn" = "https://uk-air.defra.gov.uk/openair/R_data/annual_DAQI_AURN_YEAR.rds",
    "aqe" = "https://airqualityengland.co.uk/assets/openair/R_data/annual_DAQI_AQE_YEAR.rds",
    "saqn" = "https://www.scottishairquality.scot/openair/R_data/annual_DAQI_SCOT_YEAR.rds",
    "waqn" = "https://airquality.gov.wales/sites/default/files/openair/R_data/annual_DAQI_WAQ_YEAR.rds",
    "niaqn" = "https://www.airqualityni.co.uk/openair/R_data/annual_DAQI_NI_YEAR.rds"
  )

  url <- gsub("YEAR", year, url)

  return(url)
}

#' Add DAQI bands column before "measurement_period" col
#' @noRd
append_daqi_bands <- function(data) {
  data$poll_band <- ifelse(
    data$poll_index %in% 1:3,
    "Low",
    ifelse(
      data$poll_index %in% 4:6,
      "Moderate",
      ifelse(data$poll_index %in% 7:9, "High", "Very High")
    )
  )
  data$poll_band <-
    factor(data$poll_band, c("Low", "Moderate", "High", "Very High"))

  data <-
    data[, c(
      names(data)[names(data) != "measurement_period"],
      "measurement_period"
    )]

  return(data)
}

# options for daqi columns
daqi_column_opts <- c(
  "concentration",
  "poll_index",
  "poll_band",
  "colour_index",
  "colour_band",
  "measurement_period"
)

# colours for DAQI index
daqi_index_cols <-
  stats::setNames(
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
  )

# colours for DAQI bands
daqi_band_cols <-
  stats::setNames(
    list("#009900", "#ff9900", "#ff0000", "#990099"),
    c("Low", "Moderate", "High", "Very High")
  )
