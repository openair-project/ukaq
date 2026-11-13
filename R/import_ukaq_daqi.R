#' Import UK Daily Air Quality Index Data
#'
#' Imports [UK Daily Air Quality Index
#' (DAQI)](https://www.gov.uk/government/publications/health-effects-of-air-pollution)
#' data for selected pollutants, years, and sites. Returned information can
#' include the relevant daily statistic (e.g., daily average for PM10), the
#' index (1:10), the band (Low, Moderate, High, or Very High), and the colour
#' associated with the index and/or band.
#'
#' @param source *The UK monitoring network(s) from which to import data.*
#'
#'   *default:* `"aurn"`
#'
#'   Each value should be one of `"aurn"`, `"aqe"`, `"saqn"`, `"waqn"`,
#'   `"niaqn"`, or `"ukaq"`. The DAQI is not pre-calculated for `"lmam"` sites.
#'
#'   If `code = NULL`, `source` can be any combination of networks, and all
#'   sites in those networks are returned. `"ukaq"` returns all networks.
#'
#'   If `code` is provided, `source` can be either:
#'
#'   - **A single value**, which applies to every `code`. `"ukaq"` will look for
#'   each site in all of the available networks; any other value will *only*
#'   import data from that network.
#'
#'   - **A vector the same length as `code`**, pairing each site code with a
#'   network. For example, `code = c("MY1", "KC1", "ABD")` and `source =
#'   c("aurn", "aurn", "saqn")`. `"ukaq"` can be used for any code whose
#'   network should be worked out automatically.
#'
#'   When a site is found in multiple networks under `"ukaq"`, the first of
#'   `"aurn"`, `"aqe"`, `"saqn"`, `"waqn"` and `"niaqn"` is used and a warning
#'   is given.
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
#' @references With thanks to Trevor Davies and [WSP
#'   UK&I](https://www.wsp.com/en-gb) for preparing and hosting the data
#'
#' @seealso
#'   <https://www.gov.uk/government/publications/health-effects-of-air-pollution>
#'   for more information about the DAQI.
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
    daqi_columns <-
      rlang::arg_match(daqi_columns, daqi_column_opts, multiple = TRUE)

    # work out which networks to import
    if (is.null(code)) {
      networks <-
        match_source(
          source = source,
          network_names = ukaq_network_names_nolocal
        )
    } else {
      networks <- pair_networks(
        pair_code_source(code, source, ukaq_network_names_nolocal),
        ukaq_network_names_nolocal
      )
    }

    # import data
    daqi <- importDAQI(year = year, source = networks, progress = progress)

    # format
    daqi <-
      formatDAQI(
        daqi = daqi,
        pollutant = pollutant,
        code = code,
        source = source,
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
  source,
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

  # create poll band column (also needed to assign band colours)
  if (any(c("poll_band", "colour_band") %in% daqi_columns)) {
    daqi <- append_daqi_bands(daqi)
  }

  # filter for site/pollutant
  if (!is.null(pollutant)) {
    daqi <- daqi[tolower(daqi$pollutant) %in% tolower(pollutant), ]
  }

  if (!is.null(code)) {
    daqi <- filter_code_source(daqi, code, source, ukaq_network_names_nolocal)
  }

  # assign colours
  if ("colour_index" %in% daqi_columns) {
    daqi$colour_index <- unname(daqi_index_cols[daqi$poll_index])
  }
  if ("colour_band" %in% daqi_columns) {
    daqi$colour_band <-
      unname(daqi_band_cols[as.character(daqi$poll_band)])
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
        source = unique(daqi$source),
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
      as.data.frame(daqi)[c(
        "source",
        "date",
        "code",
        "site",
        "pollutant",
        opts
      )],
      timevar = "pollutant",
      idvar = c("source", "date", "code", "site"),
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
  # indices outside 1-10 (incl. NA) get an NA band
  band <- rep(NA_character_, nrow(data))
  band[data$poll_index %in% 1:3] <- "Low"
  band[data$poll_index %in% 4:6] <- "Moderate"
  band[data$poll_index %in% 7:9] <- "High"
  band[data$poll_index %in% 10] <- "Very High"
  data$poll_band <-
    factor(band, c("Low", "Moderate", "High", "Very High"))

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
    c(
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
    c("#009900", "#ff9900", "#ff0000", "#990099"),
    c("Low", "Moderate", "High", "Very High")
  )
