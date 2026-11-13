# Import Continuous UK Air Quality Monitoring Data

Imports continuous monitoring data from UK air quality networks. This
function defaults to importing hourly data, but has options to instead
return other common time periods.

## Usage

``` r
import_ukaq_measurements(
  code,
  year,
  source = "ukaq",
  data_type = "hourly",
  pollutant = NULL,
  append_meteorology = TRUE,
  append_quality_flag = FALSE,
  append_metadata = FALSE,
  metadata_columns = c("site_type", "latitude", "longitude"),
  pivot = "wide",
  progress = NA,
  ...,
  .class = NULL
)
```

## Arguments

- code:

  *Specific site codes to import.*

  **required**

  `code` expects a vector of character values. It defines the specific
  sites to import will filter the result for specific sites based on
  their site codes, available through
  [`import_ukaq_meta()`](https://openair-project.github.io/ukaq/reference/import_ukaq_meta.md).
  Note that a mismatch between `code` and `source` may result in no data
  being imported.

- year:

  *A year, or range of years, from which to import data.*

  **required**

  `year` expects a vector of integer values. It can be a single year
  (`2020`), a selection of specific years (c(`2020, 2021, 2022`)), or a
  range of years (`2020:2025`).

- source:

  *One or more UK Monitoring networks from which to import data.*

  *default:* `"ukaq"`

  The default, `"ukaq"`, will look for sites from any of the available
  networks. Any combination of `"aurn"`, `"aqe"`, `"saqn"`, `"waqn"`,
  `"niaqn"` or `"lmam"` will *only* import data from those specific
  monitoring networks. Note that a mismatch between `code` and `source`
  may result in no data being imported.

- data_type:

  *What type of summary should be returned?*

  *default:* `"hourly"`

  `import_ukaq_measurements()` can return many different continuous
  measurement types. These are as follows:

  - `"hourly"`: Hourly data (the default).

  - `"daily"`: Daily average data.

  - `"15_min"`: 15-minute average SO2 concentrations.

  - `"8_hour"`: 8-hour rolling mean concentrations for O3 and CO.

  - `"24_hour"`: 24-hour rolling mean concentrations for particulates.

  - `"daily_max_8"`: Maximum daily rolling 8-hour maximum for O3 and CO.

- pollutant:

  *One or more pollutants for which to import data.*

  *default:* `NULL`

  By default, all available pollutants are imported. `pollutant` allows
  any specific combination of the pollutants to be returned instead.

- append_meteorology:

  *Append modelled meteorology to the dataframe?*

  *default:* `TRUE`

  Certain networks such as the AURN are accompanied by modelled
  meteorological data obtained from the WRF model. Setting
  `append_meteorology = FALSE` will remove this, which may be useful if
  joining measured meteorological data sourced from elsewhere.

- append_quality_flag:

  *Append ratification indicators to the dataframe?*

  *default:* `FALSE`

  If `TRUE`, additional `_qc` columns will be appended to the data which
  are `TRUE` if the data has been ratified and `FALSE` if it has not
  been. Specific ratification dates can be obtained from
  [`import_ukaq_meta()`](https://openair-project.github.io/ukaq/reference/import_ukaq_meta.md).

- append_metadata:

  *Append site metadata to the dataframe?*

  *default:* `FALSE`

  When `TRUE`, the resulting `data.frame` will have site metadata
  appended. The specific columns are selected using `metadata_columns`.

- metadata_columns:

  *Specific metadata columns to append to the data.frame.*

  *default:* `c("site_type", "latitude", "longitude")`

  When `append_metadata = TRUE`, the columns selected here will be
  appended to the data. Columns names should match those in
  [`import_ukaq_meta()`](https://openair-project.github.io/ukaq/reference/import_ukaq_meta.md),
  excluding pollutant-specific columns (e.g., ratification date).

- pivot:

  *Should the dataframe be 'wide' or 'long'?*

  *default:* `"wide"`

  There are two main ways to store air quality data; "long" (with
  'pollutant' and 'value' columns) or "wide" (with each pollutant value
  being stored in its own column). `pivot` allows users to define which
  format they would prefer their data in.

- progress:

  *Show a progress bar?*

  *default:* `NA`

  When `TRUE`, this function will print a progress bar to track
  individual files being imported. If `FALSE` this is suppressed. If
  `NA`, the default, the function will work out if a progress bar would
  be useful (i.e., if the function is being run in an interactive
  session and if more than one remote file is being accessed).

- ...:

  *Not used.*

- .class:

  *Signifier for the dataframe class.*

  *default:* `NULL`

  [ukaq](https://openair-project.github.io/ukaq/reference/ukaq-package.md)
  functions, by default, will return `tbl_df`s if the
  [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
  package is installed, but will otherwise return `data.frame`s.
  `.class` can override this behaviour, and takes either `"tbl"` or
  `"df"`, which sets the return class to be `tbl_df` or `data.frame`
  respectively.

## Value

a `data.frame`

## References

With thanks to Trevor Davies and [WSP UK&I](https://www.wsp.com/en-gb)
for preparing and hosting the data

## Author

Jack Davison, David Carslaw
