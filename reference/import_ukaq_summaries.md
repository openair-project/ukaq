# Import Monthly and Annual Summaries of UK Air Quality Data

Imports either annual or monthly summaries matching UK AIR. Unlike the
data available in
[`import_ukaq_measurements()`](https://openair-project.github.io/ukaq/reference/import_ukaq_measurements.md),
this data is appended with a monthly/annual data capture and additional
relevant columns (e.g., the month as a character for monthly data).

## Usage

``` r
import_ukaq_summaries(
  code = NULL,
  year,
  source = "aurn",
  data_type = "annual",
  pollutant = NULL,
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

  *default:* `NULL`

  `code` expects a vector of character values. It defines the specific
  sites to import will filter the result for specific sites based on
  their site codes, available through
  [`import_ukaq_meta()`](https://openair-project.github.io/ukaq/reference/import_ukaq_meta.md).
  Note that a mismatch between `code` and `source` may result in no data
  being imported. If `NULL`, the default, all available data for the
  `year` and `source` will be returned.

- year:

  *A year, or range of years, from which to import data.*

  **required**

  `year` expects a vector of integer values. It can be a single year
  (`2020`), a selection of specific years (c(`2020, 2021, 2022`)), or a
  range of years (`2020:2025`).

- source:

  *One or more UK Monitoring networks from which to import data.*

  *default:* `"aurn"`

  By default, data from the AURN is imported. Any combination of
  `"aurn"`, `"aqe"`, `"saqn"`, `"waqn"`, `"niaqn"` or `"lmam"` will
  *only* import data from those specific monitoring networks, and
  `"ukaq"` will import data from all of them. Note that a mismatch
  between `code` and `source` may result in no data being imported.

- data_type:

  *What type of summary should be returned?*

  *default:* `"annual"`

  `import_ukaq_summaries()` can return either `"annual"` (the default)
  or `"monthly"` summary data.

- pollutant:

  *One or more pollutants for which to import data.*

  *default:* `NULL`

  By default, all available pollutants are imported. `pollutant` allows
  any specific combination of the pollutants to be returned instead.

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
