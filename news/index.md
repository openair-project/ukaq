# Changelog

## ukaq (development version)

### New features

- [`import_ukaq_measurements()`](https://openair-project.github.io/ukaq/reference/import_ukaq_measurements.md),
  [`import_ukaq_summaries()`](https://openair-project.github.io/ukaq/reference/import_ukaq_summaries.md)
  and
  [`import_ukaq_daqi()`](https://openair-project.github.io/ukaq/reference/import_ukaq_daqi.md)
  can now pair site codes with specific networks. When `code` is
  provided, `source` can be either a single value applied to every code
  (as before), or a vector the same length as `code`, e.g.,
  `code = c("MY1", "KC1", "ABD")` and
  `source = c("aurn", "aurn", "saqn")`. Any element can be `"ukaq"` to
  let the network be worked out automatically.

- The `progress` argument of
  [`import_ukaq_measurements()`](https://openair-project.github.io/ukaq/reference/import_ukaq_measurements.md),
  [`import_ukaq_summaries()`](https://openair-project.github.io/ukaq/reference/import_ukaq_summaries.md)
  and
  [`import_ukaq_daqi()`](https://openair-project.github.io/ukaq/reference/import_ukaq_daqi.md)
  now defaults to
  [`rlang::is_interactive()`](https://rlang.r-lib.org/reference/is_interactive.html),
  so progress bars are shown in interactive sessions, even when only one
  file is imported.

### Breaking changes

- When `code` is provided, `source` must now be length 1 or the same
  length as `code`. Previously, multiple networks were treated as an
  order of preference (e.g., `source = c("saqn", "aurn")`); instead, use
  `source = "ukaq"` to have the network found automatically, or pair
  each code with its network. Codes found in multiple networks under
  `"ukaq"` use the first of `"aurn"`, `"aqe"`, `"saqn"`, `"waqn"`,
  `"niaqn"` and `"lmam"`, with a warning.

- [`import_ukaq_summaries()`](https://openair-project.github.io/ukaq/reference/import_ukaq_summaries.md)
  and
  [`import_ukaq_daqi()`](https://openair-project.github.io/ukaq/reference/import_ukaq_daqi.md)
  now return a code from a single network when `code` is provided.
  Previously, a code present in multiple networks (e.g., `"ABD"` in the
  AURN and SAQN) was returned once for each network.

- `import_ukaq_daqi(pivot = "wide")` now retains the `source` column.
  Previously this was dropped, which collapsed sites reported by more
  than one network and prevented `append_metadata = TRUE` from working.

- The `year` argument of
  [`import_ukaq_meta()`](https://openair-project.github.io/ukaq/reference/import_ukaq_meta.md)
  now returns sites open at *any point* within the given range, as
  documented. Previously only sites open for the entire range were
  returned.

- `import_ukaq_measurements(pivot = "long", append_quality_flag = TRUE)`
  no longer drops pollutants without a ratification date (e.g., all
  `"lmam"` data); these are now returned with `qc = NA`.

### Bug fixes

- [`import_ukaq_meta()`](https://openair-project.github.io/ukaq/reference/import_ukaq_meta.md)
  and
  [`import_ukaq_measurements()`](https://openair-project.github.io/ukaq/reference/import_ukaq_measurements.md)
  no longer fail when a network’s metadata is unavailable (e.g., if Air
  Quality England is offline). Unavailable networks are skipped with a
  warning.

- [`import_ukaq_measurements()`](https://openair-project.github.io/ukaq/reference/import_ukaq_measurements.md)
  now correctly returns `"15_min"`, `"8_hour"`, `"24_hour"` and
  `"daily_max_8"` data, which previously always errored with “No data
  has been returned.”

- `import_ukaq_measurements(data_type = "daily")` now includes
  additional daily statistics (e.g., gravimetric PM, `gr10` and `gr2.5`)
  where available.

- `import_ukaq_meta(by_pollutant = FALSE)` no longer drops sites which
  share a name with a site in another network (e.g., ABD7 and ABD3, both
  “Aberdeen Union Street Roadside”). This also affected
  [`import_ukaq_measurements()`](https://openair-project.github.io/ukaq/reference/import_ukaq_measurements.md)
  and `append_metadata = TRUE`.

- `import_ukaq_daqi(append_metadata = TRUE)` no longer errors.

- [`import_ukaq_meta()`](https://openair-project.github.io/ukaq/reference/import_ukaq_meta.md)
  no longer errors when `"lmam"` is listed before other networks in
  `source`.

- Missing DAQI indices now result in missing bands and colours, rather
  than a “Very High” band. Requesting `"colour_band"` without
  `"poll_band"` now returns band colours.

- Ratification flags (`append_quality_flag = TRUE`) now treat data as
  ratified up to the *end* of the ratification date, and are no longer
  incorrect when a site lists a pollutant more than once.

- `import_ukaq_summaries(pivot = "wide")` now matches `pollutant`
  exactly (e.g., `"co"` no longer also selects the `code` column).

- Unknown site codes or pollutants passed to
  [`import_ukaq_measurements()`](https://openair-project.github.io/ukaq/reference/import_ukaq_measurements.md),
  and summary requests which return no data, now give informative errors
  (or warnings, if only some values are unmatched).

- Ambiguous site codes are now reported in a single, readable warning.

- Removed R \>= 4.1 syntax so that the package works with R \>= 3.5, as
  declared in `DESCRIPTION`.

### Documentation

- The documented default `source` for
  [`import_ukaq_daqi()`](https://openair-project.github.io/ukaq/reference/import_ukaq_daqi.md)
  and
  [`import_ukaq_summaries()`](https://openair-project.github.io/ukaq/reference/import_ukaq_summaries.md)
  is now correctly given as `"aurn"`.

## ukaq 0.1.0

This is a lightweight package for importing UK air quality data.

It has a single import ([rlang](https://rlang.r-lib.org)) and two
suggested packages ([tibble](https://tibble.tidyverse.org/) and
[cli](https://cli.r-lib.org)).
