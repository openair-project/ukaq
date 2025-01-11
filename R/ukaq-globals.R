metadata_column_names <-
  c(
    "site_type",
    "latitude",
    "longitude",
    "zone",
    "agglomeration",
    "zagglom",
    "local_authority",
    "lmam_provider",
    "lmam_code"
  )

daqi_pollutant_names <-
  c("no2", "o3", "pm10", "pm2.5", "so2")

ukaq_networks <-
  data.frame(
    code = c("aurn", "aqe", "saqn", "waqn", "niaqn", "lmam"),
    name = c(
      "Automatic Urban and Rural Monitoring Network",
      "Air Quality England",
      "Air Quality in Scotland",
      "Air Quality in Wales",
      "Northern Ireland Air",
      "Locally-managed Automatic Monitoring"
    ),
    url = c(
      "https://uk-air.defra.gov.uk/",
      "https://www.airqualityengland.co.uk/",
      "https://www.scottishairquality.scot/",
      "https://www.airquality.gov.wales/",
      "https://www.airqualityni.co.uk/",
      "https://uk-air.defra.gov.uk/networks/network-info?view=nondefraaqmon"
    )
  )

ukaq_network_names <- ukaq_networks$code

ukaq_network_names_nolocal <-
  ukaq_network_names[ukaq_network_names != "lmam"]
