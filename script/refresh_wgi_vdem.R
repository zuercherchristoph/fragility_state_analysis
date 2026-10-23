# =============================================================================
# DESCRIPTION
# =============================================================================
# Rebuild data/WGIdata.rds and data/VDemCYdata.rds from the latest official
# releases of each source, replacing the full historical panel rather than
# appending new years.
#
# Why full replacement, not append: both WGI and V-Dem revise their *entire*
# historical back-series with each new release (V-Dem explicitly documents
# this in their FAQ: "scores are expected to change with new releases" when
# their measurement model is re-estimated with new coder data). Appending new
# years onto an old-vintage historical series would mix incompatible vintages
# and create an artificial discontinuity at the join point.
#
# Country universe and groupings (region/income group/etc.) are NOT rebuilt
# here -- this script joins the freshly pulled values onto the EXISTING
# data/WGImap.rds and data/VDemCmap.rds country tables by ISO3 code, so the
# app's country list and classifications are unchanged. Only the indicator
# VALUES are refreshed to a single consistent vintage.
#
# FSI is intentionally not touched by this script -- Fund for Peace has not
# published a clean bulk file for years after 2023 (see project notes).
#
# Run from the repository root:
#   Rscript script/refresh_wgi_vdem.R
# =============================================================================

library(dplyr)
library(tidyr)
library(httr)
library(jsonlite)

cat("== Loading existing country maps (unchanged) ==\n")
WGI_map  <- readRDS("data/WGImap.rds")
VDem_map <- readRDS("data/VDemCmap.rds")

# =============================================================================
# 1. WGI -- World Bank Worldwide Governance Indicators, official API
#    source=3, "Estimate" indicators (approx. -2.5 to 2.5), matches the
#    app's existing Control/Government/Political/Regulatory/Rule/Voice cols.
# =============================================================================
cat("== Fetching WGI from World Bank API (source=3) ==\n")

wgi_indicators <- c(
  Control    = "GOV_WGI_CC.EST",
  Government = "GOV_WGI_GE.EST",
  Political  = "GOV_WGI_PV.EST",
  Regulatory = "GOV_WGI_RQ.EST",
  Rule       = "GOV_WGI_RL.EST",
  Voice      = "GOV_WGI_VA.EST"
)

fetch_wb_indicator <- function(indicator_code) {
  url <- sprintf(
    "https://api.worldbank.org/v2/country/all/indicator/%s?source=3&format=json&date=1996:2025&per_page=20000",
    indicator_code
  )
  resp <- GET(url)
  stop_for_status(resp)
  parsed <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
  rows <- parsed[[2]]
  rows |>
    transmute(
      Code  = countryiso3code,
      Year  = as.numeric(date),
      value = as.numeric(value)
    ) |>
    filter(Code != "", !is.na(value))
}

wgi_long <- lapply(names(wgi_indicators), function(nm) {
  d <- fetch_wb_indicator(wgi_indicators[[nm]])
  d$Variable <- nm
  d
}) |> bind_rows()

wgi_wide <- wgi_long |>
  pivot_wider(id_cols = c(Code, Year), names_from = Variable, values_from = value)

# NOTE: WGImap.rds has a pre-existing data bug -- "French Guiana" and
# "Guyana" both share Code "GUY" (French Guiana's real ISO3 is GUF). This
# duplicate predates this script (the original WGIdata.rds already carried
# Guyana's values under French Guiana's label too) -- reproduced as-is here,
# not introduced or fixed by this refresh. relationship="many-to-many"
# silences the expected warning for that one duplicate.
WGI_data_new <- wgi_wide |>
  inner_join(WGI_map |> select(Code, Country.Territory), by = "Code",
             relationship = "many-to-many") |>
  mutate(Variable = "estimate") |>
  select(Code, Country.Territory, Year, Variable,
         Control, Government, Political, Rule, Regulatory, Voice) |>
  arrange(Country.Territory, Year)

cat(sprintf(
  "WGI: %d rows, %d countries, years %d-%d\n",
  nrow(WGI_data_new), n_distinct(WGI_data_new$Code),
  min(WGI_data_new$Year), max(WGI_data_new$Year)
))

dropped_wgi <- setdiff(unique(wgi_wide$Code), WGI_map$Code)
if (length(dropped_wgi) > 0) {
  cat(sprintf(
    "NOTE: %d WGI codes not in existing WGImap.rds, dropped: %s\n",
    length(dropped_wgi), paste(dropped_wgi, collapse = ", ")
  ))
}

saveRDS(WGI_data_new, "data/WGIdata.rds")
cat("Wrote data/WGIdata.rds\n\n")

# =============================================================================
# 2. V-Dem -- Country-Year Full+Others, v16 (March 2026), official release
#    via the vdemdata package data file (data/vdem.RData), already downloaded
#    to vdem_pkg/vdemdata/data/vdem.RData relative to the repo root's parent.
# =============================================================================
cat("== Loading V-Dem v16 (official release) ==\n")

vdem_rdata_path <- "../vdem_pkg/vdemdata/data/vdem.RData"
stopifnot(file.exists(vdem_rdata_path))
e <- new.env()
load(vdem_rdata_path, envir = e)
vdem <- e$vdem

vdem_vars <- c("v2xnp_client", "v2x_libdem", "v2caviol",
               "v2xnp_regcorr", "v2clrspct", "v2x_rule", "v2svstterr")

# Match the original data's scope: Year >= 1971 only (the existing baked-in
# panel never went further back than 1971, even though V-Dem itself covers
# some countries back to 1789).
VDemCY_data_new <- vdem |>
  filter(year >= 1971) |>
  transmute(
    Code = country_text_id,
    Year = year,
    across(all_of(vdem_vars))
  ) |>
  inner_join(VDem_map |> select(Code, Country.Territory), by = "Code") |>
  select(Country.Territory, Code, Year, all_of(vdem_vars)) |>
  arrange(Country.Territory, Year)

cat(sprintf(
  "V-Dem: %d rows, %d countries, years %d-%d\n",
  nrow(VDemCY_data_new), n_distinct(VDemCY_data_new$Code),
  min(VDemCY_data_new$Year), max(VDemCY_data_new$Year)
))

dropped_vdem <- setdiff(unique(vdem$country_text_id), VDem_map$Code)
if (length(dropped_vdem) > 0) {
  cat(sprintf(
    "NOTE: %d V-Dem codes not in existing VDemCmap.rds, dropped: %s\n",
    length(dropped_vdem), paste(dropped_vdem, collapse = ", ")
  ))
}

saveRDS(VDemCY_data_new, "data/VDemCYdata.rds")
cat("Wrote data/VDemCYdata.rds\n\n")

cat("== Done. FSI data (data/FSIdata.rds) was NOT touched. ==\n")
