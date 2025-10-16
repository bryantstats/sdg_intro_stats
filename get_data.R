library(WDI)
library(dplyr)
library(readr)

# Select key SDG 7 indicators
sdg7_indicators <- c(
  "EG.ELC.ACCS.ZS",  # Access to electricity (% of population)
  "EG.CFT.ACCS.ZS",  # Access to clean fuels and technologies for cooking (%)
  "EG.FEC.RNEW.ZS",  # Renewable energy consumption (% of total final energy)
  "EG.USE.PCAP.KG.OE", # Energy use per capita (kg oil equivalent)
  "EG.ELC.RNWX.ZS"   # Electricity production from renewables (%)
)

# Download for all countries, 2000b


library(WDI)
library(dplyr)
library(readr)

# ---- Define indicators for SDG Goal 7 + CO??? ----
indicators <- c(
  "Access_to_electricity" = "EG.ELC.ACCS.ZS",          # Access to electricity (% population)
  "Access_to_clean_fuels" = "EG.CFT.ACCS.ZS",          # Access to clean fuels for cooking (%)
  "Renewable_energy_share" = "EG.FEC.RNEW.ZS",         # Renewable energy share in final consumption (%)
  "Electricity_from_renewables" = "EG.ELC.RNWX.ZS",    # Electricity production from renewables (%)
  "Energy_intensity" = "EG.EGY.PRIM.PP.KD",            # Energy intensity (MJ per USD GDP)
  "Energy_use_per_capita" = "EG.USE.PCAP.KG.OE",       # Energy use per capita (kg oil equivalent)
  "Electric_power_consumption" = "EG.USE.ELEC.KH.PC",  # Electric power consumption (kWh per capita)
  "CO2_emissions_per_capita" = "EN.ATM.CO2E.PC",       # CO??? emissions (metric tons per capita)
  "CO2_from_electricity" = "EN.CO2.ETOT.ZS"            # CO??? from electricity and heat (% of total)
)

# ---- Download up-to-date data ----
sdg7_data <- WDI(
  country = "all",
  indicator = indicators,
  start = 2000,
  end = as.integer(format(Sys.Date(), "%Y")),
  extra = TRUE
)

# ---- Clean and organize ----
sdg7_clean <- sdg7_data %>%
  filter(region != "Aggregates") %>%      # Remove world/regional aggregates
  rename(
    Country = country,
    ISO2 = iso2c,
    ISO3 = iso3c,
    Region = region,
    IncomeGroup = income,
    Year = year
  ) %>%
  arrange(Country, Year)

# ---- Save to CSV ----
write_csv(sdg7_clean, "sdg7_data_latest.csv")

# Quick summary check
cat("??? SDG 7 data saved as 'sdg7_data_latest.csv'\n")
cat("Rows:", nrow(sdg7_clean), " Columns:", ncol(sdg7_clean), "\n")
glimpse(sdg7_clean)
