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

# Download for all countries, 2000–2023
sdg7_data <- WDI(
  country = "all",
  indicator = sdg7_indicators,
  start = 2000,
  end = 2023,
  extra = TRUE
)

# Clean names
colnames(sdg7_data) <- make.names(colnames(sdg7_data))

# Save to CSV
write_csv(sdg7_data, "sdg7_data.csv")
