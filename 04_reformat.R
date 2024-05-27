library(tidyverse)

drivers_reformat <- drivers_CT %>% 
  mutate(Precipitation_millimeterPerDay = Rain.m_day * 1000) %>%
  select(-SpecHumidity.kg_kg) %>% 
  rename(datetime = dateTime, 
         Air_Temperature_celsius = AirTemp.C, 
         Shortwave_Radiation_Downwelling_wattPerMeterSquared = ShortWave.W_m2, 
         Longwave_Radiation_Downwelling_wattPerMeterSquared = LongWave.W_m2, 
         Relative_Humidity_percent = RelHum,
         Ten_Meter_Elevation_Wind_Speed_meterPerSecond = WindSpeed.m_s,
         Surface_Level_Barometric_Pressure_pascal = SurfPressure.Pa)
col_order <- c("datetime", "Air_Temperature_celsius", "Shortwave_Radiation_Downwelling_wattPerMeterSquared", 
               "Longwave_Radiation_Downwelling_wattPerMeterSquared", "Relative_Humidity_percent", 
               "Ten_Meter_Elevation_Wind_Speed_meterPerSecond", "Precipitation_millimeterPerDay", "Surface_Level_Barometric_Pressure_pascal")
drivers_reformat <- drivers_reformat[, col_order]

write.csv(drivers_reformat,paste0(LakeName,'_Final/',LakeName,'_all_variables_for_1DAEMpy.csv'),row.names = F, quote = F)