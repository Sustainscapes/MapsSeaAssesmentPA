library(dplyr)



LT2 <- readRDS("O:/Nat_Sustain-proj/_user/derekCorcoran_au687614/biodiversitetsradet.github.io/sea_area_analyses/LT2.rds") |>
 dplyr::mutate_if(is.numeric, ~ ifelse(is.nan(.x), 0, .x)) |>
  dplyr::mutate_if(is.character, ~ifelse(is.na(.x), 0, 1)) |>
  dplyr::mutate(Km = (n*100)/1000000) |>
  dplyr::select(-n) |>
  dplyr::mutate(PS = ifelse(Natura2000 == 1 | Fredninger == 1 | Reserves ==1 | Havstrategi_omrade_existent == 1 | Natura2000Reef == 1, 1 , 0))

InsideOfProtectionSchemes <-  LT2 |>
  dplyr::filter(PS == 1)

ProtectionSchemesArea <- LT2 |>
  dplyr::filter(PS == 1) |> dplyr::pull(Km) |> sum()

PercentagePS <- 100*(ProtectionSchemesArea/Area_DK_HA_Sea)


ProtectionNatura2000Area <- LT2 |>
  dplyr::filter(Natura2000 == 1) |> dplyr::pull(Km) |> sum()

Fuglebeskyttelsesomraade_i_Tyske_Bugt <- terra::vect("O:/Nat_BDR-data/Arealanalyse/2023/RAW/Fuglebeskyttelsesområde_i_Tyske_Bugt/Fuglebeskyttelsesområde_i_Tyske_Bugt.shp") |> terra::expanse(unit = "km") |> sum()

ProtectionNatura2000Area <- ProtectionNatura2000Area + Fuglebeskyttelsesomraade_i_Tyske_Bugt

PercentageNatura2000 <- 100*(ProtectionNatura2000Area/Area_DK_HA_Sea)


ProtectionHO <- LT2 |>
  dplyr::filter(Havstrategi_omrade_existent == 1) |> dplyr::pull(Km) |> sum()

Aaresund <- terra::vect("O:/Nat_BDR-data/Arealanalyse/2024/SeaExis/Eksisterende_beskyttet_område_i_¥resund.shp") |>
  terra::expanse(unit = "km") |> sum()

ProtectionHO <- ProtectionHO + Aaresund
PercentageHO <- 100*(ProtectionHO/105000)

unzip("O:/Nat_BDR-data/Arealanalyse/2023/RAW/Fuglebeskyttelsesområde_i_Tyske_Bugt.zip", exdir = getwd())
