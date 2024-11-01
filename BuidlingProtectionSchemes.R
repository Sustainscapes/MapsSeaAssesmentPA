library(terra)
Natura2000 <- terra::vect("O:/Nat_BDR-data/Arealanalyse/2023/RAW/NATURA 2000/pg-natura_2000_omraader_natura2000.shp")


havstategi <- terra::rast("o:/Nat_Sustain-proj/_user/derekCorcoran_au687614/biodiversitetsradet.github.io/sea_area_analyses/ForLeaflet/Havstrategi_omrade_existent_Protected.tif")
N2000_iucn_reserves <- terra::rast("o:/Nat_Sustain-proj/_user/derekCorcoran_au687614/biodiversitetsradet.github.io/sea_area_analyses/Combined1.tif")

PSchemes <- c(havstategi, N2000_iucn_reserves)

PSchemesTab <- terra::crosstab(PSchemes, long = T, useNA=T)

havstategi <- havstategi |> as.polygons() |> terra::disagg()
N2000_iucn_reserves_sf <- N2000_iucn_reserves |> as.polygons() |> terra::disagg()

Union <- terra::union(havstategi, N2000_iucn_reserves_sf)

Oeresund <- terra::vect("O:/Nat_BDR-data/Arealanalyse/2024/SeaExis/Eksisterende_beskyttet_område_i_¥resund.shp") |> terra::project(Union)

Union2 <- terra::union(Union, Aaresund)

N2000Rev <- terra::vect("O:/Nat_BDR-data/Arealanalyse/2023/RAW/Trawlfri zoner/N2000 rev.shp") |>
  terra::project(Union2)

Union3 <- terra::union(Union2, N2000Rev)

terra::expanse(Union3, unit = "km") |> sum() |> magrittr::divide_by(Area_DK_HA_Sea) |> magrittr::multiply_by(100)

Nye_strengt_beskyttede <- terra::vect("O:/Nat_BDR-data/Arealanalyse/2023/RAW/Havstrategiområder/H¢ringsportalen_GIS_data/Nye_strengt_beskyttede_omraader_horing_26032021.shp") |>
  terra::project(Union2)

Nye_beskyttede <- terra::vect("O:/Nat_BDR-data/Arealanalyse/2023/RAW/Havstrategiområder/H¢ringsportalen_GIS_data/Nye_havstrategiomraader_horing_26032021.shp") |>
  terra::project(Union2)

Union4 <- terra::union(Union3, Nye_strengt_beskyttede)

Union5 <- terra::union(Union4, Nye_beskyttede)

terra::expanse(Union5, unit = "km") |> sum() |> magrittr::divide_by(Area_DK_HA_Sea) |> magrittr::multiply_by(100)

New <- terra::vect("O:/Nat_BDR-data/Arealanalyse/2023/RAW/Havplanen_digitaliseret/yderligere_tilfoejet_areal.shp") |>
  terra::project(Union2)


Union4 <- terra::union(Union3, New)
