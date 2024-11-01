BDR1
================

## Protection schemes

``` r
library(terra)
```

    ## terra 1.7.83

``` r
library(ggplot2)
library(tidyterra)
```

    ## 
    ## Vedhæfter pakke: 'tidyterra'

    ## Det følgende objekt er maskeret fra 'package:stats':
    ## 
    ##     filter

Read the protection schemes

``` r
All <- c("O:/Nat_BDR-data/Arealanalyse/2023/CLEAN/Rast_Natura2000_Croped_Sea.tif", 
            "O:/Nat_BDR-data/Arealanalyse/2023/CLEAN/Rast_Havstrategistandard_Croped_Sea.tif", 
            "O:/Nat_BDR-data/Arealanalyse/2023/CLEAN/Rast_vildtreservater_Croped_Sea.tif",
            "O:/Nat_BDR-data/Arealanalyse/2023/CLEAN/Rast_Fredninger_Croped_Sea.tif") |> 
  purrr::map(terra::rast) |> 
  purrr::reduce(c) |> 
  magrittr::set_names(c("Natura2000","Havstrategi_standard", "vildtreservater", "IUCN_Fredninger"))
```

We also need the exlucsive economic zone

``` r
SeaOfDenmark <- terra::vect("O:/Nat_BDR-data/Arealanalyse/CLEAN/SeaOfDenmarkBorder/EEZ.shp")
TemplateSea <- terra::rast("O:/Nat_BDR-data/Arealanalyse/CLEAN/SeaOfDenmarkBorder/TemplateSea.tif")

values(TemplateSea) <- 0

TemplateSea <- TemplateSea |> terra::mask(SeaOfDenmark, inverse = F)

Area_DK_HA_Sea <- terra::expanse(SeaOfDenmark, unit = "km")
```

# Combination 1

This is existing protection schemes

``` r
PSbinary <- terra::rast("o:/Nat_Sustain-proj/_user/derekCorcoran_au687614/biodiversitetsradet.github.io/sea_area_analyses/Combined1.tif")
```

# Combination 2

## Trawl free zone

All trawl free zone

``` r
TrawlFree <- terra::rast("o:/Nat_Sustain-proj/_user/derekCorcoran_au687614/biodiversitetsradet.github.io/sea_area_analyses/Trawlfri.tif")
```

Combination 2 is both trawlfree zones and natura 2000 reefs in one map

``` r
Combination2 <- terra::rast("o:/Nat_Sustain-proj/_user/derekCorcoran_au687614/biodiversitetsradet.github.io/sea_area_analyses/Combined2.tif")
```

Now from all this and overlaps of combination 1 and 2 we get the
protected areas which are:

``` r
PA <- terra::rast("o:/Nat_Sustain-proj/_user/derekCorcoran_au687614/biodiversitetsradet.github.io/sea_area_analyses/Overlap.tif")
```

We also have this layer about fishing and trawling, when we make this a
numeric variable, we care about level 1, which is not in the trawlfri
area but there is no active fishing in it (bottom trawl)

``` r
FishingTrawling <- terra::rast("o:/Nat_Sustain-proj/_proj/Coastal sequence/FishingTrawling.tif")  |> as.numeric()
```

## Goal

Final should map should have Protected areas, requires individual
assesment, insufficient legal protection, active fishing with bottom
trawl, no protection.

- Protected areas are PA
- require individual assesment is only fredninger
- insufficient legal protection (PSbinary substract Protecta and
  individual assesment, and then find which of this is not trawlfri, no
  active fishing)
- active fishing with no trawl

### First we add protected areas

``` r
Categories <- TemplateSea
Categories <- terra::ifel(PA == 1, 1, TemplateSea)
```

Then we add requires individual assesment

``` r
IUCN <- terra::rast("O:/Nat_BDR-data/Arealanalyse/2023/CLEAN/Rast_Fredninger_Croped_Sea.tif") |> as.numeric()

Categories <- terra::ifel(Categories == 0 & IUCN == 0, 2, Categories)
```

After that we include insufficient legal protection

1.- It is part of the protection schemes, 2.-It is not in the categories
of protected areas or requires individual assesment 3.- is not in the
trawlfri area 4.- there is no active fishing in it (bottom trawl)

``` r
PSbinary <- as.numeric(PSbinary)
TrawlFree <- as.numeric(TrawlFree)
Categories <- terra::ifel(PSbinary == 0 & Categories == 0 & FishingTrawling == 1, 3, Categories)
```

``` r
LVLS <- data.frame(Level = c(0:3), Category = c("Other", "Protected", "Requires individual assesment", "insufficient legal protection"))

Categories2 <- Categories
levels(Categories2) <- LVLS
SpeciesPoolR::write_cog(Categories2, "FourCats.tif")
```

## Final category

``` r
Categories <- terra::rast("FourCats.tif") |> 
  as.numeric()

Categories <- terra::ifel(Categories == 0 & FishingTrawling %in% c(2,3) & PSbinary == 0, 4, Categories)

LVLS <- data.frame(Level = c(0:4), Category = c("Other", "Protected", "Requires individual assesment", "insufficient legal protection", "Active fishing"))

Categories2 <- Categories
levels(Categories2) <- LVLS
SpeciesPoolR::write_cog(Categories2, "SeaAllCats.tif")
```
