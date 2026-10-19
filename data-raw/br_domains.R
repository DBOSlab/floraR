# Brazilian phytogeographic domains (biomes), bundled for flora_distribution_gap()
#
# Source: IBGE, "Biomas do Brasil" (2019, 1:250,000), obtained through the geobr
# package. The coastal-zone layer ("Sistema Costeiro") is dropped: it is not a
# phytogeographic domain, and it overlaps the six domains, so coastal records
# still fall within one of them. Boundaries are simplified with a 1 km tolerance
# (topology preserved), far finer than the precision of most herbarium
# coordinates, to keep the file small (about 215 KB). Domain names are IBGE's,
# which match FFB's phytogeographicDomain values.

library(sf)
sf_use_s2(FALSE)

biomes <- geobr::read_biomes(year = 2019, simplified = TRUE, showProgress = FALSE)
biomes <- biomes[!is.na(biomes$code_biome), ]

br_domains <- st_transform(biomes, 5880)  # SIRGAS 2000 / Brazil Polyconic (metres)
br_domains <- st_simplify(br_domains, preserveTopology = TRUE, dTolerance = 1000)
br_domains <- st_cast(st_transform(br_domains, 4674), "MULTIPOLYGON")  # SIRGAS 2000
br_domains <- br_domains[, "name_biome"]
names(br_domains)[names(br_domains) == "name_biome"] <- "domain"

saveRDS(br_domains, "inst/extdata/br_domains.rds", compress = "xz")
