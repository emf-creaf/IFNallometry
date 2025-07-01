#Read IFN2 tree records DOES NOT WORK ANYMORE
# dir_IFN = "~/OneDrive/EMF_datasets/ForestInventories/IFN"
# plotCodes = c("80001", "80002", "81073")
# treeDataIFN2 = IFNread::readPiesMayoresIFN2(prov = "08", DBFdir = paste0(dir_IFN,"/Sources/IFN2/DBF"), subsetVars=TRUE)
# example_tree_data = treeDataIFN2[treeDataIFN2$ID %in% plotCodes, c("ID", "Species", "N", "DBH", "H")]
# usethis::use_data(example_tree_data, overwrite = TRUE)

# Provinces
df <- as.data.frame(readxl::read_xls("data-raw/ProvinciasCCAA.xls"))
provincias <- df[,-c(1,4,5,6)]
row.names(provincias) <- df$CODIGO
usethis::use_data(provincias, overwrite = TRUE)


# Recognized species names (volumes)  -----------------------------------------
# Combine ifn23 and ifn species (code taken from package forestables)
shrub_codes_ifn4 <- readr::read_delim(
  "data-raw/shrub_codes_ifn4.csv", delim = ";",
  escape_double = FALSE, trim_ws = TRUE
) |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    SP_CODE = as.character(IFNCODE)
  ) |>
  dplyr::rename(SP_NAME = IFNNAME) |>
  dplyr::select(SP_NAME, SP_CODE)
tree_codes_ifn4 <-  readr::read_delim(
  "data-raw/tree_codes_ifn4.csv",
  delim = ";", escape_double = FALSE, trim_ws = TRUE
) |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    SP_CODE = as.character(IFNCODE)
  ) |>
  dplyr::rename(SP_NAME = IFNNAME) |>
  dplyr::select(SP_NAME, SP_CODE)
species_codes_ifn23 <- readr::read_delim(
  "data-raw/SpeciesCodesIFN23.csv",
  delim = ";", escape_double = FALSE, trim_ws = TRUE
) |>
  dplyr::as_tibble() |>
  dplyr::mutate(
    SP_CODE = as.character(IFNCODE)
  ) |>
  dplyr::rename(SP_NAME = IFNNAME) |>
  dplyr::select(SP_NAME, SP_CODE)

species_ifn <- shrub_codes_ifn4 |>
  dplyr::full_join(tree_codes_ifn4, by = c("SP_NAME", "SP_CODE")) |>
  dplyr::full_join(species_codes_ifn23, by = c("SP_NAME", "SP_CODE")) |>
  dplyr::mutate(
    SP_CODE = as.numeric(SP_CODE)
  ) |>
  dplyr::distinct() |>
  dplyr::group_by(SP_CODE) |>
  dplyr::summarise(SP_NAME = dplyr::first(SP_NAME)) |>
  dplyr::rename(IFNcode = SP_CODE,
                Species = SP_NAME)
usethis::use_data(species_ifn, overwrite = TRUE)



# Internal data -----------------------------------------------------------
biomass_species_match <- as.data.frame(readxl::read_xlsx("data-raw/Spanish Biomass models def.xlsx",
                                                        sheet = "Equiv_species"))
biomass_parameters <- as.data.frame(readxl::read_xlsx("data-raw/Spanish Biomass models def.xlsx",
                                              sheet = "Biomass_IFNallometry"))
volume_parameters <- as.data.frame(readxl::read_xlsx("data-raw/Spanish Volume models def.xlsx",
                                            sheet = "TODO", na=c("","-")))
#Remove column 'Obs'
volume_parameters = volume_parameters[,-which(names(volume_parameters)=="Obs")]
#Remove records with missing species (improve!)
volume_parameters = volume_parameters[!is.na(volume_parameters$ID_TAXON),]



usethis::use_data(biomass_species_match, biomass_parameters,
                  volume_parameters,
                  internal = TRUE, overwrite = TRUE)

