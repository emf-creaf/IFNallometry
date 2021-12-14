library(readxl)
library(foreign)

dir_IFN = "~/OneDrive/Datasets/IFN"
plotCodes = c("80001", "80002", "81073")

#Read IFN2 tree records
treeDataIFN2 = IFNread::readPiesMayoresIFN2(prov = "08", DBFdir = paste0(dir_IFN,"/Sources/IFN2/DBF"), subsetVars=TRUE)
exampleTreeData = treeDataIFN2[treeDataIFN2$ID %in% plotCodes, c("ID", "Species", "N", "DBH", "H")]
usethis::use_data(exampleTreeData, overwrite = TRUE)

df=as.data.frame(read_xls(paste0(dir_IFN,"/Sources/ProvinciasCCAA.xls")))
provincias = df[,-1]
row.names(provincias) = df$CODIGO

# provincias = provincias[order(as.numeric(row.names(provincias))),]
usethis::use_data(provincias, overwrite = TRUE)

biomass_species_match = as.data.frame(read_xlsx(paste0(dir_IFN,
                                                       "/Sources/VolumeBiomassModels/Spanish Biomass models def.xlsx"),
                                                sheet = "Equiv_species"))
biomass_parameters = as.data.frame(read_xlsx(paste0(dir_IFN,
                                                    "/Sources/VolumeBiomassModels/Spanish Biomass models def.xlsx"),
                                             sheet = "Biomass_IFNallometry"))
volume_parameters = as.data.frame(read_xlsx(paste0(dir_IFN,
                                                   "/Sources/VolumeBiomassModels/Spanish Volume models def.xlsx"),
                                            sheet = "TODO", na=c("","-")))
#Remove column 'Obs'
volume_parameters = volume_parameters[,-which(names(volume_parameters)=="Obs")]
#Remove records with missing species (improve!)
volume_parameters = volume_parameters[!is.na(volume_parameters$ID_TAXON),]
usethis::use_data(biomass_species_match, biomass_parameters,
                  volume_parameters,
                  internal = TRUE, overwrite = TRUE)

