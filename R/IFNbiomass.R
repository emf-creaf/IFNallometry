#' Biomass models
#'
#' Static models for the biomass (kg/ha) of stem, roots, branches, leaves/needles and bark of species in the IFN
#'
#' @param x A data frame with tree records in rows and columns 'Species', 'DBH' (in cm), 'H' (in m) and 'N' (ha-1)
#' @param as.CO2 Flag to indicate output as kg of CO2 / ha instead of kg of dry weight / ha. Percentage of carbon per dry weight biomass by species are
#'              taken from Montero et al. (2005) (in turn, from Ibáñez et al. 2002).
#' @param area Either 'Atlantic' or 'Mediterranean' to specify allometric equations specific to the area (for Pinus pinaster)
#' @param DBHclasses A numeric vector of DBH class limits (see breaks in function \code{cut}), used to group results by DBH class (in addition to species and plot). If \code{DBHclasses = NULL} then grouping on the basis of DBH classes is not performed.
#' @param verbose A flag to indicate console output of the biomass calculation process
#'
#'
#' @details Function \code{IFNbiomass} determines the allometric equation for each biomass fraction (i.e. compartment) using species code.
#' All equations are calculated form height and diameter at breast height (DBH) of felled trees. Biomass values from equations refer to
#' individual trees, but the function multiplies the result by the density of individuals 'N', so the resulting value is in units of kg/ha.
#' Biomass of branches is the result of adding the result of equations calibrated for different branch diameters.
#'
#' @name IFNbiomass
#' @return If \code{DBHclasses = NULL}, function \code{IFNbiomass} returns a data frame with as many rows as tree records in \code{x} and columns 'ID', 'Species', 'Roots', 'Stem', 'Branches', 'Leaves', 'Needles', 'Bark', 'Aerial' and 'Total'.
#' If \code{DBHclasses != NULL} then an extra column \code{DBHclass} is given and the data frame has less rows than tree records in \code{x}.
#' Function \code{IFNproducts} returns a data frame with the biomass of products (as well as that of stumps and slash), assuming trees have been felled down.
#'
#' @references
#' Diéguez-Aranda, U., A. Rojo Alboreca, F. Castedo-Dorado, J. G. Álvarez González, M. Barrio-Anta, F. Crecente-Campo, J. M. González González, C. Pérez-Cruzado, R. Rodríguez Soalleiro, C. A. López-Sánchez, M. Á. Balboa-Murias, J. J. Gorgoso Varela, and F. Sánchez Rodríguez (2009) Herramientas selvícolas para la gestión forestal sostenible en Galicia. Dirección Xeral de Montes, Consellería do Medio Rural, Xunta de Galicia.
#'
#' Ibáñez JJ, Vayreda J., Gracia C. (2002) Metodología complementaria al Inventario Forestal Nacional en Catalunya. En: Bravo F.; del Río M.; del Peso C.
#' (eds.) El inventario Forestal Nacional. Elemento clave para la gestión forestal sostenible: 67-77. Fundación General de la Universidad de Valladolid
#' Montero G, Ruiz-Peinado R, Muñoz M (2005) Producción de biomasa y fijación de CO2 por los bosques españoles. Monografias INIA: Serie Forestal nº 13.
#'
#' Ruiz-Peinado, R., M. Rio, and G. Montero (2011) New models for estimating the carbon sink capacity of Spanish. Forest Systems 20:176–188.
#'
#' Ruiz-Peinado, R., G. Montero, and M. Del Rio (2012) Biomass models to estimate carbon stocks for hardwood tree species. Forest Systems 21:42.
#' @examples
#' data(exampleTreeData)
#'
#' IFNbiomass(exampleTreeData)
#'
#' # Groups the result by DBH clases
#' IFNbiomass(exampleTreeData,
#'            DBHclasses = seq(0, 120, by=5))
#'
IFNbiomass<-function(x, as.CO2 = FALSE, area = NA,
                     DBHclasses = NULL, verbose = FALSE) {
  Species = x$Species
  DBH = x$DBH
  H = x$H
  N = x$N
  nr = nrow(x)
  df = data.frame(ID = x$ID, Species = x$Species, Name = rep(NA,nr),
                  SpeciesAllom = rep(NA,nr), NameAllom = rep(NA, nr),
                  Roots = rep(NA, nr), Stem = rep(NA, nr),
                  Branches = rep(NA,nr), Leaves = rep(NA, nr),
                  Needles = rep(NA, nr), Bark = rep(NA, nr),
                  Aerial = rep(NA, nr), Total = rep(NA,nr))
  sp_unique = unique(Species)
  nu = length(sp_unique)
  if(verbose) {
    pb = txtProgressBar(1, nu, style=3)
  }
  for(i in 1:nu) {
    if(verbose) setTxtProgressBar(pb, i)
    spu = sp_unique[i]
    sel = (Species==spu)
    # cat(paste("Taxon",spu," #", sum(sel)))
    perc.CO2 =  biomass_species_match$PORCENTAJE_CARBONO[biomass_species_match$ID_TAXON == spu]
    taxon =  as.numeric(biomass_species_match$ID_TAXON2[biomass_species_match$ID_TAXON == spu])
    if(length(taxon)!=1) stop(paste0("Wrong species code '", spu,"'."))
    df$Name[sel] = biomass_species_match$LATIN_TAXON[biomass_species_match$ID_TAXON == spu]
    df$SpeciesAllom[sel] = taxon
    df$NameAllom[sel] = biomass_species_match$LATIN_TAXON[biomass_species_match$ID_TAXON == taxon]
    par_stem = .getBiomassParams(taxon, "Stem", area)
    if(nrow(par_stem)>0) {
      b = .biomass(DBH[sel], H[sel], as.list(par_stem[1,]))
      if(sum(is.na(b))>0) {
        if(verbose) {
          cat(paste("\nWarning: NA values in stem biomass estimation for",sum(is.na(b)), "records.\n"))
          cat("Parameter vector:\n")
          print(par_stem[1,])
          sel2 = sel
          sel2[sel][!is.na(b)] = FALSE
          cat(paste("Input rows:",paste0(which(sel2), collapse = ","),"\n"))
          cat("Input DBH values:\n")
          print(DBH[sel2])
          cat("Input H values:\n")
          print(H[sel2])
          cat("Output:\n")
          print(b[is.na(b)])
        }
        warning(paste("Warning: NA values in stem biomass estimation for",sum(is.na(H[sel])), "records."))
      } else {
        if(sum(b<0)>0) {
          warning(paste0(sum(b<0)," negative stem biomass values truncated to zero for taxon ", taxon, " with model ", par_stem[1,"Model"],"."))
          b[b<0] = 0
        }
      }
      if(as.CO2) b = b*(perc.CO2/100)*(44/12) # from biomass as kg dry weight to biomass as kg of CO2
      df$Stem[sel] = b*N[sel]
    }
    par_branches = .getBiomassParams(taxon, "Branches", area)
    if(nrow(par_branches)>0) {
      df$Branches[sel] = 0
      for(j in 1:nrow(par_branches)) {
        b = .biomass(DBH[sel], H[sel], as.list(par_branches[j,]))
        if(as.CO2) b = b*(perc.CO2/100)*(44/12) # from biomass as kg dry weight to biomass as kg of CO2
        df$Branches[sel] = df$Branches[sel] + b*N[sel]
      }
    }

    par_roots = .getBiomassParams(taxon, "Roots", area)
    if(nrow(par_roots)>0) {
      b = .biomass(DBH[sel], H[sel], as.list(par_roots[1,]))
      if(as.CO2) b = b*(perc.CO2/100)*(44/12) # from biomass as kg dry weight to biomass as kg of CO2
      df$Roots[sel] = b*N[sel]
    }

    par_leaves = .getBiomassParams(taxon, "Leaves", area)
    if(nrow(par_leaves)>0) {
      b = .biomass(DBH[sel], H[sel], as.list(par_leaves[1,]))
      if(as.CO2) b = b*(perc.CO2/100)*(44/12) # from biomass as kg dry weight to biomass as kg of CO2
      df$Leaves[sel] = b*N[sel]
    }

    par_needles = .getBiomassParams(taxon, "Needles", area)
    if(nrow(par_needles)>0) {
      b = .biomass(DBH[sel], H[sel], as.list(par_needles[1,]))
      if(as.CO2) b = b*(perc.CO2/100)*(44/12) # from biomass as kg dry weight to biomass as kg of CO2
      df$Needles[sel] = b*N[sel]
    }

    par_bark = .getBiomassParams(taxon, "Bark", area)
    if(nrow(par_bark)>0) {
      b = .biomass(DBH[sel], H[sel], as.list(par_bark[1,]))
      if(as.CO2) b = b*(perc.CO2/100)*(44/12) # from biomass as kg dry weight to biomass as kg of CO2
      df$Bark[sel] = b*N[sel]
    }

    # Sum total biomass
    df$Total[sel] = rowSums(df[sel, c("Leaves", "Needles","Branches", "Stem", "Bark", "Roots")], na.rm=TRUE)

    #If separate allometry, recalculate total biomass
    par_total = .getBiomassParams(taxon, "Total", area)
    if(nrow(par_total)>0) {
      b = .biomass(DBH[sel], H[sel], as.list(par_total[1,]))
      if(as.CO2) b = b*(perc.CO2/100)*(44/12) # from biomass as kg dry weight to biomass as kg of CO2
      df$Total[sel] = b*N[sel]
    }

    #If aerial allometry, calculate aerial biomass, otherwise substract roots from total if possible
    par_aerial = .getBiomassParams(taxon, "Aerial", area)
    if(nrow(par_aerial)>0) {
      b = .biomass(DBH[sel], H[sel], as.list(par_aerial[1,]))
      if(as.CO2) b = b*(perc.CO2/100)*(44/12) # from biomass as kg dry weight to biomass as kg of CO2
      df$Aerial[sel] = b*N[sel]
    }
  }

  # If Aerial is not estimated, estimate it as difference
  na_aer = is.na(df$Aerial)
  if(sum(na_aer)>0){
    dif = df$Total - df$Roots
    df$Aerial[na_aer] = dif[na_aer]
  }
  if(!is.null(DBHclasses)) {
    df$DBHclass = cut(x$DBH, DBHclasses)
    df<-as.data.frame(df  %>% dplyr::group_by(ID, Species, Name, SpeciesAllom, NameAllom, DBHclass)  %>%
                        dplyr::summarize(Roots = sum(Roots, na.rm=T), Stem = sum(Stem, na.rm= T), Branches = sum(Branches, na.rm=T),
                                         Leaves = sum(Leaves, na.rm=T), Needles  = sum(Needles , na.rm=T), Bark = sum(Bark, na.rm=T),
                                         Aerial = sum(Aerial, na.rm=T), Total = sum(Total, na.rm=T)))

  }

  return(df)
}
