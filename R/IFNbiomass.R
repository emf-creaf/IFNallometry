#' Biomass allometry models
#'
#' Static models for the biomass (kg/ha) of stem, roots, branches, leaves/needles and bark of species in the IFN
#'
#' @param x A data frame with tree records in rows and columns:
#'    \itemize{
#'      \item{\code{ID}: String identifying forest stand.}
#'      \item{\code{Species}: Species numeric used in IFN or a species name matching names given in \code{\link{species_ifn}}.}
#'      \item{\code{DBH}: Tree diameter at breast height (in cm).}
#'      \item{\code{H}: Tree height (in m).}
#'      \item{\code{N}: Tree density factor (in ind/ha).}
#'    }
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
#' When species are provided as character names, both exact matching and function \code{\link{startsWith}} are used.
#'
#' @name IFNbiomass
#' @return If \code{DBHclasses = NULL}, function \code{IFNbiomass} returns a data frame with as many rows as tree records in \code{x} and columns 'ID', 'Species', 'Roots', 'Stem', 'Branches', 'Leaves', 'Needles', 'Bark', 'Aerial' and 'Total'.
#' If \code{DBHclasses != NULL} then an extra column \code{DBHclass} is given and the data frame has less rows than tree records in \code{x}.
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
#'
#' @seealso \code{\link{IFNvolume}}
#' @export
#' @examples
#' data(exampleTreeData)
#'
#' IFNbiomass(exampleTreeData)
#'
#' # Groups the result by DBH clases
#' IFNbiomass(exampleTreeData,
#'            DBHclasses = seq(0, 120, by=5))
IFNbiomass<-function(x, as.CO2 = FALSE, area = NA,
                     DBHclasses = NULL, verbose = FALSE) {
  if(!("ID" %in% names(x))) cli::cli_abort("Column 'ID' missing in 'x'")
  if(!("Species" %in% names(x))) cli::cli_abort("Column 'Species' missing in 'x'")
  if(!("DBH" %in% names(x))) cli::cli_abort("Column 'DBH' missing in 'x'")
  if(!("N" %in% names(x))) cli::cli_abort("Column 'N' missing in 'x'")
  if(!("H" %in% names(x))) cli::cli_abort("Column 'H' missing in 'x'")
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
    cli::cli_progress_bar("Species", total = nu)
  }
  for(i in 1:nu) {
    if(verbose) cli::cli_progress_update()
    spu = sp_unique[i]
    sel = (Species==spu)
    if(nchar(spu)>4) {
      if(spu %in% species_ifn$Species) {
        spu <- species_ifn$IFNcode[species_ifn$Species == spu][1]
      } else if(any(startsWith(species_ifn$Species,spu))) {
        spu <- species_ifn$IFNcode[startsWith(species_ifn$Species,spu)][1]
      } else {
        cli::cli_abort(paste0("Species name '", spu,"' not found in 'species_ifn'."))
      }
    }
    perc.CO2 =  biomass_species_match$PORCENTAJE_CARBONO[biomass_species_match$ID_TAXON == spu]
    taxon =  as.numeric(biomass_species_match$ID_TAXON2[biomass_species_match$ID_TAXON == spu])
    if(length(taxon)!=1) cli::cli_abort(paste0("Species code '", spu,"' not found in biomass match table."))
    # cat(paste("Taxon",spu," #", sum(sel)))
    df$Name[sel] <- biomass_species_match$LATIN_TAXON[biomass_species_match$ID_TAXON == spu]
    df$SpeciesAllom[sel] <- taxon
    df$NameAllom[sel] <- biomass_species_match$LATIN_TAXON[biomass_species_match$ID_TAXON == taxon]
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
        cli::cli_alert_warning(paste("NA values in stem biomass estimation for",sum(is.na(H[sel])), "records."))
      } else {
        if(sum(b<0)>0) {
          cli::cli_alert_warning(paste0(sum(b<0)," negative stem biomass values truncated to zero for taxon ", taxon, " with model ", par_stem[1,"Model"],"."))
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


#' Wrapper biomass function for packages medfate and medfateland
#'
#' Wrapper function to be used to calculate tree biomass for packages medfate and medfateland.
#'
#' @param x Data frame of tree data or \code{forest} object from package medfate.
#' @param SpParams Data frame of species parameters suitable for medfate package (not used).
#' @param area Either 'Atlantic' or 'Mediterranean' to specify allometric equations specific to the area (for Pinus pinaster)
#' @param fraction A string, either "total" (for total biomass), "aboveground" (for aboveground biomass) or "belowground" (for belowground biomass).
#' @param level A string, either "cohort" (for tree cohort-level biomass) or "stand" (for stand-level biomass).
#'
#' @returns A vector of biomass of each tree cohort (in Mg/ha of dry weight) to be used in medfate or medfateland packages.
#'
#' @details Values for parameter \code{x} will be supplied by the calling function (e.g. \code{modify_forest_structure}). Values for parameters
#' \code{area}, \code{fraction} and \code{level} (if they defaults need to be changed) should be supplied using as a list to parameter \code{biomass_arguments}.
#'
#' @seealso \code{\link{IFNbiomass}}
#' @export
#' @examples
#' if(require("medfate")) {
#'   IFNbiomass_medfate(exampleforest, SpParamsMED,
#'                      fraction = "aboveground",
#'                      level = "stand")
#' }
#'
IFNbiomass_medfate<-function(x, SpParams,
                             area = NA,
                             fraction = "total",
                             level = "cohort"){
  if(inherits(x, "forest")) x <- x$treeData
  fraction <- match.arg(fraction, c("total", "aboveground", "belowground"))
  level <- match.arg(level, c("cohort", "stand"))
  ntree <- nrow(x)
  if(ntree>0) {
    y <- data.frame(ID = rep("XX", ntree),
                    Species = x$Species,
                    DBH = x$DBH,
                    H = x$Height/100, # Height is in cm in medfate
                    N = x$N
    )
    biomass_df <- IFNallometry::IFNbiomass(y, as.CO2 = FALSE)
    if(fraction=="total") {
      bio <- biomass_df$Total
    } else if(fraction=="aboveground") {
      bio <- biomass_df$Aerial
    } else if(fraction=="belowground") {
      bio <- biomass_df$Total - biomass_df$Aerial
    }
    if(level == "stand") bio <- sum(bio)
    return(bio/1000) #From kg/ha to Mg/ha
  }
  return(numeric(0))
}
