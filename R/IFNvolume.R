#' Volume allometry models
#'
#' Static models for the wood volume (VCC, VSC, VLE & IAVC) by species and province in the IFN
#'
#' @param x A data frame with tree records in rows and columns 'Provincia' (numeric), 'Species', 'DBH' (in cm), 'H' (in m) and 'N' (ha-1).
#' Additionally, the data frame may include a column 'FC' that specifies the cubic content form for each tree. In this case, the value of parameter 'FC' is overriden.
#' @param IFN Integer or integer pair to indicate order of preferred forest inventory:
#'      (\code{IFN=2} indicates using IFN-2 only, \code{IFN=3} indicates using IFN-3 only, and \code{IFN=c(3,2)} indicates using IFN3 and if not available IFN2)
#' @param FC Vector of integers to indicate preferred cubic content forms (1 to 6)
#' \itemize{
#'    \item{Forma 1. Arboles fusiformes prácticamente en todo su fuste, con troncos maderables,
#'    limpios y derechos de más de 6 m, flecha inferior al 1% de su longitud, veta no torcida
#'    y diámetro normal mayor de 20 cm.}
#'    \item{Forma 2. Arboles que cumplan las cuatro condiciones siguientes: ser fusiformes, tener
#'    troncos maderables de 4 o más metros, ramificarse por la parte superior y no pertenecer
#'    a la forma 1.}
#'    \item{Forma 3. Arboles fusiformes pequeños, en los que el diámetro del fuste de 75 mm
#'    queda por debajo de los 4 m de altura.}
#'    \item{Forma 4. Árboles cuyo tronco principal se ramifica antes de los 4 m de altura y que
#'    pertenezcan a algunas de las siguientes especies 07, 12, 16, 23, 41, 42, 43, 44, 45, 46,
#'    47, 48, 49, 55, 56, 57, 66, 67, 71, 72, 74, 75, 79 y 94.}
#'    \item{Forma 5. Arboles cuyo tronco principal es tortuoso, está dañado o es muy ramoso, por
#'    lo que no admite la clasificación en formas 1, 2 o 3. También pies de altura de fuste
#'    menor de 4 m si son de especies diferentes a las de los códigos 4 y 6.}
#'    \item{Forma 6. Árboles descabezados o trasmochos a los que se les ha cortado la parte
#'    superior del tronco y las ramas en puntos próximos a su inserción en el tronco y que
#'    pertenezcan a algunas de las siguientes especies: 41, 42, 43, 55, 56, 71, 72 y 94.}
#' }
#' @param code_missing Species code to use when equations are not available for the species recorded ("99" is Otras frondosas)
#' @param provinceFromID A flag to indicate that province should be extracted from 'ID' assuming they are IFN plot codes.
#' @param DBHclasses A numeric vector of DBH class limits (see breaks in function \code{cut}), used to group results by DBH class (in addition to species and plot). If \code{DBHclasses = NULL} then grouping on the basis of DBH classes is not performed.
#' @param verbose A flag to indicate console output of the volume calculation process
#'
#' @return If \code{DBHclasses = NULL}, a data frame with as many rows as tree records in \code{x} and columns:
#' \itemize{
#'   \item{\code{ID}: Plot identifier}
#'   \item{\code{Species}: Species code or species name}
#'   \item{\code{Name}: Species name}
#'   \item{\code{FC}: Cubic content form used to calculate volume.}
#'   \item{\code{VCC}: Volumen con corteza (m3/ha).}
#'   \item{\code{VSC}: Volumen sin corteza (m3/ha).}
#'   \item{\code{VLE}: Volumen de leñas (m3/ha).}
#'   \item{\code{IAVC}: Incremento anual del volumen con corteza (m3/ha/yr).}
#' }
#' If \code{DBHclasses != NULL} then an extra column \code{DBHclass} is given and the data frame has less rows than tree records in \code{x}.
#'
#' @details The volumetric equation used for each tree record depends on province, species, cubic content form and volume parameter (VCC, VSC, VLE and IAVC).
#' Volumes are given as per hectare (i.e. the result of the volumetric equation is multiplied by the density 'N'). If cubic content form is not given in 'x', then
#' the function iterates over the values of 'FC' until an equation is available.
#'
#' @examples
#' data(exampleTreeData)
#'
#' IFNvolume(exampleTreeData)
#'
#' # Groups the result by DBH clases
#' IFNvolume(exampleTreeData, provinceFromID = TRUE,
#'           DBHclasses = seq(0, 120, by=5))
IFNvolume<-function(x, IFN = c(3,2), FC = 1:6, code_missing = "99",
                    provinceFromID = FALSE, DBHclasses = NULL,
                    verbose=FALSE) {
  Species = x$Species
  if(!("DBH" %in% names(x))) stop("Column 'DBH' missing in 'x'")
  if(!("N" %in% names(x))) stop("Column 'N' missing in 'x'")
  if(!("H" %in% names(x))) stop("Column 'H' missing in 'x'")
  DBH = x$DBH*10 #From cm to mm (models are calibrated with these units)
  H = x$H
  N = x$N
  if("FC" %in% names(x)) {
    fc = x$FC
  } else {
    fc = rep(NA, length(DBH))
  }

  if(provinceFromID) {
    Province = .getProvinceFromID(x$ID)
  } else {
    if(!("Provincia" %in% names(x)) && !("Province" %in% names(x))) stop("Column 'Provincia' or 'Province' missing in 'x'")
    if("Provincia" %in% names(x)) Province = x$Provincia
    else Province = x$Province
  }
  nr = nrow(x)
  df = data.frame(ID = x$ID, Species = x$Species, Name = rep(NA,nr),
                  FC = fc,
                  VCC = rep(NA, nr), VSC = rep(NA, nr),
                  VLE = rep(NA,nr), IAVC = rep(NA, nr))
  prov_tax_fc = paste(Province, Species, fc, sep="_")
  un_prov_tax_fc = unique(prov_tax_fc)
  if(verbose) {
    pb = txtProgressBar(1, length(un_prov_tax_fc), style=3)
  }
  for(i in 1:length(un_prov_tax_fc)) {
    if(verbose) setTxtProgressBar(pb, i)
    s = strsplit(un_prov_tax_fc[i],"_")[[1]]
    provincei = as.numeric(s[1])
    taxoni <- s[2]
    if(nchar(taxoni)<=4) taxoni <- as.numeric(taxoni)
    sel = (prov_tax_fc==un_prov_tax_fc[i])

    # print(provincei)
    # print(taxoni)
    fci = s[3]
    if(fci=="NA") fci=NA
    if(!is.na(fci)) {
      par_vcc = .getVolumeParams(IFN, provincei, taxoni, "VCC", fci, code_missing)
    }
    else {
      found = FALSE
      #Try with input FC
      j=1
      while((!found) && (j<=length(FC))) {
        par_vcc = .getVolumeParams(IFN, provincei, taxoni, "VCC", FC[j], code_missing)
        found = (nrow(par_vcc)>0)
        if(found) {
          df[sel,"FC"] = FC[j]
          fci = FC[j]
        }
        j = j+1
      }
    }
    par_vsc = .getVolumeParams(IFN, provincei, taxoni, "VSC", fci, code_missing)
    par_vle = .getVolumeParams(IFN, provincei, taxoni, "VLE", fci, code_missing)
    par_iavc = .getVolumeParams(IFN, provincei, taxoni, "IAVC", fci, code_missing)

    vcc = rep(NA, sum(sel))
    if(nrow(par_vcc)>0){
      df$Name[sel] = par_vcc[1,"Species"]
      vcc = .volume(DBH[sel], H[sel], NA, as.list(par_vcc[1,]))
      df$VCC[sel] = vcc*N[sel]*0.001 #from dm3 to m3
    }
    if(nrow(par_vsc)>0) {
      df$VSC[sel] = .volume(DBH[sel], H[sel], vcc, as.list(par_vsc[1,]))*N[sel]*0.001 #from dm3 to m3
    }
    if(nrow(par_vle)>0) {
      df$VLE[sel] = .volume(DBH[sel], H[sel], vcc, as.list(par_vle[1,]))*N[sel]*0.001 #from dm3 to m3
    }
    if(nrow(par_iavc)>0) {
      df$IAVC[sel] = .volume(DBH[sel], H[sel], vcc, as.list(par_iavc[1,]))*N[sel]*0.001 #from dm3 to m3
    }
  }
  if(!is.null(DBHclasses)) {
    df$DBHclass = cut(x$DBH, DBHclasses)
    df<-as.data.frame(df  %>% dplyr::group_by(ID, Species, Name, DBHclass, FC)  %>%
      dplyr::summarize(VCC = sum(VCC, na.rm=T), VSC = sum(VSC, na.rm= T), VLE = sum(VLE, na.rm=T),
                       IAVC = sum(IAVC, na.rm=T)))

  }
  return(df)
}
