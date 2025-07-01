.getSpainProv<-function() {
  SpainProv = c("01","02","03","04","05","06","07", "08","09","10",
                "11","12","13","14","15","16","17", "18","19","20",
                "21","22","23","24","25","26","27", "28","29","30",
                "31","32","33","34","35","36","37", "38","39","40",
                "41","42","43","44","45","46","47", "48","49","50")
  return(SpainProv)
}
.getProvinceFromID<-function(x) {
  return(substr(x,1, nchar(x)-4))
}
.biomassModel1<-function(DBH, H, alpha, a, b, c) {
  return(as.numeric(alpha + a*(DBH^b)*(H^c)))
}
.biomassModel2<-function(DBH, H, alpha, t, u, v, w) {
  return(as.numeric(alpha + t*(DBH^u)+v*(H^w)))
}
.biomassModel3<-function(DBH, H, Zth, g,i,j,k,l,m,n) {
  Z = ifelse(DBH>Zth, 1,0)
  return(as.numeric(Z*(g*((i+DBH)^j)+k*((l+DBH)^m)*(H^n))))
}
.biomassModel4<-function(DBH, H, alpha, p, q) {
  return(as.numeric(alpha + p*(DBH^2)*H+ q*(DBH*H)))
}
.biomassModel5<-function(DBH, H, e, f) {
  return(as.numeric(e*(((DBH^2)*H)^f)))
}
.biomassModel6<-function(DBH, H, alpha, r, s, x) {
  return(as.numeric(alpha+ (r*(DBH^2))+ (s*H)+ ((x*(DBH^2))*H)))
}
.biomassModel7<-function(DBH, beta, gamma) {
  return(as.numeric(beta*exp(gamma*DBH)))
}

.biomass<-function(DBH, H, par) {
  model = par[["Model"]]
  if(model==1) {
    return(.biomassModel1(DBH, H, par[["alpha"]], par[["a"]], par[["b"]], par[["c"]]))
  }
  else if(model==2) {
    return(.biomassModel2(DBH, H, par[["alpha"]], par[["t"]], par[["u"]], par[["v"]], par[["w"]]))
  }
  else if(model==3) {
    return(.biomassModel3(DBH, H, par[["Zth"]], par[["g"]], par[["i"]], par[["j"]], par[["k"]], par[["l"]], par[["m"]], par[["n"]]))
  }
  else if(model==4) {
    return(.biomassModel4(DBH, H, par[["alpha"]], par[["p"]], par[["q"]]))
  }
  else if(model==5) {
    return(.biomassModel5(DBH, H, par[["e"]], par[["f"]]))
  }
  else if(model==6) {
    return(.biomassModel6(DBH, H, par[["alpha"]], par[["r"]], par[["s"]], par[["x"]]))
  }
  else if(model==7) {
    return(.biomassModel7(DBH, par[["beta"]], par[["gamma"]]))
  }
  cli::cli_abort("Wrong parameter 'model' (must be an integer between 1 and 7)")
}

.getBiomassParams<-function(sp, compartment, area = NA) {
  sel = biomass_parameters$ID_TAXON==sp & biomass_parameters$Compartment==compartment
  if(!is.na(area)) {
    selArea = biomass_parameters$Area==area
    selArea[is.na(selArea)] = TRUE
    sel = sel & selArea
  }
  df = biomass_parameters[sel,]
  return(df)
}



.volumeModel1<-function(DBH, HT, a, b, c) {
  return(as.numeric(a + b*(DBH^2)*HT))
}
.volumeModel7<-function(VCC, a, b, c) {
  return(as.numeric(a + b*VCC + c*(VCC^2)))
}
.volumeModel8<-function(VCC, a, b, c) {
  return(as.numeric(a + b*VCC + c*(VCC^2)))
}
.volumeModel9<-function(DBH, q) {
  return(as.numeric(DBH^q))
}
.volumeModel10<-function(VCC, a, b, c) {
  return(as.numeric(a + b*VCC + c*(VCC^2)))
}
.volumeModel11<-function(DBH, HT, p, q, r) {
  return(as.numeric(p*(DBH^q)*(HT^r)))
}
.volumeModel12<-function(DBH, p, q) {
  return(as.numeric(p*(DBH^q)))
}
.volumeModel13<-function(DBH, a, b, dnm) {
  return(as.numeric(a+b*(DBH-dnm)))
}
.volumeModel14<-function(DBH, p, q) {
  return(as.numeric(p*(DBH^q)))
}
.volumeModel15<-function(DBH, a, b, dnm) { # TO DO!!!!
  return(as.numeric(NA))
}
.volumeModel16<-function(DBH, a, b) {
  return(as.numeric(a+b*(DBH^2)))
}
.volumeModel17<-function(DBH, a, b,c) {
  return(as.numeric(a+b*DBH+c*(DBH^2)))
}
.volumeModel18<-function(DBH, p, q) {
  return(as.numeric(p*exp(q*DBH)))
}
.volumeModel19<-function(DBH, a, b, c, d) {
  return(as.numeric(a + (b*DBH)+(c*(DBH^2))+ (d*(DBH^3))))
}
.volumeModel20<-function(DBH, a, b, d) {
  return(as.numeric(a + (b*DBH)+ (d*(DBH^3))))
}
.volumeModel21<-function(DBH, c, d) {
  return(as.numeric((c*(DBH^2))+ (d*(DBH^3))))
}
.volumeModel25<-function(DBH, HT, p, q, r) {
  return(as.numeric(p*(DBH^q)*(HT^r)))
}

.volume<-function(DBH, HT, VCC, par) {
  model = par[["Model"]]
  if(model==1) {
    return(.volumeModel1(DBH, HT, par[["a"]], par[["b"]], par[["c"]]))
  }
  else if(model==7) {
    return(.volumeModel7(VCC, par[["a"]], par[["b"]], par[["c"]]))
  }
  else if(model==8) {
    return(.volumeModel8(VCC, par[["a"]], par[["b"]], par[["c"]]))
  }
  else if(model==9) { #No formula available!
    return(.volumeModel9(DBH, par[["r"]]))
  }
  else if(model==10) {
    return(.volumeModel10(VCC, par[["a"]], par[["b"]], par[["c"]]))
  }
  else if(model==11) {
    return(.volumeModel11(DBH, HT, par[["p"]], par[["q"]], par[["r"]]))
  }
  else if(model==12) {
    return(.volumeModel12(DBH, par[["p"]], par[["q"]]))
  }
  else if(model==13) {
    return(.volumeModel13(DBH, par[["a"]], par[["b"]], par[["dnm"]]))
  }
  else if(model==14) {
    return(.volumeModel14(DBH, par[["p"]], par[["q"]]))
  }
  else if(model==15) {
    return(.volumeModel15(DBH, par[["a"]], par[["b"]], par[["dnm"]]))
  }
  else if(model==16) {
    return(.volumeModel16(DBH, par[["a"]], par[["b"]]))
  }
  else if(model==17) {
    return(.volumeModel17(DBH, par[["a"]], par[["b"]], par[["c"]]))
  }
  else if(model==18) {
    return(.volumeModel18(DBH, par[["p"]], par[["q"]]))
  }
  else if(model==19) {
    return(.volumeModel19(DBH, par[["a"]], par[["b"]], par[["c"]], par[["d"]]))
  }
  else if(model==20) {
    return(.volumeModel20(DBH, par[["a"]], par[["b"]], par[["d"]]))
  }
  else if(model==21) {
    return(.volumeModel21(DBH, par[["c"]], par[["d"]]))
  }
  else if(model==25) {
    return(.volumeModel25(DBH, HT, par[["p"]], par[["q"]], par[["r"]]))
  }
  cli::cli_abort(paste("Wrong parameter 'model' ", model))
}
.getVolumeParams<-function(ifn, prov, sp, param, fc, code_missing = "99") {
  # Select IFN, parameter and FC
  sel <- (volume_parameters$IFN %in% ifn) & (volume_parameters$PARAM==param) & (volume_parameters$FC==fc)
  sel[is.na(sel)] = FALSE
  df <- volume_parameters[sel,]
  if(nrow(df)>0) { # select province species
    sel <- rep(FALSE, nrow(df))
    if(is.numeric(sp)) {
      code_s <- strsplit(as.character(df$ID_TAXON), split=",")
      for(i in 1:length(code_s)) if(as.character(sp) %in% code_s[[i]]) sel[i] <- TRUE
    } else {
      spname_s = strsplit(as.character(df$Species), split=",")
      for(i in 1:length(spname_s)) if(as.character(sp) %in% spname_s[[i]]) sel[i] <- TRUE
    }
    if(sum(sel)==0) {
      sel[(df$Province==prov) & (df$ID_TAXON==code_missing)] = TRUE #Set otras frondosas (code = "99") to true
    } else {
      sel2 = sel & (df$Province==prov) # add province filter if possible (otherwise first province will be used)
      if(sum(sel2)>0) sel = sel2
    }
    return(df[sel,])
  }
  return(df)
}

