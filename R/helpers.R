preprocess <- function(file_path){
  
  # Holiday variables
  hol_vars = c("JUL4.HOL","J4LG.HOL","J4LD.HOL","MEMD.HOL","MDLG.HOL","MDLD.HOL","NYR.HOL","NYLG.HOL","NYLD.HOL","XMAS.HOL","XMLG.HOL","XMLD.HOL","THNK.HOL","THLG.HOL","THLD.HOL","LABD.HOL","LBLG.HOL","LBLD.HOL","EAST.HOL","EALG.HOL","EALD.HOL","CINCO.HOL","M5LG.HOL","M5LD.HOL","SBWL.HOL","SBWLG.HOL","SBWLD.HOL")
  
  # Read data
  dt = read_sav(file_path)
  dt = as.data.table(dt)
  setnames(dt, names(dt), toupper(names(dt)))
  
  # Create covid dummies
  # covid dummy variables
  dt[, COVID19.DUMMY:=ifelse(WEEK >= "2020-03-14", 1, 0)]
  setnames(dt, "MKT", "MKT_")

  dt[, COVID19Lockdown_march15and22:=ifelse(WEEK  %between% c("2020-03-15", "2020-03-22"), 1, 0)]
  dt[, MKT_:=as.character(MKT_)]
    
  dt[, COVID_CASES:=ifelse(is.na(COVID_CASES), 0, COVID_CASES)]

  # Coerce HOL vars to factors
  for(j in hol_vars) {
    set(dt, j=j, value=factor(dt[[j]], levels=c(1, 0)))
  }

  # Total marca - total segmentos
  # dt[, QKR_REST.EQU:=QUAKERCHEWY.EQU - sum()]
  dt
}

find_var <- function(dt, s) {
  grep(s, names(dt), v=T, ignore.case=T)
}


elast_matrix <- function(models, all_segments){
  elast_list = lapply(models, function(d) d$elast[, .(Coefficient)])
  elast = do.call(cbind, elast_list)
  setnames(elast, all_segments)
  cbind(x=all_segments, elast)
}
