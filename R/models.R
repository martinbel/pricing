fit_enet_model <- function(dt, 
                           alpha=1,
                           segment='C18CT',  
                           all_segments){
  # LUH: Log(Unit stz)
  # LBU: Log(Base price per unit)
  
  # Define y - X
  y_var = sprintf("%s.LUH", segment)
  xseg = paste0(segment, ".LBU")
  
  # X
  x_vars = paste0(segment, ".", c("DRT","FCT","LTDP"))
  x_vars_segment = paste0(all_segments, ".", "LBU")
  x_vars = unique(c(x_vars, x_vars_segment))
  
  # Holiday variables
  hol_vars = c("JUL4.HOL","J4LG.HOL","J4LD.HOL","MEMD.HOL","MDLG.HOL","MDLD.HOL","NYR.HOL","NYLG.HOL","NYLD.HOL","XMAS.HOL","XMLG.HOL","XMLD.HOL","THNK.HOL","THLG.HOL","THLD.HOL","LABD.HOL","LBLG.HOL","LBLD.HOL","EAST.HOL","EALG.HOL","EALD.HOL","CINCO.HOL","M5LG.HOL","M5LD.HOL","SBWL.HOL","SBWLG.HOL","SBWLD.HOL")
  
  # Select data
  data = copy(dt[, c(y_var, "COVID19.DUMMY", "COVID19Lockdown_march15and22", 
                     "SEA7.SON", "COVID_CASES", hol_vars, 'MKT_', 
                     x_vars, "CAT.WGT"), wi=F])

  # Saco NAs de la variable dependiente
  data = data[!is.na(get(y_var))]
  
  # Remove NAs from x-segment
  data = data[!is.na(get(xseg))]
  weights = as.numeric(data$CAT.WGT)
  
  # Weights
  data[, CAT.WGT:=NULL]
  
  # Imputo NAs
  nas = colSums(is.na(data))
  na_cols = names(nas[nas > 0])
  for(j in na_cols){
    set(data, j=j, value=ifelse(is.na(data[[j]]), 0, data[[j]]))
  }
  
  #cat("Total NAs: ", sum(is.na(data)), "\n")
  
  # Create vars
  data[, (paste0(segment, "_CovidCases.LBU")):=ifelse(is.na(COVID_CASES), 0, get(xseg) * COVID_CASES)]
  # data[, (paste0(segment, "_Covid.LBU")):=get(xseg) * COVID19.DUMMY]
  data[, COVID_CASES:=NULL]
  
  cor_mat = cor(data[, c(y_var, x_vars_segment), wi=F])
  
  # Fit model
  fml = as.formula(sprintf("%s ~ 0 + .", y_var))
  X_mm = model.matrix(fml, data=data)
  y = as.numeric(data[[y_var]])
  
  enet_fit = cv.glmnet(X_mm, y, alpha=alpha, 
                       family='gaussian',
                       type.measure = "mae", 
                       nfolds = 10, weights=weights, 
                       standardize=TRUE)
  plot(enet_fit)
  
  # Evaluation metrics
  best_inx = which(enet_fit$glmnet.fit$lambda == enet_fit$lambda.min)
  r2 = enet_fit$glmnet.fit$dev.ratio[best_inx] 
  mae = enet_fit$cvm[best_inx] 
  summary = data.table(r.squared=r2, mae=mae)
  
  # Coefficients
  coefs = as.matrix(coefficients(enet_fit, s=enet_fit$lambda.min))
  coefs = data.table(Variable=rownames(coefs), Coefficient=as.numeric(coefs[,1]))
  
  # elasticity coefficients
  elast = coefs[Variable %in% paste0(all_segments,".LBU")]
  
  list(coefs=coefs, fit=enet_fit, summary=summary, elast=elast, cor_mat=cor_mat)
}


fit_enet_brand <- function(dt, alpha=0, segment='QUAKERCHEWY'){
  # Modelo de marca
  # log(volumen estandarizado) en onzas
  # precio por onzas .LBP Log(Base price per onz)
  # LEQH
  # QUAKERCHEWY
  # QUAKERCHEWY.LEQH ~ QUAKERCHEWY.LBP
  
  # Define y - X
  y_var = sprintf("%s.LEQH", segment)
  xseg = paste0(segment, ".LBP")
  
  # X
  x_vars = paste0(segment, ".", c("DRT","FCT","LTDP"))
  x_vars = c(xseg, x_vars)
  #x_vars_segment = paste0(all_segments, ".", "LBU")
  #x_vars = unique(c(x_vars, x_vars_segment))
  
  # Holiday variables
  hol_vars = c("JUL4.HOL","J4LG.HOL","J4LD.HOL","MEMD.HOL","MDLG.HOL","MDLD.HOL","NYR.HOL","NYLG.HOL","NYLD.HOL","XMAS.HOL","XMLG.HOL","XMLD.HOL","THNK.HOL","THLG.HOL","THLD.HOL","LABD.HOL","LBLG.HOL","LBLD.HOL","EAST.HOL","EALG.HOL","EALD.HOL","CINCO.HOL","M5LG.HOL","M5LD.HOL","SBWL.HOL","SBWLG.HOL","SBWLD.HOL")
  
  # Select data
  data = copy(dt[, c(y_var, "SEA7.SON", "COVID_CASES", hol_vars,
                     'MKT_', "CAT.WGT", x_vars, 
                     "COVID19.DUMMY", "COVID19Lockdown_march15and22"), wi=F])
  
  # str(data)
  
  # Saco NAs de la variable dependiente
  data = data[!is.na(get(y_var))]
  
  # Remove NAs from x-segment
  data = data[!is.na(get(xseg))]
  weights = as.numeric(data$CAT.WGT)
  
  # Weights
  data[, CAT.WGT:=NULL]
  
  cor_mat = cor(data[, c(y_var, x_vars), wi=F])
  
  # Imputo NAs
  nas = colSums(is.na(data))
  na_cols = names(nas[nas > 0])
  for(j in na_cols){
    set(data, j=j, value=ifelse(is.na(data[[j]]), 0, data[[j]]))
  }
  
  #cat("Total NAs: ", sum(is.na(data)), "\n")
  
  # Create vars
  data[, (paste0(segment, "_CovidCases.LBU")):=ifelse(is.na(COVID_CASES), 0, get(xseg) * COVID_CASES)]
  # data[, (paste0(segment, "_Covid.LBU")):=get(xseg) * COVID19.DUMMY]
  data[, COVID_CASES:=NULL]
  
  # Fit model
  fml = as.formula(sprintf("%s ~ 0 + .", y_var))
  X_mm = model.matrix(fml, data=data)
  y = as.numeric(data[[y_var]])
  
  enet_fit = cv.glmnet(X_mm, y, alpha=alpha, 
                       family='gaussian',
                       type.measure = "mae", 
                       nfolds = 10, weights=weights, 
                       standardize=TRUE)
  plot(enet_fit)
  
  # Evaluation metrics
  best_inx = which(enet_fit$glmnet.fit$lambda == enet_fit$lambda.min)
  r2 = enet_fit$glmnet.fit$dev.ratio[best_inx] 
  mae = enet_fit$cvm[best_inx] 
  summary = data.table(r.squared=r2, mae=mae)
  
  # Coefficients
  coefs = as.matrix(coefficients(enet_fit, s=enet_fit$lambda.min))
  coefs = data.table(Variable=rownames(coefs), Coefficient=as.numeric(coefs[,1]))
  
  # elasticity coefficients
  elast = coefs[Variable %in% xseg]
  
  list(coefs=coefs, fit=enet_fit, summary=summary, elast=elast, cor_mat=cor_mat)
}


fit_lm_brand <- function(dt, segment='QUAKERCHEWY'){
  # Modelo de marca
  # log(volumen estandarizado) en onzas
  # precio por onzas .LBP Log(Base price per onz)
  # LEQH
  # QUAKERCHEWY
  # QUAKERCHEWY.LEQH ~ QUAKERCHEWY.LBP
  
  # Define y - X
  y_var = sprintf("%s.LEQH", segment)
  xseg = paste0(segment, ".LBP")
  
  # X
  x_vars = paste0(segment, ".", c("DRT","FCT","LTDP"))
  #x_vars = c(x_vars)
  #x_vars_segment = paste0(all_segments, ".", "LBU")
  #x_vars = unique(c(x_vars, x_vars_segment))
  
  # Holiday variables
  hol_vars = c("JUL4.HOL","J4LG.HOL","J4LD.HOL","MEMD.HOL","MDLG.HOL","MDLD.HOL","NYR.HOL","NYLG.HOL","NYLD.HOL","XMAS.HOL","XMLG.HOL","XMLD.HOL","THNK.HOL","THLG.HOL","THLD.HOL","LABD.HOL","LBLG.HOL","LBLD.HOL","EAST.HOL","EALG.HOL","EALD.HOL","CINCO.HOL","M5LG.HOL","M5LD.HOL","SBWL.HOL","SBWLG.HOL","SBWLD.HOL")
  
  # Select data
  data = copy(dt[, c(y_var, "SEA7.SON", "COVID_CASES", hol_vars,
                     'MKT_', "CAT.WGT", x_vars, xseg,
                     "COVID19.DUMMY", "COVID19Lockdown_march15and22"), wi=F])
  
  # str(data)
  
  # Saco NAs de la variable dependiente
  data = data[!is.na(get(y_var))]
  
  # Remove NAs from x-segment
  data = data[!is.na(get(xseg))]
  weights = as.numeric(data$CAT.WGT)
  
  # Weights
  data[, CAT.WGT:=NULL]
  
  cor_mat = cor(data[, c(y_var, x_vars), wi=F])
  
  # Imputo NAs
  nas = colSums(is.na(data))
  na_cols = names(nas[nas > 0])
  for(j in na_cols){
    set(data, j=j, value=ifelse(is.na(data[[j]]), 0, data[[j]]))
  }
  
  #cat("Total NAs: ", sum(is.na(data)), "\n")
  
  # Create vars
  data[, (paste0(segment, "_CovidCases.LBU")):=ifelse(is.na(COVID_CASES), 0, get(xseg) * COVID_CASES)]
  # data[, (paste0(segment, "_Covid.LBU")):=get(xseg) * COVID19.DUMMY]
  data[, COVID_CASES:=NULL]
  
  # Fit model
  fml = as.formula(sprintf("%s ~ .", y_var))
  fit = lm(fml, data=data, weights=weights)
  
  # summary(fit)
  ### VIF - collinearidad: https://cran.r-project.org/web/packages/olsrr/vignettes/regression_diagnostics.html
  vif = as.data.table(ols_vif_tol(fit))
  
  ### Coefficients
  summary = as.data.table(broom::glance(fit, conf.int=TRUE, conf.level=0.95))
  summary = cbind(dependent_segment, independent_segment, summary)
  
  mdl_coefs = as.data.table(broom::tidy(fit))
  mdl_coefs = merge(mdl_coefs, vif, by.x='term', by.y='Variables', all.x=TRUE)
  mdl_coefs[, signig_code:=ifelse(p.value <= 0.001, "***", 
                                  ifelse(p.value <= 0.01, "**", 
                                         ifelse(p.value <= 0.05, "*", 
                                                ifelse(p.value <= 0.1, ".", ""))))]
  
  elast = mdl_coefs[term == sprintf("%s.LBU", independent_segment)]$estimate
  
  list(mdl_coefs=mdl_coefs, fit=fit, summary=summary, elast=elast) 
}



fit_lm_model <- function(dt, dependent_segment='C18CT', segments){
  # LUH: Log(Unit stz)
  # LBU: Log(Base price per unit)
  
  # Define y - X
  y_var = sprintf("%s.LUH", dependent_segment)
  # X
  x_vars = paste0(independent_segment, ".", c("DRT","FCT","LTDP"))
  x_vars_segment = paste0(segments, ".", "LBU")
  x_vars = unique(c(x_vars, x_vars_segment))
  
  # Holiday variables
  hol_vars = c("JUL4.HOL","J4LG.HOL","J4LD.HOL","MEMD.HOL","MDLG.HOL","MDLD.HOL","NYR.HOL","NYLG.HOL","NYLD.HOL","XMAS.HOL","XMLG.HOL","XMLD.HOL","THNK.HOL","THLG.HOL","THLD.HOL","LABD.HOL","LBLG.HOL","LBLD.HOL","EAST.HOL","EALG.HOL","EALD.HOL","CINCO.HOL","M5LG.HOL","M5LD.HOL","SBWL.HOL","SBWLG.HOL","SBWLD.HOL")
  
  # Select data
  data = copy(dt[, c(y_var, hol_vars, 'MKT_', 
                     "COVID19.DUMMY", "COVID19Lockdown_march15and22", 
                     "SEA7.SON", "COVID_CASES",
                     x_vars), wi=F])
  
  # Create vars
  xseg = paste0(independent_segment, ".LBU")
  data[, (paste0(independent_segment, "_CovidCases.LBU")):=get(xseg) * COVID_CASES]
  # data[, (paste0(independent_segment, "_Covid.LBU")):=get(xseg) * COVID19.DUMMY]
  data[, COVID_CASES:=NULL]
  weights = as.numeric(dt$CAT.WGT)
  
  # Fit model
  fml = as.formula(sprintf("%s ~ .", y_var))
  fit = lm(fml, data=data, weights=weights)  
  
  # summary(fit)
  
  ### VIF - collinearidad: https://cran.r-project.org/web/packages/olsrr/vignettes/regression_diagnostics.html
  vif = suppressWarnings(as.data.table(ols_vif_tol(fit)))
  
  ### Coefficients
  summary = summary(fit)
  
  mdl_coefs = as.data.table(broom::tidy(fit))
  mdl_coefs = merge(mdl_coefs, vif, by.x='term', by.y='Variables', all.x=TRUE)
  mdl_coefs[, signig_code:=ifelse(p.value <= 0.001, "***", 
                                  ifelse(p.value <= 0.01, "**", 
                                         ifelse(p.value <= 0.05, "*", 
                                                ifelse(p.value <= 0.1, ".", ""))))]
  
  elast = mdl_coefs[term == sprintf("%s.LBU", segment)]$estimate
  
  list(mdl_coefs=mdl_coefs, fit=fit, summary=summary, elast=elast) 
}
