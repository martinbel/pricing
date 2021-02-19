fit_lm_grid_model_univariate <- function(dt, 
                                         segment='C18CT',  
                                         x_segment='C48CT',
                                         all_segments){
  
  # LUH: Log(Unit stz)
  # LBU: Log(Base price per unit)
  
  # Define y - X
  y_var = sprintf("%s.LUH", segment)
  x_direct = sprintf("%s.LBU", segment)
  xseg = paste0(x_segment, ".LBU")
  
  # X
  x_vars = paste0(segment, ".", c("DRT","FCT","LTDP"))
  if(x_direct != xseg){
    x_vars = c(x_vars, x_direct, xseg)
  } else {
    x_vars = c(x_vars, x_direct)
  }
  #x_vars_segment = paste0(all_segments, ".", "LBU")
  #x_vars = unique(c(x_vars, x_vars_segment))
  
  # Holiday variables
  hol_vars = c("JUL4.HOL","J4LG.HOL","J4LD.HOL","MEMD.HOL","MDLG.HOL","MDLD.HOL","NYR.HOL","NYLG.HOL","NYLD.HOL","XMAS.HOL","XMLG.HOL","XMLD.HOL","THNK.HOL","THLG.HOL","THLD.HOL","LABD.HOL","LBLG.HOL","LBLD.HOL","EAST.HOL","EALG.HOL","EALD.HOL","CINCO.HOL","M5LG.HOL","M5LD.HOL","SBWL.HOL","SBWLG.HOL","SBWLD.HOL")
  
  dt[, CAT.WGT:=ifelse(is.na(CAT.WGT), mean(CAT.WGT), CAT.WGT)]
  
  # Select data
  data = copy(dt[, c(y_var, "COVID19.DUMMY", "COVID19Lockdown_march15and22", 
                     "SEA7.SON", "COVID_CASES", hol_vars, 'MKT_', 
                     x_vars, "CAT.WGT"), wi=F])
  
  # Remove NAs from x-segment
  data = data[!is.na(get(y_var))][!is.na(get(xseg))]
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
  
  cor_mat = cor(data[, c(y_var, xseg, x_vars), wi=F])
  
  # Fit model
  fml = as.formula(sprintf("%s ~ .", y_var))
  fit = lm(fml, data=data, weights=weights)  
  
  # summary(fit)
  
  ### VIF - collinearidad: https://cran.r-project.org/web/packages/olsrr/vignettes/regression_diagnostics.html
  vif = as.data.table(suppressWarnings(ols_vif_tol(fit)))
  
  ### Coefficients
  summary = as.data.table(broom::glance(fit))
  summary = cbind(y_var, xseg, summary)
  
  mdl_coefs = as.data.table(broom::tidy(fit))
  mdl_coefs = merge(mdl_coefs, vif, by.x='term', by.y='Variables', all.x=TRUE)
  mdl_coefs[, signig_code:=ifelse(p.value <= 0.001, "***", 
                                  ifelse(p.value <= 0.01, "**", 
                                         ifelse(p.value <= 0.05, "*", 
                                                ifelse(p.value <= 0.1, ".", ""))))]
  
  if(x_direct != xseg){
    elast = mdl_coefs[term %in% c(xseg), estimate]
  } else {
    elast = mdl_coefs[term %in% c(x_direct), estimate]
  }
  
  list(mdl_coefs=mdl_coefs, fit=fit, summary=summary, 
       elast=elast, cor_mat=cor_mat, 
       model_name=paste0(y_var, "|", xseg)) 
  
}



fit_grid_model <- function(dt, alpha=1,
                           segment='C18CT',  
                           x_segment='C18CT',
                           all_segments){
  
  # LUH: Log(Unit stz)
  # LBU: Log(Base price per unit)
  
  # Define y - X
  y_var = sprintf("%s.LUH", segment)
  xseg = paste0(x_segment, ".LBU")
  
  # X
  x_vars = paste0(segment, ".", c("DRT","FCT","LTDP"))
  x_vars_segment = paste0(all_segments, ".", "LBU")
  x_vars = unique(c(x_vars, x_vars_segment))
  
  # Holiday variables
  hol_vars = c("JUL4.HOL","J4LG.HOL","J4LD.HOL","MEMD.HOL","MDLG.HOL","MDLD.HOL","NYR.HOL","NYLG.HOL","NYLD.HOL","XMAS.HOL","XMLG.HOL","XMLD.HOL","THNK.HOL","THLG.HOL","THLD.HOL","LABD.HOL","LBLG.HOL","LBLD.HOL","EAST.HOL","EALG.HOL","EALD.HOL","CINCO.HOL","M5LG.HOL","M5LD.HOL","SBWL.HOL","SBWLG.HOL","SBWLD.HOL")
  
  dt[, CAT.WGT:=ifelse(is.na(CAT.WGT), mean(CAT.WGT), CAT.WGT)]
  
  # Select data
  data = copy(dt[, c(y_var, "COVID19.DUMMY", "COVID19Lockdown_march15and22", 
                     "SEA7.SON", "COVID_CASES", hol_vars, 'MKT_', 
                     x_vars, "CAT.WGT"), wi=F])
  
  # Remove NAs from x-segment
  data = data[!is.na(get(y_var))][!is.na(get(xseg))]
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
                       nfolds = 10, weights=weights)
  plot(enet_fit)
  
  # Evaluation metrics
  best_inx = which(enet_fit$glmnet.fit$lambda == enet_fit$lambda.min)
  r2 = enet_fit$glmnet.fit$dev.ratio[best_inx] 
  mae = enet_fit$cvm[best_inx] 
  summary = data.table(r.squared=r2, mae=mae)
  
  # Coefficients
  coefs = as.matrix(coefficients(enet_fit, s=enet_fit$lambda.min))
  coefs = data.table(Variable=rownames(coefs), Coefficient=as.numeric(coefs[,1]))
  
  # coefs[Variable %in% x_vars_segment]
  # elasticity coefficients
  #elast = coefs[Variable %in% paste0(all_segments,".LBU")]
  elast = coefs[Variable %in% xseg]
  
  list(coefs=coefs, fit=enet_fit, summary=summary, 
       elast=elast, model_name=paste0(y_var, "|", x_segment), 
       cor_mat=cor_mat)
}



fit_grid_model_univariate <- function(dt, alpha=0,
                                      segment='C18CT',  
                                      x_segment='C18CT',
                                      all_segments){
  
  # LUH: Log(Unit stz)
  # LBU: Log(Base price per unit)
  
  # Define y - X
  y_var = sprintf("%s.LUH", segment)
  xseg = paste0(x_segment, ".LBU")
  
  # X
  x_vars = paste0(segment, ".", c("DRT","FCT","LTDP"))
  #x_vars_segment = paste0(all_segments, ".", "LBU")
  #x_vars = unique(c(x_vars, x_vars_segment))
  
  # Holiday variables
  hol_vars = c("JUL4.HOL","J4LG.HOL","J4LD.HOL","MEMD.HOL","MDLG.HOL","MDLD.HOL","NYR.HOL","NYLG.HOL","NYLD.HOL","XMAS.HOL","XMLG.HOL","XMLD.HOL","THNK.HOL","THLG.HOL","THLD.HOL","LABD.HOL","LBLG.HOL","LBLD.HOL","EAST.HOL","EALG.HOL","EALD.HOL","CINCO.HOL","M5LG.HOL","M5LD.HOL","SBWL.HOL","SBWLG.HOL","SBWLD.HOL")
  
  dt[, CAT.WGT:=ifelse(is.na(CAT.WGT), mean(CAT.WGT), CAT.WGT)]
  
  # Select data
  data = copy(dt[, c(y_var, "COVID19.DUMMY", "COVID19Lockdown_march15and22", 
                     "SEA7.SON", "COVID_CASES", hol_vars, 'MKT_', 
                     xseg, x_vars, "CAT.WGT"), wi=F])
  
  # Remove NAs from x-segment
  data = data[!is.na(get(y_var))][!is.na(get(xseg))]
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
  
  cor_mat = cor(data[, c(y_var, xseg, x_vars), wi=F])
  
  # Fit model
  fml = as.formula(sprintf("%s ~ 0 + .", y_var))
  X_mm = model.matrix(fml, data=data)
  y = as.numeric(data[[y_var]])
  
  enet_fit = cv.glmnet(X_mm, y, alpha=alpha, 
                       family='gaussian',
                       type.measure = "mae", 
                       nfolds = 10, weights=weights)
  plot(enet_fit)
  
  # Evaluation metrics
  best_inx = which(enet_fit$glmnet.fit$lambda == enet_fit$lambda.min)
  r2 = enet_fit$glmnet.fit$dev.ratio[best_inx] 
  mae = enet_fit$cvm[best_inx] 
  summary = data.table(r.squared=r2, mae=mae)
  
  # Coefficients
  coefs = as.matrix(coefficients(enet_fit, s=enet_fit$lambda.min))
  coefs = data.table(Variable=rownames(coefs), Coefficient=as.numeric(coefs[,1]))
  
  # coefs[Variable %in% xseg]
  # elasticity coefficients
  #elast = coefs[Variable %in% paste0(all_segments,".LBU")]
  elast = coefs[Variable %in% xseg]
  
  list(coefs=coefs, fit=enet_fit, summary=summary, 
       elast=elast, model_name=paste0(y_var, "|", x_segment), 
       cor_mat=cor_mat)
}



