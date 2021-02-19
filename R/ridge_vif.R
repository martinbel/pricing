fit_ridge_model <- function(dt, 
                            segment='C18CT',  
                            all_segments, 
                            lambda_criteria="kGCV", 
                            drop_vars=NULL){
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
  
  cor_mat = cor(data[, c(y_var, x_vars_segment), wi=F])
  
  if(!is.null(drop_vars)){
    set(data, j=drop_vars, value=NULL)
  }
  
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
  
  fml = as.formula(sprintf("%s ~ .", y_var))
  fit = lm(fml, data=data)
  X_mm = model.matrix(fit)[,-1]
  
  #plot(lm.ridge(fml, data, lambda = seq(0,0.1,0.001), weights=weights))
  #select(lm.ridge(fml, data, lambda = seq(0,0.1,0.0001), weights=weights))
  
  # Fit model
  #fml = as.formula(sprintf("%s .", y_var))
  #X_mm = model.matrix(fml, data=data)
  y = as.numeric(data[[y_var]])
  
  #lambda <- c(0.005, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.080, 0.09, 0.1)
  lambda = c(0, 0.005, 0.01, 0.02, 0.04, 0.08)#seq(0.00001, 1, 0.01)
  lambda = lambda[lambda != 0]
  lridge = genridge::ridge(y, X_mm, lambda=lambda, weights=weights)
  # coef(lridge)
  # traceplot(lridge)
  
  best_lambda = lridge[[lambda_criteria]]
  lridge = genridge::ridge(y, X_mm,
                           lambda=best_lambda,
                           weights=weights)
  
  # VIF
  vridge = vif.ridge(lridge)
  #vridge = vridge[complete.cases(vridge),]
  vridge_xseg = vridge[, intersect(names(vridge), x_vars_segment)]
  
  vridge_all = data.table(Variable=names(vridge[1,]), VIF=as.vector(vridge[1,]))
  vridge_all = vridge_all[order(-VIF)]# vridge_all[1:20]
  
  # enet_fit = cv.glmnet(X_mm, y, alpha=alpha, 
  #                      family='gaussian',
  #                      type.measure = "mae", 
  #                      nfolds = 10, weights=weights, 
  #                      standardize=TRUE)
  # plot(enet_fit)
  
  # Evaluation metrics
  summary = data.table(mse=lridge$mse)
  
  # Coefficients
  coefs = coefficients(lridge)
  coefs = data.table(Variable=colnames(coefs), Coefficient=as.numeric(coefs[1,]))
  coefs = merge(coefs, vridge_all, by='Variable')
  
  # elasticity coefficients
  elast = coefs[Variable %in% paste0(all_segments,".LBU")]
  
  list(coefs=coefs, fit=lridge, summary=summary, elast=elast, cor_mat=cor_mat)
}
