fit_bayesian_comp_model <- function(dt, segment='C18CT', all_competitors, params){
  
  # LUH: Log(Unit stz)
  # LBU: Log(Base price per unit)
  
  # Define y - X
  y_var = sprintf("%s.LUH", segment)
  x_direct = sprintf("%s.LBU", segment)
  #xseg = paste0(x_segment, ".LBU")
  
  # X
  x_vars = paste0(segment, ".", c("DRT","FCT","LTDP"))
  x_vars_segment = paste0(all_competitors, ".", "LBU")
  x_vars = unique(c(x_vars, x_vars_segment))
  
  # Holiday variables
  hol_vars = c("JUL4.HOL","J4LG.HOL","J4LD.HOL","MEMD.HOL","MDLG.HOL","MDLD.HOL","NYR.HOL","NYLG.HOL","NYLD.HOL","XMAS.HOL","XMLG.HOL","XMLD.HOL","THNK.HOL","THLG.HOL","THLD.HOL","LABD.HOL","LBLG.HOL","LBLD.HOL","EAST.HOL","EALG.HOL","EALD.HOL","CINCO.HOL","M5LG.HOL","M5LD.HOL","SBWL.HOL","SBWLG.HOL","SBWLD.HOL")
  
  dt[, CAT.WGT:=ifelse(is.na(CAT.WGT), mean(CAT.WGT), CAT.WGT)]
  
  # Select data
  data = copy(dt[, c(y_var, 'MKT_',  hol_vars, 
                     "COVID19.DUMMY", "COVID19Lockdown_march15and22", #"COVID_CASES", 
                     "SEA7.SON",  "CAT.WGT"), wi=F])
  
  data_baseline = copy(dt[, c(y_var, "SEA7.SON", x_direct, x_vars, "CAT.WGT"), wi=F])
  
  # Remove NAs from x-segment
  data = data[!is.na(get(y_var))]#[!is.na(get(x_direct))]
  data_baseline = data_baseline[!is.na(get(y_var))] 
  
  weights = as.numeric(data_baseline$CAT.WGT)
  
  # Weights
  data[, CAT.WGT:=NULL]
  data_baseline[, CAT.WGT:=NULL]
  
  # Imputo NAs
  nas = colSums(is.na(data))
  na_cols = names(nas[nas > 0])
  for(j in na_cols){
    set(data, j=j, value=ifelse(is.na(data[[j]]), 0, data[[j]]))
  }
  
  nas = colSums(is.na(data_baseline))
  na_cols = names(nas[nas > 0])
  for(j in na_cols){
    set(data_baseline, j=j, value=ifelse(is.na(data_baseline[[j]]), 0, data_baseline[[j]]))
  }
  
  # Create vars
  #data[, (paste0(segment, "_CovidCases.LBU")):=ifelse(is.na(COVID_CASES), 0, get(xseg) * COVID_CASES)]
  # data[, (paste0(segment, "_Covid.LBU")):=get(xseg) * COVID19.DUMMY]
  #data[, COVID_CASES:=NULL]
  
  cor_mat = NULL#cor(data[, c(y_var, x_vars), wi=F])
  
  # Fit model - Control
  fml = as.formula(sprintf("%s ~ .", y_var))
  fit = lm(fml, data=data, weights=weights) 
  # summary(fit)
  # Predict model1
  yhat_lm = suppressWarnings(as.vector(predict(fit, data)))
  
  # get MM 
  fit_data = lm(fml, data=data_baseline, weights=weights) 
  
  # add prediction model 1
  data_baseline[, lm:=yhat_lm]
  
  X_mm = model.matrix(fit_data)
  X_mm = cbind(X_mm, lm=yhat_lm)
  cols = colnames(X_mm)
  
  #setnames(data, "MKT_", "MKT")
  
  # specify the model
  xpromo = paste0(segment, ".", c("DRT","FCT","LTDP"))
  
  # crossed elast
  x_vars_segment_priors = setdiff(x_vars_segment, x_direct)
  
  # Prior values  
  prior_dir_mean = params$prior_dir_mean
  prior_dir_sd = params$prior_dir_sd
  
  prior_ind_mean = params$prior_ind_mean
  prior_ind_sd = params$prior_ind_sd
  
  direct_prior_string = sprintf("normal(%s, %s)", prior_dir_mean, prior_dir_sd)
  indirect_prior_string = sprintf("normal(%s, %s)", prior_ind_mean, prior_ind_sd)
  
  prior1 = prior_string(indirect_prior_string, class = "b", coef = x_vars_segment_priors)
  prior2 = prior_string(direct_prior_string, class = "b", coef = x_direct)
  priors = c(prior1, prior2)
  
  cat("Running segment: ", segment, "\n")
  print(priors)
  
  mdl <- brm(fml, data=data_baseline, seed=123, chains=2, iter=2000, 
             prior=priors, family=gaussian(),
             algorithm="sampling",
             future = TRUE, silent=FALSE)
  
  list(mdl=mdl, 
       priors=priors,
       summary=summary(mdl),
       posterior_summary=posterior_summary(mdl),
       prior_summary=prior_summary(mdl), 
       cor_mat=cor_mat, 
       fit_stg1=fit, 
       fit_stg2=fit_data)
  
}