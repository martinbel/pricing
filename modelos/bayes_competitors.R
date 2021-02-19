rm(list = ls(all = TRUE)); gc()

source("R/load_packages.R")
source("R/helpers.R")
source("R/models.R")
source("R/aggregations.R")
source("R/brm_model_competitors.R")

library(rstan)
library(future)
plan(multicore)
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)
# list.files('data')

# y: Segmento.luh
# x: Segmento.LBU

# Read data
file_path = "data/Quaker RTE Pricing 2020 Final Db dataprep Grocery With QK Rest.sav"
dt = preprocess(file_path)

# Define segments
all_segments = toupper(readLines("meta/own_segment.txt")) 
all_competitors = toupper(readLines("meta/comp_segment.txt"))

# Sacar US.F Total - + que la suma de los 64 mercados
dt = dt[MKT_ != "US.F"]

params = list(prior_dir_mean=-0.3449, prior_dir_sd=0.3449/5, 
              prior_ind_mean=0.05, prior_ind_sd=0.05/5)

quantile(rnorm(1000, params$prior_dir_mean, params$prior_dir_sd), seq(0, 1, 0.1))

# Modelo segmentos
models = lapply(all_segments, function(segment){
  print(segment)
  fit_bayesian_comp_model(dt, segment, all_competitors, params)
})
names(models) = all_segments

#models = readRDS("results/bayes_segments_comp.RData")
models = readRDS("results/bayes_segments_comp.RData")

all_segments = all_competitors

elast = do.call(cbind, lapply(models, function(m){
  vars = paste0(all_competitors, ".", "LBU")
  fixed = m$summary$fixed
  coefs = data.table(Variable=rownames(fixed), fixed)
  coefs[Variable %in% vars, .(Estimate)]
}))
setnames(elast, all_segments)
elast = cbind(segment=all_competitors, elast)

# Coefficients
coefs_tbl = do.call(cbind, lapply(all_segments, function(segment){
  cfs = coefficients(models[[segment]]$fit_stg1)
  dt_coefs = data.table(Variable=names(cfs), Coefficient=as.vector(cfs))
  cbind(segment, dt_coefs, ` `='')
}))

coefs_bayes_tbl = do.call(cbind, lapply(all_segments, function(segment){
  mdl = models[[segment]]$mdl
  smr = summary(mdl)
  
  vars = rownames(smr$fixed)
  vars = ifelse(vars == 'lm', "lm_stage1", vars)
  dt_coefs = data.table(Variable=vars, smr$fixed)
  set(dt_coefs, j=c("Rhat", "Bulk_ESS", "Tail_ESS"), value=NULL)
  
  cbind(segment, dt_coefs, ` `='')
}))

print(elast)

results_file = "results/bayes_comp_div5.xlsx"
wb <- write.xlsx(elast, file = results_file, sheetName="Elasticity Matrix")
addWorksheet(wb = wb, sheetName = "Coefs Stage1")
writeData(wb, sheet = 2, coefs_tbl, startCol = "A", startRow = 1)
addWorksheet(wb = wb, sheetName = "Coefs Stage2")
writeData(wb, sheet = 3, coefs_bayes_tbl, startCol = "A", startRow = 1)
saveWorkbook(wb, results_file, overwrite = TRUE)

# saveRDS(models, "results/bayes_segments_comp.rds")
