rm(list = ls(all = TRUE)); gc()

source("R/load_packages.R")
source("R/helpers.R")
source("R/models.R")
source("R/aggregations.R")
source("R/brm_model.R")

library(rstan)
library(future)
plan(multicore)
# list.files('data')

# y: Segmento.luh
# x: Segmento.LBU

# Read data
file_path = "data/Quaker SNACK BARS Pricing 2020 Final Db dataprep Grocery - v2.sav"
#file_path = "data/Quaker SNACK BARS Pricing 2020 Final Db dataprep Grocery.sav"
dt = preprocess(file_path)

# Define segments
all_segments = toupper(readLines("meta/own_segment.txt")) 
all_competitors = toupper(readLines("meta/comp_segment.txt"))

# RTE
# RTEQTG
# RTEQTG_Rest
# Sacar US.F Total - + que la suma de los 64 mercados
dt = dt[MKT_ != "US.F"]

mdl_category = fit_bayesian_brand(dt[MKT_ != 'US.F'], segment='RTE')
coefs_category = mdl_category$summary$fixed
coefs_category

mdl_marca = fit_bayesian_brand(dt[MKT_ != 'US.F'], segment='RTEQTG')
coefs_brand = mdl_marca$summary$fixed
coefs_brand

# Resultados Category
cat_coefs_stage1 = coefficients(mdl_category$fit_stg1)
cat_coefs_stage1 = data.table(Variable=names(cat_coefs_stage1), 
                              Coefficient=as.vector(cat_coefs_stage1))
coefs_category = mdl_category$summary$fixed
coefs_category = cbind(Variable=rownames(coefs_category), as.data.table(coefs_category))
coefs_category

# Resultados Brand
coefs_stage1 = coefficients(mdl_marca$fit_stg1)
coefs_stage1 = data.table(Variable=names(coefs_stage1), Coefficient=as.vector(coefs_stage1))
coefs_brand = mdl_marca$summary$fixed
coefs_brand = cbind(Variable=rownames(coefs_brand), as.data.table(coefs_brand))
coefs_brand

results_file = "results/brand_category.xlsx"
wb <- write.xlsx(cbind(coefs_stage1, cat_coefs_stage1), 
                 file = results_file, sheetName="Stage1")
addWorksheet(wb = wb, sheetName = "Stage2")
writeData(wb, sheet = 2, cbind(coefs_brand, coefs_category), startCol = "A", startRow = 1)
saveWorkbook(wb, results_file, overwrite = TRUE)
