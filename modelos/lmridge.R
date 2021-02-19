rm(list = ls(all = TRUE)); gc()

source("R/load_packages.R")
source("R/helpers.R")
source("R/models.R")
source("R/aggregations.R")
source("R/ridge_vif.R")

# list.files('data')

# y: Segmento.luh
# x: Segmento.LBU

# Read data
file_path = "data/Quaker SNACK BARS Pricing 2020 Final Db dataprep Grocery.sav"
dt = preprocess(file_path)

# Define segments
all_segments = c("C18CT", "C48CT", "CSB", "D14CT", "D6CT")

# Aggregaciones
aggregation_vars = c("QUAKERCHEWY", all_segments)
aggs = run_aggregations(dt, aggregation_vars)

# Sacar US.F Total - + que la suma de los 64 mercados
dt = dt[MKT_ != "US.F"]

# Modelo marca
model_brand = fit_enet_brand(dt, alpha=0, segment='QUAKERCHEWY')
model_brand$elast

# Modelo segmentos
lambda_criteria = "kGCV"
models = lapply(all_segments, function(segment){
  fit_ridge_model(dt, segment, all_segments, lambda_criteria)
})
names(models) = all_segments

# Elasticity matrix
elast_orig = elast_matrix(models, all_segments)

# Coefficients
coefs_tbl_orig = do.call(rbind, lapply(all_segments, function(segment){
  cbind(segment, models[[segment]]$coefs, Blank='')
}))

### Drop vars with high VIF
drop_var_list = lapply(models, function(x) x$coefs[VIF >= 10, Variable])
models = lapply(all_segments, function(segment){
  drop_vars = drop_var_list[[segment]]
  drop_vars = setdiff(drop_vars, paste0(segment, ".LBU"))
  fit_ridge_model(dt, segment, all_segments, lambda_criteria, drop_vars=drop_vars)
})
names(models) = all_segments

elast_list = rbindlist(lapply(all_segments, function(segment){
  d = models[[segment]]$elast
  cbind(segment, d)
}))

elast_drop_cols = dcast.data.table(elast_list, Variable ~ segment, value.var='Coefficient')

# Coefficients
coefs_tbl_dropcols = do.call(rbind, lapply(all_segments, function(segment){
  cbind(segment, models[[segment]]$coefs, Blank='')
}))

# Export results to excel
results_file = "results/elastities_ridge_vif.xlsx"

wb <- write.xlsx(elast_orig, file = results_file, sheetName="Elasticity Matrix")

addWorksheet(wb = wb, sheetName = "Elasticity drop VIF>10 cols")
writeData(wb, sheet = 2, elast_drop_cols, startCol = "A", startRow = 1)

addWorksheet(wb = wb, sheetName = "SS Models - Orig")
writeData(wb, sheet = 3, coefs_tbl_orig, startCol = "A", startRow = 1)

addWorksheet(wb = wb, sheetName = "SS Models - VIF>10")
writeData(wb, sheet = 4, coefs_tbl_dropcols, startCol = "A", startRow = 1)

addWorksheet(wb = wb, sheetName = "Brand Model")
writeData(wb, sheet = 5, model_brand$coefs, startCol = "A", startRow = 1)

saveWorkbook(wb, results_file, overwrite = TRUE)
