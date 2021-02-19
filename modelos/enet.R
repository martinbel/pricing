rm(list = ls(all = TRUE)); gc()

source("R/load_packages.R")
source("R/helpers.R")
source("R/models.R")
source("R/aggregations.R")

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
alpha=0
models = lapply(all_segments, function(segment){
  fit_enet_model(dt, alpha=alpha, segment, all_segments)
})
names(models) = all_segments

summary = cbind(Segment=all_segments, rbindlist(lapply(models, "[[", "summary")))
elast = elast_matrix(models, all_segments)
elast

# Coefficients
coefs_tbl = do.call(rbind, lapply(all_segments, function(segment){
  cbind(segment, models[[segment]]$coefs, Blank='')
}))

elast_aggs = merge(elast, aggs, by.x='x', by.y='variable')

# Export results to excel
results_file = "results/elastities_ridge.xlsx"
wb <- write.xlsx(elast_aggs, file = results_file, sheetName="Elasticity Matrix")
writeData(wb, sheet=1, summary, startCol = 'A', startRow = 12)

addWorksheet(wb = wb, sheetName = "SS Models")
writeData(wb, sheet = 2, coefs_tbl, startCol = "A", startRow = 1)

addWorksheet(wb = wb, sheetName = "Brand Model")
writeData(wb, sheet = 3, model_brand$coefs, startCol = "A", startRow = 1)

saveWorkbook(wb, results_file, overwrite = TRUE)
