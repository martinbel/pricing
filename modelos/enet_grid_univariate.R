rm(list = ls(all = TRUE)); gc()

source("R/load_packages.R")
source("R/helpers.R")
source("R/models.R")
source("R/grid_model.R")
source("R/aggregations.R")

# list.files('data')

# Read data
file_path = "data/Quaker SNACK BARS Pricing 2020 Final Db dataprep Grocery.sav"
dt = preprocess(file_path)

# y: Segmento.luh
# x: Segmento.LBU

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

# segment_vars = find_var(dt, "CDSMALLB")

alpha=0
grid = expand.grid(y=all_segments, x=all_segments, stringsAsFactors=FALSE)
grid$elast = NA

models = list()
for(i in 1:nrow(grid)){
  g = grid[i,]
  mdl = fit_grid_model_univariate(dt, alpha=alpha, 
                                  segment=g$y, x_segment=g$x, all_segments)
  models[[i]] = mdl
  grid[i,3] = mdl$elast$Coefficient
}

elast = dcast.data.table(x ~ y, data=as.data.table(grid), value.var='elast')
# join elast - aggregations
elast_aggs = merge(elast, aggs, by.x='x', by.y='variable')

# Model summary
summary = cbind(Segment=all_segments, rbindlist(lapply(models, "[[", "summary")))

# Export results to excel
results_file = "results/elastities_ridge_grid_univariate.xlsx"
wb <- write.xlsx(elast_aggs, file = results_file, sheetName="Elasticity Matrix")
writeData(wb, sheet=1, summary, startCol = 'A', startRow = 12)

addWorksheet(wb = wb, sheetName = "Brand Model")
writeData(wb, sheet = 2, model_brand$coefs, startCol = "A", startRow = 1)

saveWorkbook(wb, results_file, overwrite = TRUE)
