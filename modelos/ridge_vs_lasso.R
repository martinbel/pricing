rm(list = ls(all = TRUE)); gc()

source("R/load_packages.R")
source("R/helpers.R")
source("R/models.R")

# list.files('data')

# Read data
file_path = "data/Quaker SNACK BARS Pricing 2020 Final Db dataprep Grocery.sav"
dt = preprocess(file_path)

# SPSS syntax
mdl_script = readLines("data/4. Quaker Pricing SS Modeling.sps")

# y: Segmento.luh
# x: Segmento.LBU

# segment_vars = find_var(dt, "CDSMALLB")
# Define segments
all_segments = c("C18CT", "C48CT", "C48CTD34", "CSB", "CDLARGEB", "CDSMALLB", "D14CT", "D6CT")

summaries = list()
elast = list()
alphas = c(0, 0.2, 0.5, 0.8, 1)
for(i in seq_along(alphas)){
  
  models = lapply(all_segments, function(segment){
    fit_enet_model(dt, alpha=alpha[i], segment, all_segments)
  })
  
  summaries[[i]] = cbind(Segment=all_segments, rbindlist(lapply(models, "[[", "summary")))
  
  elast[[i]] = elast_matrix(models, all_segments)
}
names(elast) = paste0("Alpha ", alphas)
names(summaries) = paste0("Alpha ", alphas)


lst = lapply(summaries, function(d) d[, .(mae)])
smr = do.call(cbind, lst)
cbind(x=all_segments, smr)

lst = lapply(summaries, function(d) d[, .(r.squared)])
smr = do.call(cbind, lst)
cbind(x=all_segments, smr)

## write a list of data.frames to individual worksheets using list names as worksheet names
write.xlsx(elast, file = "results/elastities.xlsx")