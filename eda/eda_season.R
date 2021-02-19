rm(list = ls(all = TRUE)); gc()

source("R/load_packages.R")
source("R/helpers.R")
source("R/models.R")
source("R/aggregations.R")
source("R/brm_model.R")

library(rstan)
library(future)

# y: Segmento.luh
# x: Segmento.LBU

# Read data
file_path = "data/Quaker RTE Pricing 2020 Final Db dataprep Grocery With QK Rest.sav"
dt = preprocess(file_path)

all_segments = toupper(readLines("meta/own_segment.txt")) 

mdf = dt[MKT_ == 'US.F', .(CNCLSB.EQU, SEA.SON, SEA3.SON, SEA7.SON), by=WEEK]

mdf[WEEK %between% c("2020-02-01", "2020-05-01")]

mdf = melt(mdf, id.vars = "WEEK")

ggplot(mdf, aes(WEEK, value, color=variable)) + 
  geom_line() +
  facet_wrap(~variable, scales='free')

ggplot(mdf[variable == 'CNCLSB.EQU' & year(WEEK) == 2020], 
       aes(WEEK, value, color=variable)) + 
  geom_line()

# mdf = melt(dt[, .(WEEK, MKT_, C18CT.EQU, SEA.SON, SEA3.SON, SEA7.SON)], 
#            id.vars=c('WEEK', "MKT_"))

mdf = mdf[, .(value=mean(value)) , by=.(WEEK, variable)]
#mdf[variable != 'CDSMALLB.EQU', value:]

states = mdf[, unique(mkt_)]




ggplot(mdf[MKT_ == 'LA' ], 
       aes(WEEK, value, color=variable))  + geom_line() + facet_wrap(~variable, scales='free')

ggplot(mdf[mkt_ %in% states[1:6]], aes(WEEK, value, color=variable)) + 
  geom_line() + facet_wrap(~mkt_, scales='free')


ggplot(mdf[MKT_ == 'TUL'], 
       aes(WEEK, value, color=variable))  + geom_line()

