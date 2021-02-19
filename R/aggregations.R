aggregate_byperiod <- function(dt, aggregation_vars, var_name="EQU"){
  
  agg = rbindlist(lapply(aggregation_vars, function(j){
    segment = j
    j = paste0(j, ".", var_name)
    dt[, .(variable=segment, value=sum(get(j))), by=.(PERIOD)][order(-PERIOD)][1:2]
  }))
  
  agg = dcast.data.table(agg, variable ~ PERIOD, value.var='value')
  setnames(agg, names(agg)[2:3], paste0(var_name, "_t", names(agg)[2:3]))
  agg
}


run_aggregations <- function(dt, aggregation_vars){
  num_vars = c("EQU", "U", "RDL", "ME")
  
  aggs = lapply(num_vars, function(var_name){
    aggregate_byperiod(dt, aggregation_vars, var_name=var_name)
  })
  
  aggs = cbind(aggs[[1]][,.(variable)], 
               do.call(cbind, lapply(aggs, function(d) d[,-1, wi=F])))
  
  for(i in 2:3){
    aggs[, (paste0("PPU_t",i)):=get(paste0("RDL_t",i))/get(paste0("U_t",i))]
    aggs[, (paste0("PRI_t",i)):=get(paste0("RDL_t",i))/get(paste0("EQU_t",i))]
  }
  
  cols = names(aggs)
  
  aggs = aggs[, c("variable", paste0("EQU_t",2:3), paste0("U_t",2:3),
                  paste0("RDL_t",2:3), paste0("PPU_t",2:3), paste0("PRI_t",2:3)), 
              with=F]
  
  aggs
}
