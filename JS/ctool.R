#' run_ctool
#'
#' @param time_config 
#' @param cin_config 
#' @param m_config 
#' @param t_config 
#' @param s_config 
#' @param soil_pools 
#'
#' @return
#' @export
#'
#' @examples
run_ctool = function(time_config,
                     cin_config,
                     m_config,
                     t_config,
                     s_config,
                     soil_pools) {
  
  
  
  
  simul=1:time_config$steps
  out = as.data.frame(soil_pools)
  ctool = lapply(simul, function(tstp) {
    out = turnover(timestep = tstp, time_config = time_config, cin_config = cin_config, m_config = m_config, t_config = t_config, s_config = s_config, out = out)
    return(out)
  })
  ctool = data.table::rbindlist(ctool)
  return(cbind(time_config$timeperiod[,c('mon','yrs')], ctool))
}
