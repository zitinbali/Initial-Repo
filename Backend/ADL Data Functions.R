

##############
# splice fn
##############

ADL_splice <- function(data, window_start, window_end){
  start_rownum = which(grepl(window_start, data$Date))
  end_rownum = which(grepl(window_end, data$Date))
  
  output <- data[start_rownum:end_rownum, ]
  
  return(output)
}


