
createdurations <- function(DT, dvec){
  # smooths the streamflow time series with
  # moving average whose window length corresponds
  # to the duration of interest.
  # -----------------
  # DT - streamflow data. table, containing at least:
  #      date, streamflow
  # dvec - vector of desired durations (hours)
  # -------------------------------------------------
  
  # output: data.table 'out'
  #         year_key, decimal_date - date items
  #         Qm3_s - streamflow, cubic meters per second, original resolution
  #         sQm3_s - 'smoothed streamflow', centered moving average
  #         d - window width (hours) of centered moving average
  #         ID - station ID
  # -------------------------------------------------
  
  out <- data.table(year_key=numeric(),decimaldate=numeric(),
                    Qm3_s=numeric(),sQm3_s=numeric(),d=numeric(),
                    ID=character())
  
  # don't have enough R memory to interpolate the entire dataset
  # to hourly values and run operations on the whole thing, so
  # split by station, find the duration-specific annual maxima
  # and then delete the interpolated station chunk to free up
  # memory
  
  DTt <- split(DT,DT$ID)
  
  for(i in unique(DT$ID)){
    
    DTi <- DTt[[i]]
    
    # set up the vector of gridpoints to interpolate *to*. hourly spacing.
    xgrd <- DTi[,
                lapply(.SD, function(x) decimal_date(seq(x[1],x[.N],
                                                        by="hour"))), 
                .SDcols="date",
                by=c("year_key")]
    
    # interpolate to hourly values using stats::approx
    hourlystreamflow <- DTi[,c(
      list(grdpts = xgrd[,get("date")]),
      lapply(.SD,function(col) stats::approx(x = decimaldate, 
                                             y = Qm3_s, 
                                             xout = xgrd[,get("date")])$y)
    ), 
    .SDcols = c("decimaldate","Qm3_s"),
    by=c("year_key")]
    hourlystreamflow[,decimaldate:=NULL]
    setnames(hourlystreamflow,"grdpts","decimaldate")
    
    # smooth the interpolated streamflow data with a centered moving average.
    # find the annual maxima from the smoothed streamflow data. Save the 
    # annual maxima. 
    
    for(di in dvec){
      
      hourlystreamflow[,
                       sQm3_s := frollmean(Qm3_s,di,align="center"),
                       by="year_key"]
      
      # find the index of the annual maxima
      idx <- hourlystreamflow[, 
                              .I[which.max(sQm3_s)], 
                              by=c("year_key")]$V1
      
      # select the annual maxima 
      am <- hourlystreamflow[idx,]
      am[,d:=di]; am[,ID:=i]
      
      # save the annual maxima
      out <- rbind(out,am)
    }
    
    # remove the station-specific data items to free up memory
    rm(xgrd,hourlystreamflow)
    gc()
  }
  return(out)
}