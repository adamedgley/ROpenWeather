#' @name OpenWeather-class
#' @title OpenWeather-class
#' Contains requested weather for a given list of locations
#' @slot cityInfo List providing city information
#' @slot weather data.frame with weather observations
#' @slot obsCount Count of observation
#' @
OpenWeather = setClass(
  Class="OpenWeather", 
  slots=list(cityInfo="list", weather="data.frame", obsCount="numeric"), 
  prototype=
    list(cityInfo=list(),
         weather=data.frame("date"="April",
                            "temp"=0,          
                            "temp_min"=0,
                            "temp_max"=0,
                            "pressure"=0,
                            "humidity"=0,
                            "dt"=as.POSIXlt(Sys.Date(), origin="1970-01-01"),
                            "month_max_temp"=0,
                            "month_min_temp"=0
                            )
         )
  )


setMethod("show",
          signature(object = "OpenWeather"),
          function (object) {
            if (exists("details")){
              if(details=="weather")
                print(x@weather)
              else if(details=="cityInfo")
                print(unlist(x@cityInfo))
            } else print(x@weather)
          })

setMethod("as.data.frame",
          signature="OpenWeather",
          definition=function(x){
            return(as.data.frame(x@weather))
          }
        )
setMethod("plot",
          signature="OpenWeather",
          definition=function(x, ...){
            if (nrow(x@weather)<2) 
              return(invisible(NULL))
            ret = ggplot(data=x@weather, aes(x=dt, y=temp))
            ret = ret + geom_line()
            print(ret)
            return(invisible(ret))
          })

findCity = function(location, cnt=1) {
  stopifnot(inherits(location,"numeric"),length(cnt)==1, length(location)==2)
  
  # Create the query string
  queryString = paste("http://api.openweathermap.org/data/2.5/find?lon=",location[1],"&lat=",location[2],"&cnt=",cnt, sep="")
  
  # Download the results and parse into a list
  res = fromJSON(queryString)
  # Process results and return data.frame
  cityId = NULL
  cityName = NULL
  for (i in 1:cnt) {
    cityId = c(cityId, res$list[[i]]$id)
    cityName = c(cityName, res$list[[i]]$name)
  }
  ret = data.frame("cityId"=cityId, "cityName"=cityName)
  return(ret)
}

getWeather = function(cityId, startDate=NULL, endDate=NULL, by="day", units="metric"){
  stopifnot(is.numeric(cityId))
  if (!is.null(startDate)) {
    
    if (inherits(startDate, "POSIXt"))
      startDt = as.double(startDate)
    else if(inherits(startDate, "Date"))
      startDt = unclass(startDate) * 86400
    else if(inherits(startDate, "numeric"))
      startDt = startDate
    else
      stop("Invalid startDate - must inherit class Date, POSIXt or numeric")
    startStr = paste("&start=",startDt,sep="")
  } else
    startStr = ""
  
  if (!is.null(endDate)) {
    
    if (inherits(endDate, "POSIXt"))
      endDate = as.double(endDate)
    else if(inherits(endDate, "Date"))
      endDate = unclass(endDate) * 86400
    else if(inherits(endDate, "numeric"))
      endDate = endDate
    else
      stop("Invalid endDate - must inherit class Date, POSIXt or numeric")
    endStr = paste("&end=",endDate, sep="")
  }
  else endStr = ""
  
  if (is.null(startDate) && is.null(endDate)) 
    reqType = "weather"
  else
    reqType = "history/city"
  
  queryString = paste("http://api.openweathermap.org/data/2.5/",
                      reqType,
                      "?id=",
                      cityId,
                      startStr,
                      endStr,
                      "&type=",
                      by,
                      
                      sep=""
                      )
  #print(queryString)
  ret = fromJSON(queryString)
  ret = .newOpenWeather(ret, units=units)
  return(ret)
}


.formatMain = function(x, units=c("metric", "kelvin","fahrenheit")){
  stopifnot(units %in% c("metric", "kelvin","fahrenheit"))
  x["temp"] = .tempConversionFromK(x["temp"], to=units)
  x["temp_min"] = .tempConversionFromK(x["temp_min"], to=units)
  x["temp_max"] = .tempConversionFromK(x["temp_max"], to=units)
  ret = data.frame("temp"=x["temp"], 
                   "temp_min"=x["temp_min"],
                   "temp_max"=x["temp_max"],
                   "pressure"=x["pressure"],
                   "humidity"=x["humidity"])
  return(ret)
}

.tempConversionFromK = function(x, to="metric") {
  if (to=="metric")
    ret = x - 273.15
  else if (to=="fahrenheit")
    ret = x * (9/5) - 459.67
  else if (to=="kelvin")
    ret = x
  else 
    stop("to must be either celsius or fahrenheit")
  return(ret)
}

.newOpenWeather = function(rspns=list(), units="metric") {
  cityInfo = list("cityName"=rspns$name,
                        "cityId"=rspns$id,
                        "lon"=rspns$coord["lon"],
                        "lat"=rspns$coord["lat"]
  )
  if (!exists("list",rspns)){
    
    x = .formatMain(x=rspns$main, units)
    rn = as.POSIXlt(rspns$dt, origin='1970-01-01')
    x$dt = rn
    months(rn)
    x$date = months(rn)
    weather = x
    
  } else {
    weather = NULL
    w = rspns$list
    x = .formatMain(x=w[[1]]$main, units)
    rn = as.POSIXlt(w[[1]]$dt, origin='1970-01-01')
    for (i in 2:length(w)) {
      y = .formatMain(x=w[[i]]$main, "metric")
      x = rbind(x,y)
      rn = c(rn, as.POSIXct(w[[i]]$dt, origin='1970-01-01'))
    }
    x$dt = rn
    months(rn)
    x$date = months(rn)
    weather = x
    
  }
  ret = list("cityInfo"=cityInfo, "weather"=data.frame(weather))
  ret = OpenWeather(cityInfo=cityInfo, weather=data.frame(weather), obsCount=1)
  return(ret)
}



