weather = setClass(Class = "Weather",
                   slots = list(
                     date = "character",
                     temp = "numeric",
                     temp_min = "numeric",
                     temp_max = "numeric",
                     pressure = "numeric",
                     humidity = "numeric",
                     dt = "POSIXt",
                     month_max_temp = "numeric",
                     month_min_temp = "numeric"),
                   prototype = list(
                     "date"="April",
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

setMethod("show",
          signature = "Weather",
          definition = function(object){
            cat("Weather observation for ", object@date)
          }
)