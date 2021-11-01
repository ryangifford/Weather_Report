

library(taskscheduleR)


fichero <- "C:\\Users\\Ryan\\Desktop\\weather model validation\\NWS_API.R"

taskscheduler_create(taskname = "NWS_API", 
                     rscript = fichero,
                     schedule = "MINUTE", 
                     starttime = format(Sys.time(), "%H:%M:%S"), 
                     startdate = format(Sys.Date(), "%m/%d/%Y"))
