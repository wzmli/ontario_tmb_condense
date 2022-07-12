start_time <- Sys.time()

source("run_calibration.R")

source("run_forecast.R")

end_time <- Sys.time()
beepr::beep(sound = 5)
print(end_time - start_time)
