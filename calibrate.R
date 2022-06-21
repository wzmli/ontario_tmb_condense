# ---------------------------
# Calibrate the model to observed data
# ---------------------------

model_calibrated = calibrate_flexmodel(model_uncalibrated
                                       # , optimizer = 'nlminb'
                                       )

# ---------------------------
# Script output
# ---------------------------

parameters <- addEnvironment(parameters,c("model_calibrated"))

