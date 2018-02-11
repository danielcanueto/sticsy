# sticsy

Statistical total correlation spectroscopy with new dependence measures (distance correlation, distance covariance, MIC, gMIC, TIC)

Installation: devtools::install_github("danielcanueto/sticsy")

Use:

data=MTBLS1()

results=sticsy(data$dataset,data$ppm,c(7.58,7.54),method='dcov',visualization_method='median')