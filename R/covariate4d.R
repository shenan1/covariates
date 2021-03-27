#' Calculate covariates at train.data locations and train.times, 
#' using quadrilinear (3D + T) interpolation, 
#' for a sigma coordinate system, where depth = f(lon, lat, depth, time)
#' 
#' @param COV MASS-4D CHL
#' 
#' @return Covariates at train.data locations and train.times.
covariate.4d <- function(train.length, train.data, COV, train.times) {
  covariate <- integer(train.length)
  for (i in 1:train.length) {
    if (sum(is.na(train.data[i,])) != 0) {
      covariate[i] <- NA} else if (train.data$depth[i] == 0) {
        covariate[i] <- NA} else {
          lon.nearby <- covariates:::find.index(train.data$lon[i], COV$lon)
          lat.nearby <- covariates:::find.index(train.data$lat[i], COV$lat)
          time.nearby <- covariates:::find.index(train.times[i], COV$time)
          covariate1 <- interp.surface(list(x=COV$depth[lon.nearby[1], lat.nearby[1], , time.nearby[1]], 
                                            y=COV$time, 
                                            z=COV$var[lon.nearby[1], lat.nearby[1], , ]), 
                                       cbind(x=max(train.data$depth[i], min(COV$depth[lon.nearby[1], lat.nearby[1], , time.nearby[1]])), 
                                             y=train.times[i]))
          covariate2 <- interp.surface(list(x=COV$depth[lon.nearby[2], lat.nearby[1], , time.nearby[1]], 
                                            y=COV$time, 
                                            z=COV$var[lon.nearby[2], lat.nearby[1], , ]), 
                                       cbind(x=max(train.data$depth[i], min(COV$depth[lon.nearby[2], lat.nearby[1], , time.nearby[1]])), 
                                             y=train.times[i]))
          covariate3 <- interp.surface(list(x=COV$depth[lon.nearby[1], lat.nearby[2], , time.nearby[1]], 
                                            y=COV$time, 
                                            z=COV$var[lon.nearby[1], lat.nearby[2], , ]), 
                                       cbind(x=max(train.data$depth[i], min(COV$depth[lon.nearby[1], lat.nearby[2], , time.nearby[1]])), 
                                             y=train.times[i]))
          covariate4 <- interp.surface(list(x=COV$depth[lon.nearby[2], lat.nearby[2], , time.nearby[1]], 
                                            y=COV$time, 
                                            z=COV$var[lon.nearby[2], lat.nearby[2], , ]), 
                                       cbind(x=max(train.data$depth[i], min(COV$depth[lon.nearby[2], lat.nearby[2], , time.nearby[1]])), 
                                             y=train.times[i]))
          covariate5 <- interp.surface(list(x=COV$depth[lon.nearby[1], lat.nearby[1], , time.nearby[2]], 
                                            y=COV$time, 
                                            z=COV$var[lon.nearby[1], lat.nearby[1], , ]), 
                                       cbind(x=max(train.data$depth[i], min(COV$depth[lon.nearby[1], lat.nearby[1], , time.nearby[2]])), 
                                             y=train.times[i]))
          covariate6 <- interp.surface(list(x=COV$depth[lon.nearby[2], lat.nearby[1], , time.nearby[2]], 
                                            y=COV$time, 
                                            z=COV$var[lon.nearby[2], lat.nearby[1], , ]), 
                                       cbind(x=max(train.data$depth[i], min(COV$depth[lon.nearby[2], lat.nearby[1], , time.nearby[2]])), 
                                             y=train.times[i]))
          covariate7 <- interp.surface(list(x=COV$depth[lon.nearby[1], lat.nearby[2], , time.nearby[2]], 
                                            y=COV$time, 
                                            z=COV$var[lon.nearby[1], lat.nearby[2], , ]), 
                                       cbind(x=max(train.data$depth[i], min(COV$depth[lon.nearby[1], lat.nearby[2], , time.nearby[2]])), 
                                             y=train.times[i]))
          covariate8 <- interp.surface(list(x=COV$depth[lon.nearby[2], lat.nearby[2], , time.nearby[2]], 
                                            y=COV$time, 
                                            z=COV$var[lon.nearby[2], lat.nearby[2], , ]), 
                                       cbind(x=max(train.data$depth[i], min(COV$depth[lon.nearby[2], lat.nearby[2], , time.nearby[2]])), 
                                             y=train.times[i]))
          if (is.na(covariate1) & is.na(covariate2)) {
            covariate12 <- NA} else if (is.na(covariate1)) {
              covariate12 <- covariate2} else if (is.na(covariate2)) {
                covariate12 <- covariate1} else {
                  covariate12 <- approx(c(COV$lon[lon.nearby[1]], COV$lon[lon.nearby[2]]),
                                        c(covariate1, covariate2),
                                        xout=train.data$lon[i], method="linear", rule=1, ties=mean)$y}
          if (is.na(covariate3) & is.na(covariate4)) {
            covariate34 <- NA} else if (is.na(covariate3)) {
              covariate34 <- covariate4} else if (is.na(covariate4)) {
                covariate34 <- covariate3} else {
                  covariate34 <- approx(c(COV$lon[lon.nearby[1]], COV$lon[lon.nearby[2]]),
                                        c(covariate3, covariate4),
                                        xout=train.data$lon[i], method="linear", rule=1, ties=mean)$y}
          if (is.na(covariate5) & is.na(covariate6)) {
            covariate56 <- NA} else if (is.na(covariate5)) {
              covariate56 <- covariate6} else if (is.na(covariate6)) {
                covariate56 <- covariate5} else {
                  covariate56 <- approx(c(COV$lon[lon.nearby[1]], COV$lon[lon.nearby[2]]),
                                        c(covariate5, covariate6),
                                        xout=train.data$lon[i], method="linear", rule=1, ties=mean)$y}
          if (is.na(covariate7) & is.na(covariate8)) {
            covariate78 <- NA} else if (is.na(covariate7)) {
              covariate78 <- covariate8} else if (is.na(covariate8)) {
                covariate78 <- covariate7} else {
                  covariate78 <- approx(c(COV$lon[lon.nearby[1]], COV$lon[lon.nearby[2]]),
                                        c(covariate7, covariate8),
                                        xout=train.data$lon[i], method="linear", rule=1, ties=mean)$y}
          if (is.na(covariate12) & is.na(covariate34)) {
            covariate1234 <- NA} else if (is.na(covariate12)) {
              covariate1234 <- covariate34} else if (is.na(covariate34)) {
                covariate1234 <- covariate12} else {
                  covariate1234 <- approx(c(COV$lat[lat.nearby[1]], COV$lat[lat.nearby[2]]),
                                          c(covariate12, covariate34),
                                          xout=train.data$lat[i], method="linear", rule=1, ties=mean)$y}
          if (is.na(covariate56) & is.na(covariate78)) {
            covariate5678 <- NA} else if (is.na(covariate56)) {
              covariate5678 <- covariate78} else if (is.na(covariate78)) {
                covariate5678 <- covariate56} else {
                  covariate5678 <- approx(c(COV$lat[lat.nearby[1]], COV$lat[lat.nearby[2]]),
                                          c(covariate56, covariate78),
                                          xout=train.data$lat[i], method="linear", rule=1, ties=mean)$y}
          if (is.na(covariate1234) & is.na(covariate5678)) {
            covariate[i] <- NA} else if (is.na(covariate1234)) {
              covariate[i] <- covariate5678} else if (is.na(covariate5678)) {
                covariate[i] <- covariate1234} else {
                  covariate[i] <- approx(c(COV$time[time.nearby[1]], COV$time[time.nearby[2]]),
                                         c(covariate1234, covariate5678),
                                         xout=train.times[i], method="linear", rule=1, ties=mean)$y}
          }
    }
  return(covariate)}
