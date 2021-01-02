#' Load covariate data, including 3D, time and 1 variable.
#' Includes flags for variable and data source.
#' Also returns origin from which time is counted (excludes MASS data).
#' 
#' @param file path to file
#' @param variable uo, vo, chlf, chlr, oxyf, oxyr
#' @param source MASS
#' 
#' @return Covariate data and origin.
load.data.3d <- function(file, variable = 'thetao', source = 'CMEMS') {
  if (source == 'MASS') {
    lon.id <- 'longitudes'
    lat.id <- 'latitutdes'
    depth.id <- 'depths'} else {
      lon.id <- 'lon'
      lat.id <- 'lat'
      depth.id <- 'depth'}
  
  covariate <- nc_open(file)
  lon <- ncvar_get(covariate, varid = lon.id)
  lat <- ncvar_get(covariate, varid = lat.id)
  depth <- ncvar_get(covariate, varid = depth.id)
  
  # var <- case_when(
  #   variable == 'uo' ~ ncvar_get(covariate, varid = 'uo'),
  #   variable == 'vo' ~ ncvar_get(covariate, varid = 'vo'),
  #   variable == 'chlf' ~ ncvar_get(covariate, varid = 'ChlTot_fore'),
  #   variable == 'chlr' ~ ncvar_get(covariate, varid = 'ChlTot_rean'),
  #   variable == 'oxyf' ~ ncvar_get(covariate, varid = 'Ox_fore'),
  #   variable == 'oxyr' ~ ncvar_get(covariate, varid = 'Oxy_rean'),
  #   variable == 'thetao' ~ ncvar_get(covariate))
  
  if (variable == 'uo') {
    var <- ncvar_get(covariate, varid = 'uo')} else if (variable == 'vo') {
      var <- ncvar_get(covariate, varid = 'vo')} else if (variable == 'chlf') {
        var <- ncvar_get(covariate, varid = 'ChlTot_fore')} else if (variable == 'chlr') {
          var <- ncvar_get(covariate, varid = 'ChlTot_rean')} else if (variable == 'oxyf') {
            var <- ncvar_get(covariate, varid = 'Ox_fore')} else if (variable == 'oxyr') {
              var <- ncvar_get(covariate, varid = 'Oxy_rean')} else {
                var <- ncvar_get(covariate)}
  
  if (source == 'MASS') {
    time <- (1:length(var[1,1,1,]))*3600*24 - 1800*24
    origin <- NULL} else {
      time <- ncvar_get(covariate, varid = 'time')
      text1 <- grep("since", as.character(covariate), value=TRUE)[1]            # returns first line containing "since"
      text2 <- sub(".*since ", "", text1)                                       # returns everything after "since " in text1
      origin <- substr(text2, 1, 10)}                                           # returns characters 1-10 in text2
  
  nc_close(covariate)
  rm(covariate)
  
  return(list(lon    = lon, 
              lat    = lat, 
              depth  = depth, 
              time   = time, 
              var    = var, 
              origin = origin))}
