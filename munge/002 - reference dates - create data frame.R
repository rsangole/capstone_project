
################################################################################
## Create a reference table of all dates
################################################################################

is.leapyear <- function(year) {
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

create.date <- function(day.of.yr,yr) {
  # print(paste(yr,day.cnt))
  my.date <- as.Date(day.of.yr - 1, origin = paste(yr,"01","01",sep="-"))
  return(my.date)
}

create.yr <- function(yr) {
  if (is.leapyear(yr)) day.cnt <- 366 
  else day.cnt <- 365
  my.dates <- sapply(1:day.cnt,create.date,yr=yr)
  return(my.dates)
}
# create.yr(2007)

create.dates <- function(start.year,end.year) {
  df <- data.frame(date=as.Date(do.call(c,lapply(start.year:end.year,create.yr))))
  df$yr <- lubridate::year(df$date)
  df$mo <- lubridate::month(df$date)
  df$day <- lubridate::day(df$date)
  df$qtr <- lubridate::quarter(df$date)
  df$wk <- lubridate::week(df$date)
  df$day.of.yr <- as.numeric(strftime(df$date, format = "%j"))
  df$day.of.wk <- format(df$date,"%w")
  df$day.of.wk.name <- weekdays(as.POSIXct(df$date), abbreviate = T)
  df$eval.day <- 1 + df$date - min(df$date)
  df$eval.wk <- ceiling(df$eval.day / 7)
  return(df)
}

ref.dates <- create.dates(2007,2018)




