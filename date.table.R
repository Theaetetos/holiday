# ================================================================== #
# date.table class and methods
# ================================================================== #


#' date.tables are defined as data.tables with the following columns:
#' Date, Year, Month, Weekday, and NWeekday, i.e. all of the data
#' necessary to find holidays. A date.table may have more columns. It
#' is also subject to the further constraint that all dates be
#' continuous.


# Setup
# ================================================================== #

# Packages

load_pkgs <- function(pkgs){
  #' Loads a list of packages, installing them if not already
  #' installed
  #' pkgs is a vector of package names as strings
  #' =============================================================== #
  for(pkg in pkgs){
    if(!(require(pkg, character.only=TRUE))){
      install.packages(pkg)
      library(pkg, character.only=TRUE)
    }
  }
}

load_pkgs(c('bit64', 'data.table'))

# Variables

wkdy_abb <- c('Sun', 'M', 'T', 'W', 'Tr', 'F', 'Sat')


# Functions
# ================================================================== #


date.table <- function(object, datecol = 'Date'){
  #' generic constructor for date.tables
  #' x may only be class integer or data.table
  #' datecol is name of Date column according to class definition
  #' above; it is either searched for or used to create it depending
  #' on the method called.
  #' =============================================================== #
  UseMethod('date.table', object)
}

date.table.integer <- function(object, datecol = 'Date'){
  # ensure that vector of years is continuous - violates necessary
  # property of date.table otherwise
  if(!all(abs(diff(object)) == 1)){
    stop('Integer vector of years must be continuous')
  }
  ret <- data.table(
    Date = seq.Date(
      from = as.Date(paste(min(object), 1, 1, sep = '-')),
      to = as.Date(paste(max(object), 12, 31, sep = '-')),
      by = 1
    )
  )
  ret[
    ,
    `:=`(
      Year = year(Date),
      Month = month(Date),
      Weekday = factor(
        strftime(Date, '%w'),
        levels = as.character(0:6),
        labels = wkdy_abb
      )
    )
  ]
  ret[, NWeekday := seq_len(.N), by = .(Year, Month, Weekday)]
  setnames(ret, old = c('Date'), new = c(datecol))
  return(ret)
}

date.table.double <- function(object, datecol = 'Date'){
  if(as.integer(object) == object){
    date.table.integer(object, datecol)
  } else {
    stop('Don\'t know how to interpret non-integer year')
  }
}

date.table.numeric <- function(object, datecol = 'Date'){
  if(as.integer(object) == object){
    date.table.integer(object, datecol)
  } else {
    stop('Don\'t know how to interpret non-integer year')
  }
}

date.table.data.table <- function(object, datecol){
  # make sure datecol is in object
  if(!(datecol %in% names(object))){
    stop(paste0('Column \'', datecol, '\'not found in object'))
  }
  # ensure that datecol is continuous sequence of days - violates
  # necessary property of date.table otherwise
  dts <- object[
    ,
    abs(as.integer(diff(get(datecol))))
  ]
  if(!all(dts == 1)){
    stop(
      paste0(
        'Column \'',
        datecol,
        '\' not a continuous sequence of days'
      )
    )
  }
  # copy object to add columns to
  ret <- copy(object)
  # Pad days such that first date begins on first of month; otherwise
  # NWeekday count will be wrong
  min_dt <- ret[, min(get(datecol))]
  if(mday(min_dt) != 1){
    mpad <- data.table(
      Date = seq.Date(
        from = as.Date(
          paste(
            year(min_dt),
            month(min_dt),
            1,
            sep = '-'
          )
        ),
        to = min_dt - 1,
        by = 1
      )
    )
    setnames(mpad, old = c('Date'), new = datecol)
    ret <- rbind(
      mpad,
      ret,
      fill = T
    )
  }
  ret[
    ,
    `:=`(
      Year = year(get(datecol)),
      Month = month(get(datecol)),
      Weekday = factor(
        strftime(get(datecol), '%w'),
        levels = as.character(0:6),
        labels = wkdy_abb
      )
    )
  ]
  ret[, NWeekday := seq_len(.N), by = .(Year, Month, Weekday)]
  return(ret[get(datecol) >= min_dt, ])
}
