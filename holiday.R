# ================================================================== #
# holiday class and methods
# ================================================================== #


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

date_table_loc <- 'H:/scripts/r/holiday'
source(file = file.path(date_table_loc, 'date.table.R'))


# Classes
# ================================================================== #

holiday <- function(Name = '', Date = NA, Month = NA, MDay = NA,
                    Weekday = NA, NWeekday = NA, Bump = T,
                    Priority = 1){
  #' constructor for holiday class; one or more of args are used to
  #' determine date on which holiday falls every year
  #' Name should be character
  #' Date should be Date
  #' Rest should be integers; Month and Weekday can also take
  #' character args corresponding to month.name or month.abb in case
  #' of former and wkdy_abb in case of latter; NWeekday can be set to
  #' Inf for last weekday of month (e.g., Memorial Day)
  #' Sunday = 0 is assumed for Weekday
  #' Any combination of arguments may be provided, but the resolution
  #' in methods is: 1) Month and MDay - regular holiday falling on the
  #' same date every year, (e.g. Christmas); 2) Month, Weekday, and
  #' NWeekday - regular holiday falling on the same weekday each year
  #' (e.g. Thanksgiving); 3) Date - irregular holiday (e.g. Easter).
  #' The object attributes are attempted to be read in that order, and
  #' then an error is thrown if all three fail.
  #' Bump means that a holiday should be moved to the closest business
  #' day if it falls on a non-business day and Priority is used to
  #' decide which holiday to classify a date as if more than one falls
  #' on that day.
  #' =============================================================== #
  # Validate Name arg
  stopifnot(is.character(Name))
  # TODO - validate Date arg? other classes?
  # Validate Month arg
  if(!is.na(Month)){
    if(is.character(Month)){
      if(Month %in% month.abb){
        Month <- which(month.abb == Month)
      } else if(Month %in% month.name){
        Month <- which(month.name == Month)
      } else {
        stop(paste0('Month \'', Month, '\' not recognized'))
      }
    } else {
      m <- tryCatch(
        {as.integer(Month)},
        error = simpleError('Month must be convertible to integer')
      )
      if(m != Month){
        stop('Don\'t know how to interpret non-integer month')
      }
      Month <- m
    }
  }
  # validate MDay arg
  if(!is.na(MDay)){
    md <- tryCatch(
      {as.integer(MDay)},
      error = simpleError('MDay must be convertible to integer')
    )
    if(md != MDay){
      stop('Don\'t know how to interpret non-integer day of month')
    }
    MDay <- md
  }
  # validate Weekday arg
  if(!is.na(Weekday)){
    if(is.character(Weekday)){
      if(Weekday %in% wkdy_abb){
        Weekday <- which(wkdy_abb == Weekday) - 1
      } else {
        stop(paste0('Weekday \'', Weekday, '\' not recognized'))
      }
    } else {
      wd <- tryCatch(
        {as.integer(Weekday)},
        error = simpleError('Weekday must be convertible to integer')
      )
      if(wd != Weekday){
        stop('Don\'t know how to interpret non-integer Weekday')
      }
      Weekday <- wd
    }
  }
  # validate NWeekday arg
  if(!is.na(NWeekday)){
    if(NWeekday != Inf){
      nw <- tryCatch(
        {as.integer(NWeekday)},
        error = simpleError('NWeekday must be convertible to integer')
      )
      if(nw != NWeekday){
        stop('Don\'t know how to interpret non-integer NWeekday')
      }
      NWeekday <- nw
    }
  }
  # create object using cleaned args
  return(
    structure(
      list(
        Name = Name,
        Date = Date,
        Month = Month,
        MDay = MDay,
        Weekday = Weekday,
        NWeekday = NWeekday,
        Bump = Bump,
        Priority = Priority
      ),
      class = 'holiday'
    )
  )
}


# Functions
# ================================================================== #

easter_eastern <- function(years){
  #' returns holiday object encoding the date on which Easter falls
  #' according to the Eastern liturgical calendar
  #' R implementation of the Meeus algorithm
  #' Only valid between 1900 and 2099
  #' =============================================================== #
  stopifnot(all(years > 1899 & years < 2100))
  lapply(
    years,
    function(y){
      a <- y %% 4
      b <- y %% 7
      c <- y %% 19
      d <- (19*c + 15) %% 30
      e <- (2*a + 4*b - d + 34) %% 7
      return(
        holiday(
          Name = 'Easter Sunday (Eastern)',
          Date = as.Date(
            paste(
              y,
              (d + e + 114) %/% 31,
              ((d + e + 114) %% 31) + 1,
              sep = '-'
            )
          ) + 13,
          Bump = F
        )
      )
    }
  )
}

easter_western <- function(years){
  #' returns holiday object encoding the date on which Easter falls
  #' according to the Western
  #' liturgical calendar
  #' R implementation of the Meeus/Jones/Butcher algorithm
  #' =============================================================== #
  lapply(
    years,
    function(y){
      a <- y %% 19
      b <- y %/% 100
      c <- y %% 100
      d <- b %/% 4
      e <- b %% 4
      f <- (b + 8) %/% 25
      g <- (b - f + 1) %/% 3
      h <- (19*a + b - d - g + 15) %% 30
      i <- c %/% 4
      k <- c %% 4
      l <- (32 + 2*e + 2*i - h - k) %% 7
      m <- (a + 11*h + 22*l) %/% 451
      return(
        holiday(
          Name = 'Easter Sunday (Western)',
          Date = as.Date(
            paste(
              y,
              (h + l - 7*m + 114) %/% 31,
              ((h + l - 7*m + 114) %% 31) + 1,
              sep = '-'
            )
          ),
          Bump = F
        )
      )
    }
  )
}

easter <- function(years, tradition = 'western', surrounding = F){
  #' returns list of holiday objects encoding the easter holiday for
  #' the years given according to the tradition specified (one of
  #' 'western' or 'eastern'; partial matching is used). If 
  #' 'surrounding' is TRUE, then objects for Good Friday, Holy
  #' Saturday, and Easter Monday are also included
  #' =============================================================== #
  if(substr(tolower(tradition), 1, 1) == 'w'){
    tradition <- 'Western'
    ret <- easter_western(years)
  } else if(substr(tolower(tradition), 1, 1) == 'e'){
    tradition <- 'Eastern'
    ret <- easter_eastern(years)
  } else {
    stop(paste('tradition', tradition, 'not recognized'))
  }
  if(surrounding){
    ret <- unlist(
      lapply(
        ret,
        function(x){
          dt <- x$Date
          pr <- x$Priority
          return(
            list(
              holiday(
                Name = paste0('Good Friday (', tradition, ')'),
                Date = dt - 2,
                Bump = F,
                Priority = pr + 1
              ),
              holiday(
                Name = paste0('Holy Saturday (', tradition, ')'),
                Date = dt - 1,
                Bump = F,
                Priority = pr + 1
              ),
              x,
              holiday(
                Name = paste0('Easter Monday (', tradition, ')'),
                Date = dt + 1,
                Bump = F,
                Priority = pr + 1
              )
            )
          )
        }
      ),
      recursive = F
    )
  }
  return(ret)
}

falls_on <- function(hol, years){
  #' Returns a list of holiday objects giving exact dates on which
  #' holiday falls in each year in years. This date is solved for
  #' using the holiday object (should be result of a call to
  #' holiday()) passed in via hol argument. See doc for holiday class
  #' for order in which args are attempted to be read.
  #' If holiday is irregular, a list including only hol is returned.
  #' =============================================================== #
  if(!is.na(hol$Month)){
    if(!is.na(hol$MDay)){
      return(
        lapply(
          years,
          function(y){
            hol_dt <- as.Date(
              paste(
                y,
                hol$Month,
                hol$MDay,
                sep = '-'
              )
            )
            if(hol$Bump){
              hol_dt <- (
                hol_dt
                + (1 * (strftime(hol_dt, '%w') == '0'))
                - (1 * (strftime(hol_dt, '%w') == '6'))
              )
            }
            return(
              holiday(
                Name = hol$Name,
                Date = hol_dt,
                Month = hol$Month,
                MDay = hol$MDay,
                Bump = hol$Bump,
                Priority = hol$Priority
              )
            )
          }
        )
      )
    } else if(!is.na(hol$Weekday) & !is.na(hol$NWeekday)){
      dt <- date.table(years)
      if(hol$NWeekday == Inf){
        return(
          lapply(
            years,
            function(y){
              hol_dt <- dt[
                dt[
                  ,
                  .I[Year == y &
                       Month == hol$Month &
                       Weekday == wkdy_abb[hol$Weekday + 1] &
                       NWeekday == max(NWeekday)],
                  by = .(Year, Month, Weekday)
                ]$V1,
                Date
              ]
              if(hol$Bump){
                hol_dt <- (
                  hol_dt
                  + (1 * (strftime(hol_dt, '%w') == '0'))
                  - (1 * (strftime(hol_dt, '%w') == '6'))
                )
              }
              return(
                holiday(
                  Name = hol$Name,
                  Date = hol_dt,
                  Month = hol$Month,
                  Weekday = hol$Weekday,
                  NWeekday = hol$NWeekday,
                  Bump = hol$Bump,
                  Priority = hol$Priority
                )
              )
            }
          )
        )
      } else {
        return(
          lapply(
            years,
            function(y){
              hol_dt <- dt[
                Year == y &
                  Month == hol$Month &
                  Weekday == wkdy_abb[hol$Weekday + 1] &
                  NWeekday == hol$NWeekday,
                Date
                ]
              if(hol$Bump){
                hol_dt <- (
                  hol_dt
                  + (1 * (strftime(hol_dt, '%w') == '0'))
                  - (1 * (strftime(hol_dt, '%w') == '6'))
                )
              }
              return(
                holiday(
                  Name = hol$Name,
                  Date = hol_dt,
                  Month = hol$Month,
                  Weekday = hol$Weekday,
                  NWeekday = hol$NWeekday,
                  Bump = hol$Bump,
                  Priority = hol$Priority
                )
              )
            }
          )
        )
      }
    } else {
      stop(
        paste(
          'if Month is provided, then one of MDay or both',
          'Weekday and NWeekday must be provided'
        )
      )
    }
  } else {
    return(list(hol))
  }
}

federal_holidays <- function(surrounding = F){
  #' returns list of holiday objects encoding all US federal holidays
  #' setting surround to T will add New Year's Eve and Christmas Eve
  #' to the list
  #' =============================================================== #
  holidays <- c('New_Years_Day', 'Martin_Luther_King_Jr_Day',
                'Presidents_Day', 'Memorial_Day', 'Independence_Day',
                'Labor_Day', 'Columbus_Day', 'Veterans_Day',
                'Thanksgiving_Day', 'Christmas_Day')
  eves <- c('New_Years_Eve', 'Christmas_Eve')
  
  ret <- vector(mode = 'list', length = length(holidays))
  names(ret) <- holidays
  
  ret$New_Years_Day <- holiday(
    Name = 'New Year\'s Day',
    Month = 1,
    MDay = 1
  )
  ret$Martin_Luther_King_Jr_Day <- holiday(
    Name = 'Martin Luther King, Jr. Day',
    Month = 1,
    Weekday = 1,
    NWeekday = 3,
    Bump = F
  )
  ret$Presidents_Day <- holiday(
    Name = 'Presidents\' Day',
    Month = 2,
    Weekday = 1,
    NWeekday = 3,
    Bump = F
  )
  ret$Memorial_Day <- holiday(
    Name = 'Memorial Day',
    Month = 5,
    Weekday = 1,
    NWeekday = Inf,
    Bump = F
  )
  ret$Independence_Day <- holiday(
    Name = 'Independence Day',
    Month = 7,
    MDay = 4
  )
  ret$Labor_Day <- holiday(
    Name = 'Labor Day',
    Month = 9,
    Weekday = 1,
    NWeekday = 1,
    Bump = F
  )
  ret$Columbus_Day <- holiday(
    Name = 'Columbus Day',
    Month = 10,
    Weekday = 1,
    NWeekday = 2,
    Bump = F
  )
  ret$Veterans_Day <- holiday(
    Name = 'Veterans Day',
    Month = 11,
    MDay = 11,
    Bump = T
  )
  ret$Thanksgiving_Day <- holiday(
    Name = 'Thanksgiving Day',
    Month = 11,
    Weekday = 4,
    NWeekday = 4,
    Bump = F
  )
  ret$Christmas_Day <- holiday(
    Name = 'Christmas Day',
    Month = 12,
    MDay = 25,
    Bump = T
  )
  if(surrounding){
    ret2 <- vector(mode = 'list', length = length(eves))
    names(ret2) <- eves

    ret2$New_Years_Eve <- holiday(
      Name = 'New Year\'s Eve',
      Month = 12,
      MDay = 31,
      Bump = F,
      Priority = 2
    )
    ret2$Christmas_Eve <- holiday(
      Name = 'Christmas Eve',
      Month = 12,
      MDay = 24,
      Bump = F,
      Priority = 2
    )
    ret <- c(ret, ret2)
  }
  return(ret)
}
