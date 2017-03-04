#' Import FARS ("Fatality Analysis Reporting System") data
#'
#' This is a function for importing a single FARS data file into R for analysis.
#'
#' @param filename a valid filepath to data stored in comma separated value form
#'
#' @return This function returns a data frame containing FARS data
#'
#' @note The function will stop if the \code{filename} parameter does not refer to a
#'    valid file.
#'    If the file is not in CSV form, the function will attempt to parse it anyway,
#'    but will return parsing failure warnings. The function should not be used for
#'    non-CSV files, as it will likely not properly import the data.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{fars_read("data/accident_2013.csv.bz2")}
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Filename maker
#'
#' This function creates a string containing a standardized filename for a CSV file containing
#'    a year of FARS data. It is used as an input to other functions in the package to help them
#'    identify FARS data files in the working directory.
#'
#' @param year The year for which a filename is to be generated.
#'
#' @return Returns a string representing a filename.
#'
#' @details This function takes the year input and passes it to the \code{sprintf} function, a wrapper
#' for the C \code{sprintf} function
#'
#' @note This function requres that the year parameter be interpretable as an integer.
#'    For proper functioning, only integer values should be used, although numeric inputs
#'    will be rounded down to the nearest integer and the function will produce the requested
#'    output.
#'
#' @examples
#' make_filename(2013)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read year and month columns of multiple years worth of FARS data into R
#'
#' This function reads the month and year columns of multiple single-year FARS data files
#'    into R for analysis. It is mostly useful as an input of the \code{fars_summarize_years}
#'    function.
#'
#' @param years a vector of integers representing the year values to be read. Each value
#'    must correspond to a year with a CSV file in the working directory that can be read.
#'
#' @return a list of data frames, each containing the year and month columns of the raw data
#'    for each year in the input
#'
#' @note This function will return warnings if any years are input that do not correspond
#'    to CSV files saved with the correct filename syntax (\code{accident_(year).csv.bz2}) in
#'    the working directory. Instead of a data frame containing data from the file, it will
#'    input a \code{NULL} value for the corresponding list element and generate the following
#'    warning: \code{In value[[nL]](cond) : invalid year: YYYY}
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select
#'
#' @examples
#' \dontrun{fars_read_years(c(2013, 2014, 2015))}
#' \dontrun{fars_read_years(2013)}
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize the number of FARS incidents in each month of several years
#'
#' This function produces a summary data frame containing the number of FARS
#'    entries in each month of a single year or for each of several years.
#'
#' @param years a vector of integers representing the year values to be read. Each value
#'    must correspond to a year with a CSV file in the working directory that can be read.
#'
#' @return This function returns a data frame, with rows representing calendar months and
#'    columns for each year in the input, with the total number of FARS entries in each month
#'    for the given year.
#'
#' @note This function will only return results columns for years that correspond to CSV
#'    files saved with the correct filename syntax (\code{accident_(year).csv.bz2}) in
#'    the current working directory. For any ears that do not match such a file, no column will
#'    appear in the output and the following warnign will be generated:
#'    \code{In value[[nL]](cond) : invalid year: YYYY}
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{fars_summarize_years(c(2013,2014,2015))}
#' \dontrun{fars_read_years(2013)}
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot the Location of FARS incidents in a state
#'
#' This function produces a map with a plot of the location of all FARS incidents
#'    in a given state during a given year.
#'
#' @param state.num An (integer) state identification number as listed in the FARS data files.
#'
#' @param year An integer value corresponding to the year for which the map should be plotted.
#'
#' @return a plot showing the location (latitude and longitude) of all FARS incidents, plotted on a map of
#'    the state identified by \code{state.num} during the specified \code{year}.
#'
#' @note This function will stop and return the message \code{"invalid STATE number: state.num"} if a \code{state.num}
#'    value that does not correspond to a state listed in the data file is input.
#' @note This funciton will stop and return the message \code{"no accidents to plot"} if there are no FARS
#'    incidents in the specified state during the specified year.
#' @note This function will result in an error if the \code{year} specified does not correspond to a
#'    FARS CSV file in the current working directory with the proper filename syntax (\code{accident_YYYY.csv.bz2})
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(6, 2014)}
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}

