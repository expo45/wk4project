% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Plot the Location of FARS incidents in a state}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{An (integer) state identification number as listed in the FARS data files.}

\item{year}{An integer value corresponding to the year for which the map should be plotted.}
}
\value{
a plot showing the location (latitude and longitude) of all FARS incidents, plotted on a map of
   the state identified by \code{state.num} during the specified \code{year}.
}
\description{
This function produces a map with a plot of the location of all FARS incidents
   in a given state during a given year.
}
\note{
This function will stop and return the message \code{"invalid STATE number: state.num"} if a \code{state.num}
   value that does not correspond to a state listed in the data file is input.

This funciton will stop and return the message \code{"no accidents to plot"} if there are no FARS
   incidents in the specified state during the specified year.

This function will result in an error if the \code{year} specified does not correspond to a
   FARS CSV file in the current working directory with the proper filename syntax (\code{accident_YYYY.csv.bz2})
}
\examples{
\dontrun{fars_map_state(6, 2014)}

}
