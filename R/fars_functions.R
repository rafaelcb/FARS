#' Read FARS file
#'
#' This function checks if the input file exists and then reads it. If the file
#' does not exist the function will stop with an error.
#'
#' @param filename A string indicating the path to the csv file with data.
#'
#' @return The function returns a tibble with the data in the  file.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{fars_read("accident_1995.csv.bz2")}
#' \dontrun{fars_read("accident_2010.csv.bz2")}
#'
#' @export
fars_read <- function(filename) {
    if (!file.exists(filename))
        stop("file '", filename, "' does not exist")
    data <- suppressMessages({
        readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
}

#' Make filename for Fatality Analysis Reporting System data
#'
#' This function creates a filename corresponding to the Fatality Analysis
#' Reporting System (FARS) csv files for a specific year.
#'
#' @param year A numeric value indicating the year for which the data is going
#'   to be read.
#'
#' @return A string containing the name of the file with FARS data for the input
#'   year.
#'
#' @note The csv files with the data need to be downloaded previously from the
#'   US National Highway Traffic Safety Administration's Fatality Analysis
#'   Reporting System website
#'   (\url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars})
#'
#' @examples
#' make_filename(2010)
#' make_filename(2015)
#'
#' @export
make_filename <- function(year) {
    year <- as.integer(year)
    sprintf("accident_%d.csv.bz2", year)
}

#' Read Fatality Analysis Reporting System data one or more years
#'
#' This function reads the Fatality Analysis Reporting System (FARS) data from
#' local csv files for one or multiple years. The function returns NULL if the
#' information is not available for a specific year.
#'
#' @param years A numeric vector of years. Uses a YYYY specification.
#'
#' @return A list including tibbles with the FARS data for each of the input
#'   years.
#'
#' @examples
#' \dontrun{fars_read_years(2010)}
#' \dontrun{fars_read_years(2010:2015)}
#'
#' @importFrom dplyr mutate select
#'
#' @note The csv files with the data need to be downloaded previously from the
#'   US National Highway Traffic Safety Administration's Fatality Analysis
#'   Reporting System website
#'   (\url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars})
#'
#' @export
fars_read_years <- function(years) {
    lapply(years, function(year) {
        file <- make_filename(year)
        tryCatch({
            dat <- fars_read(file)
            dplyr::mutate_(dat, "year" = year) %>%
                dplyr::select_("MONTH", "year")
        }, error = function(e) {
            warning("invalid year: ", year)
            return(NULL)
        })
    })
}

#' Summarise data for multiple years of Fatality Analysis Reporting System data
#'
#' This function returns a summary of the number of fatalities in motor vehicle
#' crashes for multiple years. The summary is done by year and month.
#'
#' @inheritParams fars_read_years
#'
#' @return A tibble showing the number of fatalities by month and year.
#'
#' @note The csv files with the data need to be downloaded previously from the
#'   US National Highway Traffic Safety Administration's Fatality Analysis
#'   Reporting System website
#'   (\url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars})
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{fars_summarize_years(2010)}
#' \dontrun{fars_summarize_years(2012:2015)}
#' @export
fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>%
        dplyr::group_by_("year", "MONTH") %>%
        dplyr::tally() %>%
        tidyr::spread_(key_col = "year", value_col = "n")
}

#' Create map with Fatality Analysis Reporting System data
#'
#' This function creates a map with fatalities in motor vehicle crashes for a
#' specific year and state. The function fails if an invalid year or state is
#' passed as input.
#'
#' @param state.num A numeric value with the state number to plot
#' @param year A numeric value with the year for which the data will be plotted.
#'
#' @note The csv files with the data need to be downloaded previously from the
#'   US National Highway Traffic Safety Administration's Fatality Analysis
#'   Reporting System website
#'   (\url{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars})
#'
#' @return Plots a map with points marking the locations of accidents.
#'
#' @examples
#' \dontrun{fars_map_state(3, 2015)}
#'
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
    filename <- make_filename(year)
    data <- fars_read(filename)
    state.num <- as.integer(state.num)

    if (!(state.num %in% unique(data$STATE)))
        stop("invalid STATE number: ", state.num)
    data.sub <- dplyr::filter_(data, paste0("STATE == ", state.num, "L"))
    if (nrow(data.sub) == 0L) {
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
