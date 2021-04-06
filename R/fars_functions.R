#' Reading Fatality Analysis Reporting System data
#'
#' Read Fatality Analysis Reporting System (FARS) data from
#' US National Highway Traffic Safety Administration. \cr
#' \code{fars_read} reads a file defined by \code{filename} from the current
#' working directory.
#'
#' @param filename (\emph{character}) The file to be read.
#'
#' @details
#' If \code{fars_read} cannot find the given filename in the current
#' directory an error is thrown. It is recommended to use
#' \code{\link{make_filename}} to create a filename for a given \code{year}
#' according to the FARS filenaming scheme. \cr
#' \code{\link{fars_read}} is a helper function for \code{\link{fars_read_years}}
#'
#' @return FIBS data is returned as tibble (see \code{\link[dplyr]{tbl_df}})
#'
#' @references US National Highway Traffic Safety Administration \cr
#' (\href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars})
#'
#' @seealso \code{\link{make_filename}}, \code{\link{fars_read_years}}
#'
#' @examples
#' \dontrun{
#' year <- 2014
#' df <- fars_read(make_filename(year))
#' }
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#' @export
fars_read <- function(filename) {
    if(!file.exists(filename))
        stop("file '", filename, "' does not exist")
    data <- suppressMessages({
        readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
}



#' Create a Fatality Analysis Reporting System filename
#'
#' \code{make_filename} creates a filename for a given \code{year} according to
#' the FARS (Fatality Analysis Reporting System) filenaming scheme.
#'
#' @param year (\emph{character} or (\emph{integer}) ) The year for which the
#' filename will be created.
#'
#' @details
#' Both \emph{integer} and \emph{character} are allowed for \code{year}.
#' \code{year} is coerced to integer, if \code{year} cannot be coerced to integer
#' a warning is thrown and the returned filename may be invalid. \cr
#' \code{\link{make_filename}} is a helper function for \code{\link{fars_read}}.
#'
#' @return  A character containing the filename.
#'
#' @references US National Highway Traffic Safety Administration \cr
#' (\href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars})
#'
#' @seealso \code{\link{fars_read}}
#'
#' @examples
#' \dontrun{
#' flname <- make_filename("2014")
#' flname <- make_filename(2014)
#' }
#'
#' @export
make_filename <- function(year) {
    year <- as.integer(year)
    sprintf("accident_%d.csv.bz2", year)
}



#' Read and wrangle multiple Fatality Analysis Reporting System files
#'
#' \code{fars_read_years} reads Fatality Analysis Reporting System (FARS) files
#' for multiple \code{years} and rearranges the data such that each row contains
#' one incident with columns \emph{MONTH} and \emph{year}.
#'
#' @param years (\emph{character} or (\emph{integer}) ) A vector containing the desired years.
#' Both \emph{numeric} and \emph{character} vectors are allowed for \code{years}.
#' \code{years} is coerced to integer. If \code{years} cannot be coerced to integer,
#' \code{fars_read_years} will result in an error as filenames cannot be created
#' properly.
#'
#' @details
#' \emph{list} is also allowed for \code{year} but not recommended.
#' Files must exist in current working directory. \cr
#' \code{fars_read_years} is a helper function for \code{\link{fars_summarize_years}}.
#'
#' @return A list containing the rearranged datasets for each given year.
#'
#' @references US National Highway Traffic Safety Administration \cr
#' (\href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars})
#'
#' @seealso \code{\link{fars_read}}, \code{\link{make_filename}}, \code{\link{fars_summarize_years}}
#'
#' @examples
#' \dontrun{
#' lst <- fars_read_years(c(2014, 2015))
#' lst <- fars_read_years(c("2014", "2015"))
#' # also usage of lists is allowed:
#' lst <- fars_read_years(list(2014, "2015"))
#' }
#'
#' @importFrom dplyr mutate_ select_
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


#' Summarize Fatality Analysis Reporting System data
#'
#' Summarizes the Fatality Analysis Reporting System (FARS) incidents per month
#' and year for given \code{years}.
#'
#' @inheritParams fars_read_years
#'
#' @details
#' FARS data is read using \code{\link{fars_read_years}}. FARS incidents
#' are grouped by year and month and then counted. \cr
#' Files must exist in current working directory.
#'
#' @return Summarized data is returned as tibble (see \code{\link[dplyr]{tbl_df}})
#' where for each month (rows) the number of incidents are given for each year (columns).
#'
#' @references US National Highway Traffic Safety Administration \cr
#' (\href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars})
#'
#' @seealso \code{\link{fars_read_years}}
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2014, 2015))
#' smry <- fars_summarize_years(list(2014, "2015"))
#' }
#'
#' @importFrom dplyr bind_rows group_by_ summarize_ n
#' @importFrom tidyr spread_
#' @importFrom magrittr `%>%`
#' @export
fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>%
        dplyr::group_by_("year", "MONTH") %>%
        dplyr::summarize_("n" = n()) %>%
        tidyr::spread_("year", "n")
}



#' Create Fatality Analysis Reporting System incident map
#'
#' Draws a map of Fatality Analysis Reporting System (FARS) incidents of a
#' given \code{year} for a state.
#'
#' @inheritParams make_filename
#' @param state.num (\emph{integer}) An identifier specifying the state map to be drawn.
#' (see also the \href{https://crashstats.nhtsa.dot.gov/Api/Public/ViewPublication/812827}{dataset documentation}) \cr
#' \tabular{llll}{
#' \strong{state.num} \tab \strong{State Name} \tab
#' \strong{state.num} \tab \strong{State Name} \cr
#' 1 \tab Alabama \tab 31 \tab Nebraska\cr
#' 2 \tab Alaska \tab 32 \tab Nevada\cr
#' 4 \tab Arizona \tab 33 \tab New Hampshire\cr
#' 5 \tab Arkansas \tab 34 \tab New Jersey\cr
#' 6 \tab California \tab 35 \tab New Mexico\cr
#' 8 \tab Colorado \tab 36 \tab New York\cr
#' 9 \tab Connecticut \tab 37 \tab North Carolina\cr
#' 10 \tab Delaware \tab 38 \tab North Dakota\cr
#' 11 \tab District of Columbia \tab 39 \tab Ohio\cr
#' 12 \tab Florida \tab 40 \tab Oklahoma\cr
#' 13 \tab Georgia \tab 41 \tab Oregon\cr
#' 15 \tab Hawaii \tab 42 \tab Pennsylvania\cr
#' 16 \tab Idaho \tab 43 \tab Puerto Rico\cr
#' 17 \tab Illinois \tab 44 \tab Rhode Island\cr
#' 18 \tab Indiana \tab 45 \tab South Carolina\cr
#' 19 \tab Iowa \tab 46 \tab South Dakota\cr
#' 20 \tab Kansas \tab 47 \tab Tennessee\cr
#' 21 \tab Kentucky \tab 48 \tab Texas\cr
#' 22 \tab Louisiana \tab 49 \tab Utah\cr
#' 23 \tab Maine \tab 50 \tab Vermont\cr
#' 24 \tab Maryland \tab 52 \tab Virgin Islands (since 2004) \cr
#' 25 \tab Massachusetts \tab 51 \tab Virginia\cr
#' 26 \tab Michigan \tab 53 \tab Washington\cr
#' 27 \tab Minnesota \tab 54 \tab West Virginia\cr
#' 28 \tab Mississippi \tab 55 \tab Wisconsin\cr
#' 29 \tab Missouri \tab 56 \tab Wyoming\cr
#' 30 \tab Montana\cr
#' }
#'
#' @details
#' If FARS file for the given \code{year} does not exist in the current
#' working directory or if an undefined \code{state.num} is selected, an error
#' is thrown.
#'
#' @return NULL
#'
#' @references US National Highway Traffic Safety Administration \cr
#' (\href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars})
#'
#' @seealso \code{\link{fars_read}}, \code{\link{make_filename}}
#'
#' @examples
#' \dontrun{
#' # draw map of Alabama for year 2014
#' fars_map_state(1, 2014)
#' # draw map of Nevada for year 2015
#' fars_map_state(32, 2015)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom maps map
#' @importFrom graphics points
#' @export
fars_map_state <- function(state.num, year) {
    filename <- make_filename(year)
    data <- fars_read(filename)
    state.num <- as.integer(state.num)

    if(!(state.num %in% unique(data$STATE)))
        stop("invalid STATE number: ", state.num)
    data.sub <- dplyr::filter(data, .data$STATE == state.num)
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
