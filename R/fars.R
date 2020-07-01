utils::globalVariables(c("MONTH","STATE","year"))
#' Read a .csv file.
#' @description Reads a .csv file and turns it into a tibble
#' @param filename File location or name if in working directory
#' @return Dataset in Tibble
#' @examples
#' \dontrun{
#' fars_read("R/accident_2013.csv.bz2")
#' fars_read("./accident_2013.csv.bz2")
#' }
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}
#' Prints the name of a file
#' @description Prints the name of a file from the combination of a character string and value of interest
#' @param year year that complements the character string
#' @return Chraracter with combination
#' @examples
#' \dontrun{
#' make_filename(2013)
#' }
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}
#' Extracts the month and year
#' @description Extracts the month and year from a data set
#' @param years reference year
#' @return Dataset in Tibble form.
#' @examples
#' \dontrun{
#' fars_read_years(2015)
#' }
#' @importFrom dplyr mutate select
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
#' Extract the number of vehicle collision injuries per month
#' @description Extract the number of vehicle collision injuries per month with reference year
#' @param years reference year
#' @return Dataset in Tibble form.
#' @examples
#' \dontrun{
#' fars_summarize_years(2013)
#' }
#' @importFrom dplyr bind_rows group_by summarize %>% n
#' @importFrom tidyr spread
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Locations of traffic accidents in the USA
#' @description Maps accident locations in each state by year
#' @param year reference year
#' @param state.num state identification
#' @return Dataset in Tibble form.
#' @examples
#' \dontrun{
#' fars_map_state(12,2013)
#' }
#' @importFrom dplyr filter %>%
#' @importFrom maps map
#' @importFrom graphics points
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
