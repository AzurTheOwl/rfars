#'Read FARS data file
#'
#'This is a function to open file with specified \code{filename} argument.
#'In case of wrong path it will show error "file \code{filename} does not exist.
#'
#'@param filename A character string that specify file to read
#'
#'@return This function returns file content in "tbl_df" format
#'
#'@examples
#'fars_read("data/file.txt")
#'
#'@importFrom dplyr tbl_df
#'@importFrom readr read_csv
#'
#'@export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#'Generate filename
#'
#'This is the function to generate filename depending on \code{year}.
#'
#'@param year year required
#'
#'@return Returns character vector contains filenume for specified \code{year}
#'
#'@examples
#'make_filename(2015)
#'
#'@export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#'Gain month/year combinations for FARS data read
#'
#'This is the function to read FARS files for specified \code{years}
#'and get a list of month/year data frames when succeed. In case of incorrect year, function will throw warning "invalid year: \code{year}".
#'
#'@param years years required
#'
#'@return This function returns a list of data frames with month/year combination 
#'
#'@examples
#'fars_read_years(2013:2015)
#'fars_read_years(c(2013, 2015))
#'
#'@importFrom dplyr mutate
#'@importFrom dplyr select
#'@importFrom tidyr %>%
#'
#'@export
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

#'Summarise FARS data according to observations number
#'
#'This is the function to get all month/year pairs for specified \code{years} range
#'and then summarise according to them.
#'
#'@param years years required
#'
#'@return This function returns data frame with table of month/year pairs quantity for FARS data
#'
#'@examples
#'fars_summarize_years(2013:2015)
#'fars_summarize_years(c(2013, 2015))
#'
#'@importFrom dplyr bind_rows
#'@importFrom dplyr group_by
#'@importFrom dplyr summarize
#'@importFrom tidyr spread
#'@importFrom tidyr %>%
#'
#'@export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#'Plot fatal injures locations on state map
#'
#'This is a function for plotting fatal accidents on state map according to FARS data with specified \code{state.num}
#'and \code{year}. In case of wrong \code{state.num} function will throw "invalid STATE number" error.
#'
#'@param state.num integer with state number required
#'@param year year required
#'
#'@return This function returns state map with plotted points of fatal accidents or show message that there is nothing to be plotted
#'
#'@examples
#'fars_map_state(10, 2015)
#'
#'@importFrom maps map
#'@importFrom graphics points
#'
#'@export
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
