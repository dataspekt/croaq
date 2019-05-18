#' @aliases croaq-package
#' @details This package simplifies access to air quality data from state and local networks of measurement stations provided by Croatian Ministry of Environmental Protection and Energy. Main function of this package which is used to fetch data is `croaq()`. Meta-data on stations, pollutants and datatypes is provided by functions `listStations()`, `listPollutants()` and `listDatatypes()`.
"_PACKAGE"

#' Fetch air quality data
#'
#' Fetch air quality data from state and local networks of measurement stations in Croatia.
#'
#' @param station integer, station code.
#' @param item integer or character, pollutant codes or labels.
#' @param from,to character, start and end date.
#' @param type integer, value datatype.
#' @param labels character, custom item labels.
#' @param fill logical, if \code{TRUE} (default) fill data within time range with missing values where no data is available.
#'
#' @details Fetches values of one or more \code{item}s (pollutants, etc.) measured at a single \code{station} within time period specified by \code{from} and \code{to}.
#'
#' For a list of stations and their codes, run \code{listStations()}.
#' 
#' Items can be specified using integer codes or (in some cases) short labels, run \code{listPollutants()} for a list. Custom labels can be set by \code{labels} argument which should be a character vector of the same length as \code{item} and have unique values.
#' 
#' There are various data\code{types} available (raw, validated, hourly, daily, etc.). Run \code{listDatatypes()} for a list.
#'
#' Dates should be specified as character values in `Y-m-d` format. If \code{fill=TRUE} (default), time points with missing values will be added to data where no data is available. If \code{fill = FALSE}, missing values are not accounted for in summary and plot methods.
#'
#' @note Availability of data depends on selected station, time period and datatype. A note is printed if requested data for some \code{item}s is not available and if no data at all is available \code{NULL} is returned.
#' 
#' @return Object of class \code{croaq} inheriting from a data frame class with dedicated summary and plot methods.
#'
#' @seealso \code{\link{wide}}
#' 
#' @references Data exchange service at \url{http://iszz.azo.hr/iskzl/exc.htm}.
#' 
#' @export
croaq <- function(station, item, from, to, type = 0, labels = NULL, fill = TRUE){
    # Set time range
    time.range <- set_time_range(from, to)
    ftime <- format(time.range,"%d.%m.%Y")
    
    # Check station code
    if(!station %in% stations$code)
        stop("Unknown station code.")
    
    # Check datatype
    if(!type %in% datatypes$code)
        stop("Unknown datatype code.")
    
    # Check and label items
    ic <- set_items(item, labels)
    
    # Fetch data
    lst <- lapply(ic,function(i){
        url <- paste(
            "http://iszz.azo.hr/iskzl/rs/podatak/export/json?",
            "postaja=", station, "&",
            "polutant=", i, "&",
            "tipPodatka=", type, "&",
            "vrijemeOd=", ftime[1], "&",
            "vrijemeDo=", ftime[2],
            sep="")
        js <- jsonlite::fromJSON(url)
        if(is.data.frame(js))
            data.frame(station = as.integer(station),
                 code = as.integer(i),
                 label = names(ic)[ic == i],
                 unit = as.character(unique(js$mjernaJedinica)),
                 type = as.integer(type),
                 time = as.POSIXct(js$vrijeme/1000, origin = "1970-01-01"),
                 value = as.numeric(js$vrijednost)
                 ) else NULL
    })
    
    # Check for empty or partial data
    no.data <- sapply(lst,is.null)
    nd <- ifelse(all(no.data), "empty",
          ifelse(any(no.data), "partial","full"))
    switch(nd,
           empty = {
               message("No data, returning NULL.")
               return(NULL)
           },
           partial = {
               message(paste("No data for", paste(names(ic)[no.data], collapse = ", ")))
           })
    lst <- lst[!sapply(lst,is.null)]

    # Merge, fill and sort data
    data <- Reduce(function(...) merge(...,all=TRUE), lst)
    if(fill)
        data <- fill_time_range(data, time.range, type)
#    data <- data[order(data$code, data$time),]

    # Return object of class 'croaq'
    structure(data, class = c("croaq", "data.frame"))
}

#' @describeIn croaq Summary of \code{croaq} object
#' 
#' @param object \code{croaq} object.
#'
#' @return Summary method invisibly returns total and missing values for each item.
#' 
#' @export
summary.croaq <- function(object, ...)
{
    tr <- format(range(object$time),format="%Y-%m-%d %H:%M:%S", usetz=TRUE)
    cat(paste("Station:", unique(object$station)), "\n")
    cat(paste("Items:", paste(unique(object$label),collapse=", ")), "\n")
    cat(paste("From:", tr[1]), "\n")
    cat(paste("To:", tr[2]), "\n")
    cat("Values:\n")
    x <- split(object, object$label)
    df <- lapply(x, function(x) {
        data.frame(total = length(x$value),
                   missing = sum(is.na(x$value))
                   )
    })
    tbl <- do.call("rbind", df)
    print(tbl)
    invisible(tbl)
}

#' @describeIn croaq Plot \code{croaq} object.
#'
#' @return Plot method shows time on x-axis and values on y-axis. Time points with missing values are shown as vertical red lines.
#'
#' @param x \code{croaq} object.
#' @param ... further parameters.
#'
#' @export
plot.croaq <- function(x, ...)
{
    lst <- split(x, x$label)
    graphics::par(mfrow = grDevices::n2mfrow(length(lst)))
    invisible(lapply(lst, function(x){
        with(x, plot(value ~ time,
                     main = unique(x$label),
                     ylab = unique(x$unit),
                     ...))
        graphics::abline(v = x$time[is.na(x$value)], col = "red")
    }))
}


#' Check and set time range
#' @param from,to character.
#' @keywords internal
set_time_range <- function(from, to)
{
    tr <- strptime(c(from = from, to = to), format = "%Y-%m-%d")
    if(any(is.na(tr)))
        stop("Invalid date specification.")
    if(tr[1] > tr[2])
        stop("Start date should be <= end date.")
    as.POSIXct(tr)
}

#' Check and set item specification.
#' @param item integer or character.
#' @param labels character.
#' @keywords internal
set_items <- function(item, labels)
{
    # convert labels to codes
    ic <- switch(
        class(item),
        character = char2ic(item),
        numeric = num2ic(item),
        integer = num2ic(item),
        stop("Argument 'item' should be numeric, integer or character.")
    )
    
    # check for missing codes
    if(any(is.na(ic)))
        stop(paste("Unknown item code/label:", paste(item[is.na(ic)], collapse=", ")))
    
    # add custom labels
    if(is.character(labels)){
        if(length(labels) != length(ic))
            stop("Arguments 'labels' and 'items' should be of same length.")
        if(any(duplicated(labels)))
            stop("Arguments 'labels' must have unique values.")
        names(ic) <- labels
    }
    
    return(ic)
}

#' Convert character input to labeled item codes
#' @param i character vector of item labels.
#' @keywords internal
char2ic <- function(i)
{
    ic <- sapply(i, function(x)
    {
        code <- pollutants$code[is.element(pollutants$label,x)]
        if(length(code)==0) code <- NA
        return(code)
    })
    return(ic)
}

#' Convert numeric/integer input to labeled item codes
#' @param i numeric/integer vector of item codes.
#' @keywords internal
num2ic <- function(i)
{
    ic <- i
    icname <- sapply(ic, function(x){
        lb <- pollutants$label[pollutants$code == x]
        if(length(lb) == 0) lb <- paste0("item", x)
        return(lb)
    })
    ic <- ifelse(ic %in% pollutants$code, ic, NA)
    names(ic) <- icname
    return(ic)
}

#' Fill time range with missing values
#' @param data data frame.
#' @param time.range vector of length 2 in POSIXct format.
#' @param type integer, datatype.
#' @keywords internal
fill_time_range <- function(data, time.range, type)
{
    # cycle depends on type!
    cycle <- switch(datatypes$cycle[datatypes$code == type],
                    hourly = "hour",
                    daily = "day",
                    yearly = "year"
                    )

    # offsets
    offset1 <- switch(cycle,
                      hour = 60*60,
                      0)
    offset2 <- switch(cycle,
                      hour = 60*60*24,
                      0)

    # full time
    ftime <- seq(time.range[1] + offset1,
                 time.range[2] + offset2,
                 by = cycle)
    
    # merge data & ftime
    data <- do.call("rbind", lapply(split(data, data$code), function(x){
        fdf <- data.frame(station = unique(x$station),
                          code = unique(x$code),
                          label = unique(x$label),
                          unit = unique(x$unit),
                          type = unique(x$type),
                          time = ftime)
        merge(x, fdf, all=TRUE)
    }))
    
    return(data)
}


#' Reshape data into wide format
#'
#' Sometimes it is handy to work with data in wide format.
#' 
#' @param data object of class \code{croaqData}.
#' @param idvar see \code{\link[stats]{reshape}}.
#' @param timevar see \code{\link[stats]{reshape}}.
#' 
#' @export
wide <- function(data, idvar = c("station", "time", "type"), timevar = "label")
{
    stats::reshape(data, idvar = idvar, timevar = timevar, direction="wide")
}

#' List stations
#' @return A data frame with stations listing.
#' @references \url{http://iszz.azo.hr/iskzl/doc/servis_uputa.docx}
#' @export
listStations <- function(){ stations }

#' List items (pollutants)
#' @return A data frame with items (pollutants) listing.
#' @references \url{http://iszz.azo.hr/iskzl/doc/servis_uputa.docx}
#' @export
listPollutants <- function(){ pollutants }

#' List datatypes
#' @return A data frame with datatypes listing.
#' @export
listDatatypes <- function(){ datatypes }
