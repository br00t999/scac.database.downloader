#' @name download.scac.database
#' @title Download the Most Recent Version of the SCAC Filings Database
#' @description Download the database of securities class action fillings from SCAC
#' as published at: http://securities.stanford.edu/filings.html
#' @param start.page numeric Which page of the filings html database to start scraping from
#' @param debug logical Whether or not to be verbose
#' @param update.cache logical Whether or not to update the package dataset with most 
#' recently downloaded copy of the database
#' @param cache.file character path to .rdata file where most recent copy of the scac db
#' is stored
#' @return returns data.frame 
#' @import httr XML data.table stringr
#' @export
download.scac.database <- function(start.page = 1, 
                                   debug = FALSE, 
                                   update.cache = FALSE,
                                   cache.file = file.path(system.file(package = 'scac.database.downloader', 'data'), 'scac-db-df.RData')) {
        next.page <- page <- start.page
        data <- list(1e04) # pre-allocate a big list
        while(!is.na(next.page)) {
                base.url <- 'http://securities.stanford.edu/'
                url <- 'http://securities.stanford.edu/filings.html'
                url <- modify_url(url, query = list(page = page))
                if(debug) message(paste0('\nGetting ', url, '\n'))
                if(debug) pg <- GET(url, config = verbose()) else pg <- GET(url)
                # extract the list of filings
                tbl <- readHTMLTable(content(pg), stringsAsFactors = FALSE)[[ 1 ]]
                # extract the list of links to cases
                xpath <- '//tr[ @class = "table-link" ]'
                case.links <- paste0(base.url, 
                                     str_replace_all(sapply(lapply(xpathApply(content(pg), 
                                                                              xpath), 
                                                                   xmlAttrs), '[', 
                                                            'onclick'), 
                                                     'window.location=\'|\'', ''))
                # clean up column names
                tbl <- setNames(tbl, str_replace_all(colnames(tbl), '[^A-Za-z]', ''))
                tbl$CaseLink <- case.links
                tbl$FilingDate <- as.Date(tbl$FilingDate, format = '%m/%d/%Y')
                data[[ page ]] <- tbl
                xpath <- '//div[ @class = "pagination pagination-right" ]//ul//li[ @class = "active" ]//a'
                current.page <- str_replace_all(sapply(xpathApply(content(pg), xpath), 
                                                       xmlAttrs), '\\?page=', '')
                xpath <- '//div[ @class = "pagination pagination-right" ]//ul//li//a'
                all.pages <- str_replace_all(sapply(xpathApply(content(pg), xpath), 
                                                    xmlAttrs), '\\?page=', '')
                next.page <- all.pages[ (which(all.pages == current.page) + 1) ]
                # if we reach the last page stop
                if(as.numeric(next.page) == as.numeric(current.page)) next.page <- NA 
                page <- next.page
                Sys.sleep(runif(1, 1, 3)) # pause to be polite
        }
        data <- data[ which(lapply(data, class) == 'data.frame') ]
        scac.db.df <- data.frame(rbindlist(data), stringsAsFactors = FALSE) 
        scac.db.df$DistrictCourt <- factor(scac.db.df$DistrictCourt)
        scac.db.df$Exchange <- factor(scac.db.df$Exchange)
        scac.db.df$Ticker <- factor(scac.db.df$Ticker)
        scac.db.df$FetchDate <- Sys.Date()
        if(update.cache) save(scac.db.df, file = cache.file)
        scac.db.df
}

#'  The Securities Class Action Clearinghouse (SCAC) provides detailed information 
#'  relating to the prosecution, defense, and settlement of federal class action 
#'  securities fraud litigation.
#'  
#'  The SCAC team maintains a Filings database of more than 3,800 securities class 
#'  action lawsuits filed since passage of the Private Securities Litigation Reform 
#'  Act of 1995. The database also contains copies of more than 44,000 complaints, 
#'  briefs, filings, and other litigation-related materials filed in these cases.
#'  
#'  In 2002 Joseph A. Grundfest, founder and Principal Investigator of the SCAC wrote:
#'  
#'      "The Stanford Clearinghouse offers information that is substantially more detailed 
#'      and timely than can be found on other services, which generally limit their 
#'      databases to judicial decisions. By accessing this site, each user can form 
#'      his or her own opinion as to whether litigation is with or without merit, 
#'      whether too few or too many companies are being sued, and whether recoveries 
#'      are too small or too large."
#'      
#'  The SCAC offers regular updates identifying companies that have recently been 
#'  named as defendants in federal class action securities fraud complaints. 
#'  Institutional investors with more than $1.5 trillion in assets under 
#'  management are registered with the SCAC to receive automatic notices of 
#'  litigation developments that may affect investments in their portfolios. 
#'  All members of the public are invited to register to receive prompt notification 
#'  of litigation developments. If you would like to receive this information, 
#'  please register here: \url{http://securities.stanford.edu/about-the-scac.html#register}.
#' 
#' The variables are as follows:
#'
#' \itemize{
#'  \item FilingName
#'  \item FilingDate
#'  \item DistrictCourt
#'  \item Exchange
#'  \item Ticker
#'  \item CaseLink
#' }
#'
#' @format A data.table with 3945 rows and 6 variables
#' @source The Securities Class Action Clearinghouse (SCAC) \url{http://securities.stanford.edu/}
#' @name scac.db.df
NULL