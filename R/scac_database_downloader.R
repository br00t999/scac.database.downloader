#' @name download.scac.database
#' @title Download the Most Recent Version of the SCAC Filings Database
#' @description Download the database of securities class action fillings from SCAC
#' as published at: http://securities.stanford.edu/filings.html
#' @param start.page numeric Which page of the filings html database to start scraping from
#' @param debug logical Whether or not to be verbose
#' @param update.cache logical If TRUE we download the entire database and update locally cached copy
#' if FALSE we simply return the locally cached copy
#' @param cache.file character path to .rdata file where most recent copy of the scac db
#' is stored
#' @return returns data.frame 
#' @import httr rvest data.table stringr 
#' @export
download.scac.database <- function(start.page = 1, 
                                   debug = FALSE, 
                                   update.cache = FALSE,
                                   cache.file = file.path(system.file(package = 'scac.database.downloader', 'data'), 'scacdb.rdata')) {
        if (update.cache) {
                base.url <- 'http://securities.stanford.edu/'
                filings.url <- 'http://securities.stanford.edu/filings.html'
                next.page <- start.page
                page <- start.page
                data <- list(1e04) # pre-allocate a big list
                while(!is.na(next.page)) {
                        message('downloading page ', page, ' of scac database')
                        
                        # download page
                        url <- sprintf('%s?page=%s', filings.url, page)
                        if(debug) message(paste0('\nGetting ', url, '\n'))
                        if(debug) pg <- GET(url, config = verbose()) else pg <- GET(url)
                        pg <- content(pg)
                        
                        # extract the list of filings
                        tbl <- html_table(pg)[[ 1 ]]
                        
                        # extract the list of links to cases
                        case.links <- html_nodes(pg, 'tr.table-link')
                        case.links <- html_attrs(case.links)
                        case.links <- lapply(case.links, '[', 'onclick')
                        case.links <- str_extract_all(case.links, 'filings-case.html\\?id=[0-9]+')
                        case.links <- unlist(case.links)
                        case.links <- paste0(base.url, case.links)
                        
                        # download summaries
                        summaries <- lapply(case.links, function(x) {
                                Sys.sleep(2)
                                message('downloading case details ', x)
                                tmp <- read_html(x)
                                smtxt <- html_nodes(tmp, '#summary')
                                smtxt <- html_text(smtxt)
                                smtxt <- str_replace_all(smtxt, '\\s+', ' ')
                                smtxt <- str_trim(smtxt)
                                sector <- html_text(html_nodes(tmp, '#company > div:nth-child(3) > div:nth-child(1)'))
                                industry <- html_text(html_nodes(tmp, '#company > div:nth-child(3) > div:nth-child(2)'))
                                headquarters <- html_text(html_nodes(tmp, '#company > div:nth-child(3) > div:nth-child(3)'))
                                fic <- html_text(html_nodes(tmp, '#fic > div:nth-child(1) > h4:nth-child(2)'))
                                docket <- html_text(html_nodes(tmp, 'div.row-fluid:nth-child(2) > div:nth-child(2)')[ 1 ])
                                judge <- html_text(html_nodes(tmp, 'div.row-fluid:nth-child(2) > div:nth-child(3)')[ 1 ])
                                firms <- paste0(html_text(html_nodes(tmp, '.styled > li')), collapse = '\n')
                                data.frame(CaseSummary = smtxt, 
                                           Sector = sector, 
                                           Industry = industry, 
                                           Headquarters = headquarters,
                                           FirstIdentifiedComplainant = fic,
                                           Judge = judge,
                                           PlaintiffFirms = firms)
                        })
                        tmp <- rbindlist(summaries, use.names = TRUE, fill = TRUE)
                        tmp <- as.data.frame(tmp)
                        tbl <- data.frame(tbl, tmp)
                        
                        # clean up column names
                        tbl <- setNames(tbl, str_replace_all(colnames(tbl), '[^A-Za-z]', ''))
                        tbl$CaseLink <- case.links
                        tbl[] <- lapply(tbl, as.character)
                        data[[ page ]] <- tbl

                        # what page are we on and what is the next one?
                        nav <- html_nodes(pg, 'div.pagination-right > ul > li')
                        idx <- which(lapply(html_attrs(nav), '[', 'class') == 'active')
                        has.next <- lapply(html_attrs(nav), '[', 'class')[ idx + 1 ] != 'disabled'
                        current.page <- html_text(nav[ idx ])
                        next.page <- html_text(nav[ idx + 1 ])
                        
                        # when we reach the end of pages, next.page will be NA
                        next.page <- suppressWarnings(as.numeric(next.page))
                        if (is.na(next.page)) {
                                if (has.next) {
                                        next.page <- as.integer(current.page) + 1        
                                } else {
                                        message('we have reached the end of the scac database')
                                        next.page <- NA        
                                }
                                
                        }
                        page <- next.page
                        Sys.sleep(2) # pause to be polite
                }
                idx <- which(lapply(data, class) == 'data.frame')
                data <- data[ idx ]
                scac.db <- rbindlist(data, use.names = TRUE, fill = TRUE) 
                scac.db <- as.data.frame(scac.db)
                scac.db[] <- lapply(scac.db, as.character)
                scac.db$FilingDate <- as.Date(scac.db$FilingDate, format = '%m/%d/%Y')
                scac.db$FetchDate <- Sys.Date()
                save('scac.db', file = cache.file)        
        } else {
                load(cache.file)
        }
        scac.db
}