library(testthat)

context('Test SCAC DB downloader')

test_that(desc = 'Test `download.scac.database` function', code = {
        df <- scac.database.downloader::download.scac.database(start.page = 196, 
                                                               debug = TRUE, 
                                                               update.cache = FALSE)
        expect_equal(class(df), 'data.frame')
        expect_equal(colnames(df), c('FilingName', 'FilingDate', 'DistrictCourt', 
                                     'Exchange', 'Ticker', 'CaseLink', 'FetchDate'))
        expect_equal(class(df$FilingDate), 'Date')
})

test_that(desc = 'Test package data set `scac.db.df`', code = {
        scac.db.df <- scac.database.downloader::scac.db.df
        expect_equal(class(scac.db.df), 'data.frame')
        expect_equal(colnames(scac.db.df), c('FilingName', 'FilingDate', 'DistrictCourt', 
                                             'Exchange', 'Ticker', 'CaseLink', 'FetchDate'))
        expect_equal(class(scac.db.df$FilingDate), 'Date')
})