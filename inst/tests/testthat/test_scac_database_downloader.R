library(testthat)

context('Test SCAC DB downloader')

test_that(desc = 'Test the SCAC DB downloader function', code = {
        expect_false(TRUE)
})

test_that(desc = 'Test package data set scac.db.df', code = {
        scac.db.df <- scac.database.downloader::scac.db.df
        expect_equal(class(scac.db.df), 'data.frame')
        expect_equal(colnames(scac.db.df), c('FilingName', 'FilingDate', 'DistrictCourt', 
                                             'Exchange', 'Ticker', 'CaseLink', ''))
        expect_equal(class(scac.db.df$FilingDate), 'Date')
                     
})