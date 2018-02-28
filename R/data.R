#'  scac.db
#'  
#'  scac.db
#'  
#'  The Securities Class Action Clearinghouse (SCAC)
#'  
#'  The Securities Class Action Clearinghouse (SCAC) provides detailed 
#'  information relating to the prosecution, defense, and settlement of federal 
#'  class action securities fraud litigation. The SCAC team maintains a Filings 
#'  database of securities class action lawsuits filed since passage of the Private 
#'  Securities Litigation Reform Act of 1995. The database also contains copies 
#'  of more than 44,000 complaints, briefs, filings, and other litigation-related materials 
#'  filed in these cases.
#' 
#' @format A data.frame with 3000+ rows and 7 variables
#' \itemize{
#'  \item FilingName: name of the filing
#'  \item FilingDate: filing date
#'  \item DistrictCourt: court 
#'  \item Exchange: exchange
#'  \item Ticker: ticker symbol
#'  \item CaseSummary: summmary
#'  \item Sector: sector
#'  \item Industry: industry
#'  \item Headquarters: headquarters location
#'  \item FirstIdentifiedComplainant: first identified complainant
#'  \item Judge: presiding judge
#'  \item PlaintiffFirms: plaintiff law firms
#'  \item CaseLink: link to full case details
#'  \item FetchDate: date record was last fetched on
#' }
#' @source The Securities Class Action Clearinghouse (SCAC) \url{http://securities.stanford.edu/}
'scac.db'