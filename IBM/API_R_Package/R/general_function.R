#' Obtain consumer complaint information in a flexible search
#'
#' It displays all associated datapoint(s) depending on what was specified by the user. The user has varying options to specify what kind of complaint the user wants. The output is a single dataframe that contains all the relevant datapoints.
#'
#' The function will take the input parameters from the user, and the input paramters need to be correctly matched with how it is recorded in the database, unlike other functions in this package.
#'
#' @param company A character input from the user to specify a company name. Defaults to Bank of America.
#' @param product A character input from the user to specify a product category. Defaults to checking or savings account category.
#' @param issue A character input from the user to specify an issue category. Defaults to managing an account.
#' @param limit A integer input from the user to limit the number of results. Defaults to 10.
#' @return a dataframe of the chosen parameters
#' @author Jae Woong Ham
#' @import httr
#' @import rjson
#' @import ggplot2
#' @import maps
#' @import sp
#' @import maptools
#' @import stringr
#' @import magrittr
#' @import rvest
#' @import dplyr
#' @export
#' @examples
#' general_function()

general_function <- function(company = "BANK OF AMERICA, NATIONAL ASSOCIATION",product = "Checking or savings account",issue="Managing an account",limit=10){
  endpoint <- "https://www.consumerfinance.gov/data-research/consumer-complaints/search/api/v1/"
  query_params <- list("company"=company,"product"=product,"issue"=issue,"size"=limit)
  parameter_response <- GET(endpoint,query = query_params)
  finaloutput <- list()
  TableInitiator1 <- c()
  n <- c(1:length(content(parameter_response)$hits$hits))
  for (i in n) {
    ifelse(!is.null(content(parameter_response)$hits$hits[[i]]$`_id`),ComplaintID <- content(parameter_response)$hits$hits[[i]]$`_id`,ComplaintID <- "NA")
    ifelse(!is.null(content(parameter_response)$hits$hits[[i]]$`_source`$company),Company <- content(parameter_response)$hits$hits[[i]]$`_source`$company,Company <- "NA")
    ifelse(!is.null(content(parameter_response)$hits$hits[[i]]$`_source`$product),Product <- content(parameter_response)$hits$hits[[i]]$`_source`$product,Product <- "NA")
    ifelse(!is.null(content(parameter_response)$hits$hits[[i]]$`_source`$sub_product),SubProduct <- content(parameter_response)$hits$hits[[i]]$`_source`$sub_product,SubProduct <- "NA")
    ifelse(!is.null(content(parameter_response)$hits$hits[[i]]$`_source`$issue),Issue <- content(parameter_response)$hits$hits[[i]]$`_source`$issue,Issue <- "NA")
    ifelse(!is.null(content(parameter_response)$hits$hits[[i]]$`_source`$sub_issue),SubIssue <- content(parameter_response)$hits$hits[[i]]$`_source`$sub_issue,SubIssue <- "NA")
    ifelse(!is.null(content(parameter_response)$hits$hits[[i]]$`_source`$date_sent_to_company),DateSent <- content(parameter_response)$hits$hits[[i]]$`_source`$date_sent_to_company,DateSent <- "NA")
    ifelse(!is.null(content(parameter_response)$hits$hits[[i]]$`_source`$consumer_consent_provided),ConsumerConsent <- content(parameter_response)$hits$hits[[i]]$`_source`$consumer_consent_provided,ConsumerConsent <- "NA")
    ifelse(!is.null(content(parameter_response)$hits$hits[[i]]$`_source`$date_received),DateReceived <- content(parameter_response)$hits$hits[[i]]$`_source`$date_received,DateReceived<- "NA")
    ifelse(!is.null(content(parameter_response)$hits$hits[[i]]$`_source`$zip_code),ZipCode <- content(parameter_response)$hits$hits[[i]]$`_source`$zip_code,ZipCode<- "NA")
    ifelse(!is.null(content(parameter_response)$hits$hits[[i]]$`_source`$state),State <- content(parameter_response)$hits$hits[[i]]$`_source`$state,State<- "NA")
    ifelse(!is.null(content(parameter_response)$hits$hits[[i]]$`_source`$company_public_response),PublicResponse <- content(parameter_response)$hits$hits[[i]]$`_source`$company_public_response,PublicResponse <- "NA")
    ifelse(!is.null(content(parameter_response)$hits$hits[[i]]$`_source`$company_response),CompanyResponse <- content(parameter_response)$hits$hits[[i]]$`_source`$company_response,CompanyResponse <- "NA")
    ifelse(!is.null(content(parameter_response)$hits$hits[[i]]$`_source`$timely),Timely <- content(parameter_response)$hits$hits[[i]]$`_source`$timely,Timely <- "NA")
    ifelse(!is.null(content(parameter_response)$hits$hits[[i]]$`_source`$consumer_disputed),ConsumerDisputed <- content(parameter_response)$hits$hits[[i]]$`_source`$consumer_disputed,ConsumerDisputed <- "NA")
    TableInitiator1 <- rbind(TableInitiator1,c(ComplaintID,Company,Product,SubProduct,Issue,SubIssue,DateSent,ConsumerConsent,DateReceived,State,ZipCode,PublicResponse,CompanyResponse,Timely,ConsumerDisputed))
  }

  final_df<-as.data.frame(TableInitiator1)
  names(final_df)[1] <- "Complaint ID"
  names(final_df)[2] <- "Associated Company"
  names(final_df)[3] <- "Product"
  names(final_df)[4] <- "Sub-Product"
  names(final_df)[5] <- "Issue"
  names(final_df)[6] <- "Sub-Issue"
  names(final_df)[7] <- "Date Sent"
  names(final_df)[8] <- "Consumer Consent"
  names(final_df)[9] <- "Date Received"
  names(final_df)[10] <- "State"
  names(final_df)[11] <- "Zipcode"
  names(final_df)[12] <- "Public Company Response"
  names(final_df)[13] <- "Company Response"
  names(final_df)[14] <- "Timely"
  names(final_df)[15] <- "Consumer Disputed"

  final_df$`Date Sent` <- str_sub(final_df$`Date Sent`,1,10)
  final_df$`Date Received` <- str_sub(final_df$`Date Received`,1,10)
  return(final_df)
}
