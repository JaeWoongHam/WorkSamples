#' Obtain consumer complaint information about a specific company
#'
#' It displays various useful summaries of the number of consumer complaints pertaining to different categories of interest for a single company, and puts them in dataframe(s).
#'
#' The function will first prompt the user to enter the desired company as an input for its first parameter. The input will be used to then guess possible suggestions related to that company name. This is necessary as the company name has to be inputted exactly the same as what is recorded in the consumer complaint database. Once the user finds the right suggestion, the user is prompted again to input the correct name of the company. Once that is complete, it will display any of the four possible summarization options. The user can specify whether to include any one or all of the summarization options by specifying (\code{TRUE}) or (\code{FALSE}).
#'
#' @param product_option A logical indicating whether to aggregate all the consumer complaints by product category under the selected company. If (\code{TRUE}), such information will be created in the form of a dataframe as the output. Defaults to (\code{TRUE})
#' @param issue_option A logical indicating whether to aggregate all the consumer complaints by issue category under the selected company. If (\code{TRUE}), such information will be created in the form of a dataframe as the output. Defaults to (\code{TRUE})
#' @param response_option A logical indicating whether to aggregate all the consumer complaints by company response category under the selected company. If (\code{TRUE}), such information will be created in the form of a dataframe as the output. Defaults to (\code{TRUE})
#' @param public_option A logical indicating whether to aggregate all the consumer complaints by public company response category under the selected company. If (\code{TRUE}), such information will be created in the form of a dataframe as the output. Defaults to (\code{FALSE}), as not all companies have a public response
#' @return A list of the dataframe(s) of the options selected. Multiple dataframes are presented as a list because a function can only return one output. As such, by feeding multiple dataframes through a list, multiple outputs can be achieved.
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
#' #No examples can be demonstrated, as it requires a manual user input from a prompt

company_function <- function(product_option = TRUE,issue_option=TRUE,response_option=TRUE,public_option=FALSE){
  finaloutput <- c()
  question <- readline(prompt="Enter the company name: ")
  endpoint1 <- "https://www.consumerfinance.gov/data-research/consumer-complaints/search/api/v1/_suggest_company/"
  query_params1 <- list("text"=question)
  parameter_response1 <- GET(endpoint1,query = query_params1)
  selections = content(parameter_response1)
  s <- c(1:length(content(parameter_response1)))
  for (i in s){
    cat(paste("Did you mean: ",selections[[i]],"?\n"))}
  company_name<- readline(prompt="Reenter the company name: ")
  choice <- company_name

  if (product_option==TRUE){
    endpoint2 <- "https://www.consumerfinance.gov/data-research/consumer-complaints/search/api/v1/"
    query_params2 <- list("company"=choice)
    parameter_response <- GET(endpoint2,query = query_params2)
    TableInitiator1 <- c()
    n <- c(1:length(content(parameter_response)$aggregations$product$product$buckets))
    for (i in n) {
      if(!is.null(content(parameter_response)$aggregations$product$product$buckets[[i]])){
        Product <- content(parameter_response)$aggregations$product$product$buckets[[i]]$key
        Count <- content(parameter_response)$aggregations$product$product$buckets[[i]]$doc_count
        TableInitiator1 <- rbind(TableInitiator1,c(Product,Count))}
    }

    # Finalizing the end output table
    final_df1<-as.data.frame(TableInitiator1)
    names(final_df1)[1] <- "Product"
    names(final_df1)[2] <- "Count of Complaints"
    finaloutput <- append(finaloutput,list(final_df1))
  }

  # SECOND OPTION
  if (issue_option==TRUE){
    TableInitiator2 <- c()
    n <- c(1:length(content(parameter_response)$aggregations$issue$issue$buckets))
    for (i in n) {
      if(!is.null(content(parameter_response)$aggregations$issue$issue$buckets[[i]])){
        Issue <- content(parameter_response)$aggregations$issue$issue$buckets[[i]]$key
        Count <- content(parameter_response)$aggregations$issue$issue$buckets[[i]]$doc_count
        TableInitiator2 <- rbind(TableInitiator2,c(Issue,Count))}
    }

    final_df2<-as.data.frame(TableInitiator2)
    names(final_df2)[1] <- "Issue"
    names(final_df2)[2] <- "Count of Complaints"
    finaloutput <- append(finaloutput,list(final_df2))
  }

  if (response_option==TRUE){
    TableInitiator3 <- c()
    n <- c(1:length(content(parameter_response)$aggregations$company_response$company_response$buckets))
    for (i in n) {
      if(!is.null(content(parameter_response)$aggregations$company_response$company_response$buckets[[i]])){
        Response <- content(parameter_response)$aggregations$company_response$company_response$buckets[[i]]$key
        Count <- content(parameter_response)$aggregations$company_response$company_response$buckets[[i]]$doc_count
        TableInitiator3 <- rbind(TableInitiator3,c(Response,Count))}
    }

    final_df3<-as.data.frame(TableInitiator3)
    names(final_df3)[1] <- "Company Response"
    names(final_df3)[2] <- "Count of Complaints"
    finaloutput <- append(finaloutput,list(final_df3))
  }

  if (public_option == TRUE){
    TableInitiator4 <- c()
    if(length(content(parameter_response)$aggregations$company_public_response$company_response$buckets) != 0){
      n <- c(1:length(content(parameter_response)$aggregations$company_public_response$company_response$buckets))}
    else(stop("No public response available for ",choice,". Please disable this option, and try again."))
    for (i in n) {
      if(!is.null(content(parameter_response)$aggregations$company_response$company_response$buckets[[i]])){
        Response <- content(parameter_response)$aggregations$company_response$company_response$buckets[[i]]$key
        Count <- content(parameter_response)$aggregations$company_response$company_response$buckets[[i]]$doc_count
        TableInitiator4 <- rbind(TableInitiator4,c(Response,Count))}
    }

    final_df4<-as.data.frame(TableInitiator4)
    names(final_df4)[1] <- "Public Company Response"
    names(final_df4)[2] <- "Count of Complaints"
    finaloutput <- append(finaloutput,list(final_df4))
  }

  return(finaloutput)
}
