#' Obtain consumer complaint information about a specific product
#'
#' It displays various useful summaries of the number of consumer complaints pertaining to different categories of interest for a single product, and puts them in dataframe(s).
#'
#' The function will first try to assess the input of the user, and see if it partially matches any of the prescribed product categories. This is necessary as the product name has to be inputted exactly the same as what is recorded in the consumer complaint database. If the input is accepted, it will display any of the two possible summarization options. If the input is not accept, the user needs to put in the right product name from the list of suggestions. The user can specify whether to include any one or all of the summarization options by specifying (\code{TRUE}) or (\code{FALSE}).
#'
#' @param product A character input where the user inputs the product category. If the input does not match any of the prescribed product categories on their database, the fuction is stopped. Please note that the user input must always start with a capital letter.
#' @param subproduct_option A logical indicating whether to aggregate all the consumer complaints by sub-product category under the selected product. If (\code{TRUE}), such information will be created in the form of a dataframe as the output. Defaults to (\code{TRUE})
#' @param state_option A logical indicating whether to aggregate all the consumer complaints by state category under the selected product. If (\code{TRUE}), such information will be created in the form of a dataframe as the output. Defaults to (\code{TRUE})
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
#' product_function()

product_function <- function(product="Debt collection",subproduct_option = TRUE,state_option=TRUE){
  correctproducts <- c("Student loan","Vehicle loan or lease","Payday loan, title loan, or personal loan","Mortage","Credit card or prepaid card","Checking or savings account","Money transfer, virtual currency, or money service","Debt collection","Credit reporting, credit repair services, or other personal consumer reports")
  for (i in c(1:9)) {
    ifelse(length(str_subset(correctproducts[i],paste("\\b",product,sep = "")))>0,s<-str_subset(correctproducts[i],paste("\\b",product,sep = "")),"")
  }
  if (product != s){
    stop("Your input from ",product," is not valid. Please choose from the following: ",s)
  }
  endpoint <- "https://www.consumerfinance.gov/data-research/consumer-complaints/search/api/v1/"
  query_params <- list("product"=product)
  parameter_response <- GET(endpoint,query = query_params)
  finaloutput <- list()

  if (subproduct_option==TRUE){
    TableInitiator1 <- c()
    n <- c(1:length(content(parameter_response)$aggregations$product$product$buckets))
    for (i in n) {
      if(content(parameter_response)$aggregations$product$product$buckets[[i]]$key==product){
        d <- as.numeric(i)}
    }
    l <- c(1:length(content(parameter_response)$aggregations$product$product$buckets[[d]]$sub_product.raw$buckets))
    for (i in l) {
      if(!is.null(content(parameter_response)$aggregations$product$product$buckets[[d]]$sub_product.raw$buckets[[i]])){
        Subproduct <- content(parameter_response)$aggregations$product$product$buckets[[d]]$sub_product.raw$buckets[[i]]$key
        Count <- content(parameter_response)$aggregations$product$product$buckets[[d]]$sub_product.raw$buckets[[i]]$doc_count
        TableInitiator1 <- rbind(TableInitiator1,c(Subproduct,Count))}
    }

    final_df1<-as.data.frame(TableInitiator1)
    names(final_df1)[1] <- "Sub-product"
    names(final_df1)[2] <- "Count of Complaints"
    finaloutput <- append(finaloutput,list(final_df1))
  }

  if (state_option==TRUE){
    TableInitiator2 <- c()
    n <- c(1:length(content(parameter_response)$aggregations$state$state$buckets))
    for (i in n) {
      if(!is.null(content(parameter_response)$aggregations$state$state$buckets[[i]])){
        State <- content(parameter_response)$aggregations$state$state$buckets[[i]]$key
        Count <- content(parameter_response)$aggregations$state$state$buckets[[i]]$doc_count
        TableInitiator2 <- rbind(TableInitiator2,c(State,Count))}
    }

    final_df2<-as.data.frame(TableInitiator2)
    names(final_df2)[1] <- "State"
    names(final_df2)[2] <- "Count of Complaints"
    finaloutput <- append(finaloutput,list(final_df2))
  }
  return(finaloutput)
}
