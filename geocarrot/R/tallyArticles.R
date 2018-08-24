#' Summarize Article Statistics
#'
#' Summarizes the number of articles per journal for a publisher in the GeoDeepDive library.
#'
#' @param Publisher a character vector
#'
#' @details This function allows you to determine the number of documents for each journal associated with a
#' publisher. This data is pulled from the \href{https://geodeepdive.org/api/journals}{GeoDeepDive /journals} API route.
#'
#' @return A data frame with journal titles as row names, showing the number of articles for each journal within a publisher.
#'
#' @import RJSONIO
#'
#' @author Andrew A. Zaffos & Erika T. Ito
#'
#' @examples
#' # Determine the number of articles for each journal within the Elsevier publishing company
#'
#' @rdname tallyArticles
#' @export
# Downloads article bibjson metadata from /articles route of the GeoDeepDive API.
tallyArticles<-function(Publisher) {
    URL<-paste0("https://geodeepdive.org/api/journals?publisher=",Publisher)
    Output<-RJSONIO::fromJSON(URL)
    Articles<-subset(unlist(Output),names(unlist(Output))=="success.data.articles")
    Journals<-subset(unlist(Output),names(unlist(Output))=="success.data.journal")
    Lookup<-as.data.frame(cbind(unname(Articles),unname(Journals)),stringsAsFactors=FALSE)
    Lookup[,"V1"]<-as.numeric(Lookup[,"V1"])
    # Remove "blank" as a journal option
    Lookup<-subset(Lookup,nchar(Lookup[,2])>0)
    Lookup<-Lookup[order(Lookup[,"V1"],decreasing=TRUE),]
    Lookup<-transform(Lookup,row.names=V2,V2=NULL)
    colnames(Lookup)<-"Number of Articles"
    return(Lookup)
    }
