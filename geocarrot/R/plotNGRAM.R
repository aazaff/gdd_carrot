#' Plot Article Age Data
#'
#' Plots the age distribution of articles using certain terms of interest (i.e. ngram).
#'
#' @param Term a character string
#' @param Publisher a character vector
#' @param Journal a character vector
#'
#' @details This function plots the age distribution of articles from the GeoDeepDive digital library that contain a certain term of interest. This
#' data comes from the \href{https://geodeepdive.org/api/articles}{GeoDeepDive /articles} API route.
#'
#' @return A time series plot
#'
#' @import RJSONIO
#'
#' @author Andrew A. Zaffos & Erika T. Ito
#'
#' @examples
#'
#' # TBDDDDD
#'
#' @rdname plotNGRAM
#' @export
# Plots ngram
plotNGRAM<-function(Term, Publisher="", Journal=""){
    Journal<-gsub(" ", "%20", Journal)
    Term<-gsub(" ", "%20", Term)
    Publisher<-gsub(" ","%20",Publisher)
    URL<-paste0("https://geodeepdive.org/api/articles?pubname=",Journal,"&term=",Term,"&publisher=",Publisher)
    JSON<-RJSONIO::fromJSON(URL)
    }
