

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
trueCommas<-function(Words) {
        InsideQuotes<-regmatches(Words, gregexpr('"[^"]*"',Words))[[1]]
        if (length(InsideQuotes)<1) {return(Words)}
        Replacements<-gsub(",","",InsideQuotes)
        for (i in 1:length(InsideQuotes)) {
                Words<-noquote(gsub(InsideQuotes[i],Replacements[i],Words))
                }
        return(Words)
        }
