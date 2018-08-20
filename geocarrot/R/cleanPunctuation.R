
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
# Remove or replace problematic punctuation
# Even though this is redundnat with trueCommas it applies to more fields
cleanPunctuation<-function(Sentence) {
        Sentence<-gsub("\"\"","SPACESUB",Sentence)
        Sentence<-gsub("\",\"","COMMASUB",Sentence) 
        Sentence<-gsub("\\{|\\}","",Sentence)
        Sentence<-gsub("-LRB-","(",Sentence)
        Sentence<-gsub("-RRB-",")",Sentence)
        Sentence<-gsub("-LCB-","{",Sentence)
        Sentence<-gsub("-RCB-","}",Sentence)
        return(Sentence)
        }
