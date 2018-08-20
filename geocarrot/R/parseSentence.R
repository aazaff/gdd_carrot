
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
# Parse the NLP strings into a matrix format
parseSentence<-function(Sentence,Parameters=c("words","dep_paths","dep_parents")) {
        Sentence<-setNames(cleanPunctuation(Sentence),names(Sentence))
        if ("words"%in%names(Sentence)) {Sentence["words"]<-trueCommas(Sentence["words"])}
        WordsMatrix<-sapply(Sentence[Parameters],function(x) strsplit(x,","))
        WordsMatrix<-do.call(rbind,WordsMatrix)
        WordsMatrix[which(WordsMatrix=="COMMASUB")]<-","
        WordsMatrix[which(WordsMatrix=="SPACESUB")]<-""
        colnames(WordsMatrix)<-1:ncol(WordsMatrix)
        return(WordsMatrix)
        }
