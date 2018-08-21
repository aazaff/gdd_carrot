#' A function to make GDD NLP output human-readable
#'
#' This function will convert a sentence from the raw GeoDeepDive nlp output tables into a more human-readable format.
#'
#' @param Sentence a record (row) in the GDD NLP output
#' @param Parameters a vector of sentence properties
#'
#' @details This function will convert a sentence from the raw GeoDeepDive nlp output tables into a more human-readable format. It returns a matrix of where each column represents an individual word in the sentence, and each row represents a specific property of the word - e.g., poses, dep_parents, dep_paths. 
#'
#' @return A matrix 
#'
#' @author Andrew A. Zaffos & Erika T. Ito
#'
#' @examples
#'
#' # TBDDDDD
#'
#' @rdname parseSentence
#' @export
# Plots ngram
# Parse the NLP strings into a matrix format
parseSentence<-function(Sentence,Parameters=c("words","dep_paths","dep_parents")) {
        Sentence<-setNames(cleanPunctuation(Sentence),names(Sentence))
        if ("words"%in%names(Sentence)) {Sentence["words"]<-trueCommas(Sentence["words"])}
        WordsMatrix<-sapply(Sentence[Parameters],function(x) strsplit(x,","))
        if (sum(diff(sapply(WordsMatrix,length)))!=0) {return(NA)}
        WordsMatrix<-do.call(rbind,WordsMatrix)
        WordsMatrix[which(WordsMatrix=="COMMASUB")]<-","
        WordsMatrix[which(WordsMatrix=="SPACESUB")]<-""
        colnames(WordsMatrix)<-1:ncol(WordsMatrix)
        return(WordsMatrix)
        }
                            
# R confuses 2,000,381 in a PostgreSQL array as 2 000 381, this function will convert those cases to 2000381.  
trueCommas<-function(Words) {
        InsideQuotes<-regmatches(Words, gregexpr('"[^"]*"',Words))[[1]]
        if (length(InsideQuotes)<1) {return(Words)}
        Replacements<-gsub(",","",InsideQuotes)
        for (i in 1:length(InsideQuotes)) {
                Words<-noquote(gsub(InsideQuotes[i],Replacements[i],Words))
                }
        return(Words)
        }
