
#' Find clusters of consecutive proper nouns
#'
#' Returns tuples of GeoDeepDive docid, sentid, and sets of consecutive proper nouns.
#'
#' @param Sentence a GeoDeepDive output nlp output record
#'
#' @details This function will find groupings of proper nouns within a sentence using the poses output of the StanfordCoreNLP. The format is a matrix of tuples of GeoDeepDive docid, sentid, and the proper nouns. This function is designed to help find the names of organizations, people, and places.
#'
#' @return A character matrix
#'
#' @author Andrew A. Zaffos & Erika T. Ito
#'
#' @examples
#'
#' # TBDDDDD
#'
#' @rdname findCluster
#' @export
# A function to find proper noun clusters
findCluster<-function(Sentence,Parameters=c("words","poses")) {
        ParsedSentence<-parseSentence(Sentence,Parameters)
        if(all(is.na(ParsedSentence))) {return(setNames(rep(NA,3), c("docid", "sentid", "Proper")))}
        FindConsecutive<-findConsecutive(which(ParsedSentence["poses",]=="NNP"))
        Proper<-sapply(FindConsecutive,function(x) paste(unname(ParsedSentence["words",x]),collapse=" "))
        Proper<-unname(cbind(Sentence["docid"],Sentence["sentid"],Proper))
        colnames(Proper)<-c("docid","sentid","Proper") 
        return(Proper)
        }

# Sees if numbers are consecutive
findConsecutive<-function(NumericSequence) {
        Breaks<-c(0,which(diff(NumericSequence)!=1),length(NumericSequence))
        ConsecutiveList<-lapply(seq(length(Breaks)-1),function(x) NumericSequence[(Breaks[x]+1):Breaks[x+1]])
        return(ConsecutiveList)
        }
