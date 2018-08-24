
#' Find clusters of consecutive posees
#'
#' Returns tuples of GeoDeepDive docid, sentid, and sets of consecutive parts of speech (i.e., poses).
#'
#' @param Sentence a GeoDeepDive output nlp output record
#' @param Pose a string
#'
#' @details This function will find groupings of parts of speech that occur consecutively within a sentence using the poses output of the StanfordCoreNLP. It primarily makes sense to look for clusters of proper nouns ("NNP") to extract entities with multi-word names (e.g., people, places, organizations). However, you can look for other types of pose as well. The format is a matrix of tuples of GeoDeepDive docid, sentid, and the cluster of poses.
#'
#' @return A character matrix
#'
#' @author Andrew A. Zaffos & Erika T. Ito
#'
#' @examples
#'
#' # TBDDDDD
#'
#' @rdname consecutivePoses
#' @export
# A function to find proper noun clusters
consecutivePoses<-function(Sentence,Pose="NNP") {
        ParsedSentence<-parseSentence(Sentence,c("words","poses"))
        if(all(is.na(ParsedSentence))) {return(setNames(c(Sentence["docid"], Sentence["sentid"],"parsing error"), c("docid", "sentid", "Proper")))}
        FindConsecutive<-findConsecutive(which(ParsedSentence["poses",]==Pose))
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
