#' Find children and parents
#'
#' Returns a tuple of a child and a parent defined by the StanfordCoreNLP.
#'
#' @param Sentence a record (row) in the GeoDeepDive NLP output
#' @param Path a stanford core nlp pose code
#'
#' @details 
#'
#' @return A character matrix
#'
#' @author Andrew A. Zaffos & Erika T. Ito
#'
#' @examples
#'
#' # TBDDDDD
#'
#' @rdname adjacencyPath
#' @export
# A function to parse a sentence and extract any grammatically linked termspaired terms
# In principle this should work with other path types, not just amod, but I have not tested it.
findParent<-function(Sentence,Path="amod") {
        ParsedSentence<-parseSentence(Sentence,c("words","dep_paths","dep_parents"))
        if (all(is.na(ParsedSentence))) {return(setNames(c(Sentence["docid"], Sentence["sentid"],rep("parsing error",2)),c("docid","sentid","child","parent")))}
        PathMods<-as.matrix(ParsedSentence[,which(ParsedSentence["dep_paths",]==Path)])
        if (length(PathMods)<1) {return(setNames(rep(NA,4),c("docid","sentid","child","parent")))}
        FinalList<-vector("list")
        for (j in seq_len(ncol(PathMods))) {
                FinalList[[length(FinalList)+1]]<-c(Sentence[1:2],PathMods["words",j],ParsedSentence["words",as.numeric(PathMods["dep_parents",j])])
                }
        FinalTable<-do.call(rbind,FinalList)
        colnames(FinalTable)<-c("docid","sentid","child","parent")
        return(FinalTable)
        }
