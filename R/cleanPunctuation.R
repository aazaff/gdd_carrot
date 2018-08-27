
#' Clean or replace punctuation oddities
#'
#' A function to help R better parse comma separated arrays taken from PostgreSQL. This includes cleaning of brackets, spaces, commas, and parenthesis.
#'
#' @param Sentence A string or vector of strings
#'
#' @details The punctuation changes made here are not related to GeoDeepDive or its NLP output, rather it is solving artefacts of how R handles concatenated arrays in PostgreSQL. For example, PostgreSQL brackets its arrayws with { }, which R keeps when reading the array in, which is undesirable.
#'
#' @return A string or vector of strings
#'
#' @author Andrew A. Zaffos & Erika T. Ito
#'
#' @examples
#'
#' # Example
#'
#' @rdname cleanPunctuation
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
