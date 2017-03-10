#' Fetch Article Metadata
#'
#' Downloads article bibjson metadata from \code{/articles} route of the GeoDeepDive API.
#'
#' @param Journal a character vector
#' @param Term a character vector
#' @param DocID a character vector
#' @param Surname a character vector; author's last name
#' @param Given a character vector; author's first name
#'
#' @details This function allows you to download article metadata from the GeoDeepDive library. It allows you
#' to choose article metadata based on one more parameters including the journal title, GeoDeepDive assigned
#' document id, author first name, author last name, or term within the article. This data is pulled from the 
#' \href{https://geodeepdive.org/api/articles}{GeoDeepDive /articles} route
#'
#' Note: There are other API parameter options that are not included in this simplified wrapper.
#'
#'
#' @return A list of article metadata
#'
#' @import RJSONIO
#'
#' @author Andrew A. Zaffos & Erika T. Ito
#'
#' @examples
#' # Download the metadata for all articles from the Journal of Vertebrate Paleontology
#' GDDMetadata<-downloadGDD(Journal="Journal of Vertebrate Paleontology")
#'
#' # Download the metadata for all articles from the Journal of Vertebrate Paleontology written by Michael J. Benton
#' GDDMetadata<-downloadGDD(Journal="Journal of Vertebrate Paleontology", Surname="Benton", Given="Michael")
#'
#' @rdname downloadGDD
#' @export
# Downloads article bibjson metadata from /articles route of the GeoDeepDive API.
downloadGDD<-function(Journal="", Term="", DocID="", Surname="", Given="") {
    Journal<-gsub(" ", "%20", Journal)
    Term<-gsub(" ", "%20", Term)
    Surname<-gsub(" ", "%20", Surname)
    Given<-gsub(" ", "%20", Given)
    URL<-paste0("https://geodeepdive.org/api/articles?pubname=",Journal,"&term=",Term,"&lastname=",Surname,"&firstname=",Given)
    JSON<-RJSONIO::fromJSON(URL)
    Output<-parseGDD(JSON)
    return(Output)
    }

parseGDD<-function(JSON) {
    gddid<-sapply(JSON$success$data, function(x) x$`_gddid`)
    type<-sapply(JSON$success$data, function(x) x$type)
    author<-sapply(JSON$success$data, function(x) paste(x$author, collapse=";"))
    year<-sapply(JSON$success$data, function(x) x$year)
    title<-sapply(JSON$success$data, function(x) x$title)
    journal<-sapply(JSON$success$data, function(x) x$journal)
    volume<-sapply(JSON$success$data, function(x) x$volume)
    number<-sapply(JSON$success$data, function(x) x$number)
    pages<-sapply(JSON$success$data, function(x) x$pages)
    publisher<-sapply(JSON$success$data, function(x) x$publisher)
    link<-sapply(JSON$success$data,function(x) unlist(x$link)[["url"]])
    doi<-sapply(JSON$success$data, function(x) unlist(x$identifier)[["id"]])
    Output<-cbind(gddid, type, author, year, title, journal, volume, number, pages, publisher, link, doi)
    return(Output)
    }
