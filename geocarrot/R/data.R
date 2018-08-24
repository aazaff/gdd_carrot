#' Test dataset of public USGS datasets
#'
#' An example StanfordCoreNLP 352 output for 5 USGS documents from GeoDeepDive. Note that the data is not normalized, most fields are comma-separated arrays. We recommend using \code{cleanPunctuation()}
#'
#' @format A character matrix with 14,560 rows and 9 fields:
#' \describe{
#'    \item{docid}{Document id number, alphanumeric code}
#'    \item{sentid}{Sentence ide number, integer identifying the sentence in the document}
#'    \item{wordidx}{integer, index number of word in sentence}
#'    \item{words}{string, the actual words of the sentence}
#'    \item{poses}{grammatical parts of speech codes}
#'    \item{ners}{StanfordCoreNLP default named-entities}
#'    \item{lemmas}{Stems of words}
#'    \item{dep_paths}{Linkages between words}
#'    \item{dep_parents}{The index numbers for the dep_paths}
#' }
#' @source \url{https://geodeepdive.org/}
"usgs_gdd"
