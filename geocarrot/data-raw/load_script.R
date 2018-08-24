# Load the data inside
usgs_gdd<-read.table(file="/data-raw/sentences_nlp352.tsv",header=TRUE,stringsAsFactors=FALSE,sep="\t")

# Tell devtools to consider this a data product
devtools::use_data(usgs_gdd)
