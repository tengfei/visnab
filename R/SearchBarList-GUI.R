if(FALSE) {
## load("~/gene/melanie/genesymbol.rda")

## gr1 <-
##   GRanges(seqnames =
##           Rle(c("chr1", "chr2", "chr1", "chr3"), c(1, 3, 2, 4)),
##           ranges =
##           IRanges(1:10, end = 7:16, names = head(letters, 10)),
##           strand =
##           Rle(strand(c("-", "+", "*", "+", "-")),
##               c(1, 2, 2, 3, 2)),
##           score = 1:10,
##           GC = seq(1, 0, length=10))

## gr2 <-
##   GRanges(seqnames =
##           Rle(c("chr2", "chr4", "chr1", "chr5"), c(1, 3, 2, 4)),
##           ranges =
##           IRanges(4:13, end = 8:17, names = tail(letters, 10)),
##           strand =
##           Rle(strand(c("-", "+", "*", "+", "-")),
##               c(1, 2, 2, 3, 2)),
##           geneID = as.factor(c("gene1",rep("gene2",2),rep("gene3",3),
##             rep("gene4",4))),
##           GC = seq(1, 0, length=10))

## grl <- GenomicRangesList("gr1" = gr1, "gr2" = gr2)

# search bar for searching through a GenomicRangesList
qsetClass("SearchBar", Qt$QLineEdit, function(grl, ref = NULL, parent = NULL)
{
  super(parent)
  this$grl <- grl; this$ref <- ref

  unqSeqnames <- function(gr) levels(seqnames(gr))
  unqMetaData <- function(gr) unique(as.character(unlist(values(gr))))

  namesList <- lapply(grl,unqSeqnames)
  names <- unlist(namesList)
  names <- unique(names)
  #names <- unique(unlist(lapply(grl,unqSeqnames)))
  metadataList <- lapply(grl,unqMetaData)
  metadata <- unlist(metadataList)
  metadata <- unique(metadata)
  #metadata <- unique(unlist(lapply(grl,unqMetaData)))

  namesRef <- NULL
  metadataRef <- NULL
  if(!is.null(ref)) {
    namesRef <- levels(seqnames(ref))
    metadataRef <- unique(unlist(sapply(values(ref),as.character)))
  }  
  
  compVec <- sort(unique(c(names,metadata,namesRef,metadataRef)))
  comp <- Qt$QCompleter(compVec)
  this$setCompleter(comp)

  # initialize the GRanges object containing the search range
  searchRange <- NULL

  # parse the text and update GRanges object when return pressed
  qconnect(this, "returnPressed", function() {
    parseSearchString(this$text, grl, ref)
  })
  
})

qsetSignal("rangeChanged", SearchBar)

qsetMethod("getSearchRange", SearchBar, function() {
  this$searchRange
})

qsetMethod("setSearchRange", SearchBar, function(newRange) {
  this$searchRange <- newRange
})

qsetMethod("parseSearchString", SearchBar, function(text, grl, ref = NULL) {
  colon <- grepl(":",text,fixed=TRUE)
  minus <- grepl("-",text,fixed=TRUE)
  plus <- grepl("+",text,fixed=TRUE)

  if(colon & minus) {  # seqname and interval specified
    # obtain seqname, start, end
    colonIdx <- regexpr(":",text,fixed=TRUE)
    minusIdx <- regexpr("-",text,fixed=TRUE)
    name <- substr(text,1,colonIdx-1)
    start <- as.integer(substr(text,colonIdx+1,minusIdx-1))
    end <- as.integer(substr(text,minusIdx+1,nchar(text)))

    # return GRanges object with relevant sequence
    this$searchRange <- GRanges(seqnames =
                     Rle(name, 1),
                     ranges =
                       IRanges(start = start, end = end),
                   )

    this$rangeChanged()
           
  } else if(colon & plus) {  # seqname and center/radius specified
    # obtain seqname, start, end
    colonIdx <- regexpr(":",text,fixed=TRUE)
    plusIdx <- regexpr("+",text,fixed=TRUE)
    name <- substr(text,1,colonIdx-1)
    center <- as.integer(substr(text,colonIdx+1,plusIdx-1))
    radius <- as.integer(substr(text,plusIdx+1,nchar(text)))
    start <- center - radius
    end <- center + radius

    # return GRanges object with relevant sequence
    this$searchRange <- GRanges(seqnames =
                     Rle(name, 1),
                     ranges =
                       IRanges(start = start, end = end),
                   )

    this$rangeChanged()
    
  } else {
    allMetaData <- function(gr) as.character(unlist(values(gr)))

    grNames <- as.vector(sapply(sapply(grl,seqnames),as.character))
    in.names <- grep(text,grNames)
    metaData <- as.vector(sapply(grl,allMetaData))
    in.metadata <- grep(text,metaData)
    in.namesRef <- NULL
    in.metadataRef <- NULL
    if(!is.null(ref)) {
      # there's gotta be a better way to do this!!
      namesReference <- levels(seqnames(ref))
      in.namesRef <- grep(text,namesReference)
      metaDataReference <- unlist(sapply(values(ref),as.character))
      in.metadataRef <- grep(text,metaDataReference)
    }
    if(length(in.names) > 0) {
      grSubset <- gr[seqnames(gr) == text]
      minLoc <- min(IRanges::start(grSubset))
      maxLoc <- max(IRanges::end(grSubset))

      # return GRanges object with relevant sequence
      this$searchRange <- GRanges(seqnames =
                       Rle(text, 1),
                       ranges =
                         IRanges(start = minLoc, end = maxLoc),
                     )

      this$rangeChanged()
    } else if (length(in.metadataRef) > 0 & (!is.null(ref))) {
      matchRows <- in.metadataRef %% dim(values(ref))[1]
      matchRows[matchRows == 0] <- dim(values(ref))[1]
      
      # return GRanges object with matching rows in reference set
      refSubset <- ref[matchRows]
      this$searchRange <- refSubset

      this$rangeChanged()

      # change text in search bar to match intervals being displayed
      setText(base::paste(GenomicRanges::seqnames(refSubset),":",
                          IRanges::start(refSubset),
                          "-",IRanges::end(refSubset),collapse="; ",sep=""))
    } else if (length(in.namesRef) > 0) {
      refSubset <- ref[seqnames(ref) == text]
      minLoc <- min(start(refSubset))
      maxLoc <- max(end(refSubset))

      # return GRanges object with relevant sequence
      this$searchRange <- GRanges(seqnames =
                       Rle(text, 1),
                       ranges =
                         IRanges(start = minLoc, end = maxLoc),
                     )

      this$rangeChanged()      
    } else if (length(in.metadata) > 0) {
      matchRows <- in.metadata %% dim(values(gr))[1]
      matchRows[matchRows == 0] <- dim(values(gr))[1]
      
      # return GRanges object with matching rows in reference set
      grSubset <- gr[matchRows]
      this$searchRange <- grSubset

      this$rangeChanged()

      # change text in search bar to match intervals being displayed
      setText(base::paste(GenomicRanges::seqnames(grSubset),":",
                          IRanges::start(grSubset),"-",
                          IRanges::end(grSubset),collapse="; ",sep=""))
    } else {
      setText("")
      setPlaceholderText("Error: search string not recognized")
      this$searchRange <- NULL
    }
  }

})
}
