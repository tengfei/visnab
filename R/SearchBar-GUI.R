qsetClass("SearchBar", Qt$QLineEdit, function(gr, ref = NULL, parent = NULL)
{
  super(parent)
  this$gr <- gr; this$ref <- ref

  names <- levels(seqnames(gr))
  metadata <- unique(unlist(sapply(values(gr),as.character)))
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
  qconnect(this, "returnPressed", function() parseSearchString(this$text, gr, ref))
})

qsetSignal("rangeChanged", SearchBar)

qsetMethod("getSearchRange", SearchBar, function() {
  this$searchRange
})

qsetMethod("setSearchRange", SearchBar, function(newRange) {
  this$searchRange <- newRange
})

qsetMethod("parseSearchString", SearchBar, function(text, gr, ref = NULL) {
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
    grNames <- levels(seqnames(gr))
    in.names <- grep(text,grNames)
    metaData <- unlist(sapply(values(gr),as.character))
    in.metadata <- grep(text,metaData)
    in.namesRef <- NULL
    in.metadataRef <- NULL
    if(!is.null(ref)) {
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