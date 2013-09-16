
#' Redirecting a moved ffdf 
#' 
#'  redirffdf (ff, newdir) , allows to move "manually" a ffdf folder, saved with save.ffdf, 
#'  and avoid file access error, making a redir(ffdf, newdir), with newdir, the dir 
#'  (without counting the ffdf name dir) where we moved the ffdf. 
#'  
#'  @param ff An ffdf data base 
#'  @param newdir A new directory where the ffdf was moved **manually**
#'  @return A ffdf, with "corrected" file-path 
#'  @export

redirffdf <- function(ff, newdir = getwd()) {
  require(ffbase)
  nn <- substitute(ff)
  nn <- all.names(nn)
  #print(nn)
  for (x in physical(ff)) {
    fn <- basename(filename(x))
    physical(x)$filename <- file.path(newdir,nn, fn)
  }
  return (ff)
}


#' Redirecting a moved ffdf 
#' 
#'  redirffdfffdfsave (ff, newdir) , allows to move "manually" a ffdf folder, saved with **ffdfsave**, 
#'  and avoid file access error, making a redirffdfffdfsave(ffdf, newdir), with newdir, the dir 
#'  (without counting the ffdf name dir) where we moved the ffdf. 
#'  
#'  @param ff An ffdf data base 
#'  @param newdir A new directory where the ffdf was moved **manually**
#'  @return A ffdf, with "corrected" file-path 
#'  @export

redirffdfffdfsave <- function(ff, newdir = getwd()) {
  require(ffbase)
  nn <- substitute(ff)
  nn <- all.names(nn)
  nn <- paste(nn,"ff", sep= "")
  for (x in physical(ff)) {
    fn <- basename(filename(x))
    physical(x)$filename <- file.path(newdir,nn, fn)
  }
  return (ff)
}


#' Find if a ffdf column is as 1:N (ordered "row numbers")
#' 
#' Will check if data is ordered by column colname
#' i.e. if colname is exactly as 1,2,3,4,5,.... ,nrows. 
#' @export
#' @param data An ffdf. 
#' @param colname = the name of the column to check. 

isorderedbyrowcol <- function(data,colname){
  require(ffbase)
  rows <- ff(1:nrow(data)); 
  ord <- rows - data[[colname]]; 
  lo <- ffwhich(ord, ord !=0); 
  return(length(lo) == 0)
  
}



#'  Quote a "list" of names : 
#'  
#' quotenames(ident,treatment,diagnose) will give a character vector = c("ident","treatment","diagnose")
#' 
#' very useful to quote a large list of names that are not quoted (param names, var names, etc)
#' @export

quotenames <- function(...){
  x <- substitute(c(...))
  x <- all.names(x)
  N <- length(x)
  x <- x[2:N]
  x
  
}

#'  Quote a "list" of names : 
#'  
#' same as "quotenames(x)"
#' @export

qn <- function(...){
  x <- substitute(c(...))
  x <- all.names(x)
  N <- length(x)
  x <- x[2:N]
  x
  
}


#' Substitute danish letters  
#' 
#' Will substitute danish letters by english ones. 
#' @export 
changedanishletters <- function(y){
  
  x <- gsub("æ","a",y)
  x <- gsub("Æ","A",x)
  x <- gsub("ø","o",x)
  x <- gsub("Ø","O",x)
  x <- gsub("å","a",x)
  x <- gsub("Å","A",x)
  x  
}


#' Order a ffdf, in splits to avoid memory problems. 
#' 
#' orderffdf will order the ffdf using a column 
#' it will make the process using each time a portion of the total database 
#' @export
#' @param data : a ffdf data base.
#' @param ordercols : vector of names of the columns to order the data 
#' @param splits : number of splits to make while ordering. Will avoid memory problems. 
#' @param verbose : if verbose the process. 

orderffdf <- function(data, ordercols = names(data), splits = 1, verbose = F){
  require(ffbase)
  idx <-ffdforder(data[c(ordercols)])
  if(verbose) print(paste("lenght result ", length(idx)))
  res <- indexffdfsplit(data,idx,splits,verbose)
  return(res)
}



#' Subset a ffdf, with splits and conditions. 
#' 
#' subsetffdf will subset the ffdf using ffwhich over conditions ...
#' and will make the data using indexffdf 
#' @export
#' @param data : a ffdf data base.
#' @param conditioncols : the columns that are used in the condition to subset 
#' @param condition : a condition to subset, like " diagnose != "a" " 
#' @param splits : number of splits to make while subsetting. Will avoid memory problems. 
#' @param verbose : if verbose the process. 

subsetffdf <- function(data, conditioncols = names(data),condition, splits = 1, verbose = F){
  require(ffbase)
  exp <-substitute( (condition) %in% TRUE)
  idx <- do.call(ffwhich, list(data, exp) ) # here is the trick: do.call !!!
  #widx <-with(data[c(conditioncols)], ...)
  #idx <- ffwhich(widx,widx %in% TRUE)  # this makes NA be FALSE ... 
  
  #exp <-substitute(condition)
  #idx <-ffwhich(data[c(conditioncols)], exp  )
  
  print(paste("length result ", length(idx)))
  res <- indexffdfsplit(data,idx,splits,verbose)
  return(res)
}

#'  Simpler ffdfsave  
#'  
#'  will save the data in a file named as the data using ffdfsave. 
#'  @export

saveffdf <- function(...){ require(ffbase); ffdfsave(...,file = all.names(substitute(...))[[1]]  )}


#'  Column bind two ffdf data bases. 
#'  
#'  ffdfs must be same row length 
#'  @export

cbindffdf <- function(x,y){
  require(ffbase)
  Ncol1 <- ncol(x); 
  Ncol2 <- ncol(y)
  for( col in 1:Ncol2){
    namecol <- names(y)[col]
    x[[namecol]] <- y[[namecol]] 
    
  }
  return(x)
  
}


#'  Append two ffdf by chunks 
#'  
#' Will append TWO ffdfs "x" and "y" by chunks of size "chunks"
#' Will verbose some information on timings.  
#' @export 

appendffdf <- function(x,y,chunks){
  require(ffbase)
  chunk <- chunks; 
  y <- y[names(x)] # select only x cols or order them as x 
  Nsplits <- ceiling(nrow(y) / chunk )
  beg <- Sys.time(); 
  
  rowend <- 0L
  
  for(i in 1:Nsplits){
    
    begloop <- Sys.time()
    rowini <-as.integer( rowend +1)
    rowend <-as.integer(i * chunk)
    
    if(rowend > nrow(y)) rowend <- nrow(y)
    z <-  y[rowini:rowend, ][,] 
    if(i == 1){ 
      
      res <- ffdfappend(x,z )
      
    } else {
      
      res <- ffdfappend(res,z )   
    }
    rowini <- rowend +1 
    timeloop <- Sys.time() - begloop
    print(paste( " done split ", i, "/", Nsplits, 
                 ", rowend ", rowend, 
                 " spent : ", round(timeloop) , 
                 ", E. total ", round(timeloop * (Nsplits)), 
                 ", E. remain  ", round( timeloop * (Nsplits -i))  
    )
    ) 
  }
  
  print(paste(" Total time :", round( Sys.time()-beg , digits = 1)))
  return(res)
  
}

#' Merge two data bases (ffdf)
#' 
#' merge two data bases ffdf
#' , using a merge in data.table 
#' in the form ::  res <- y[x,nomatch =0 ], so the result will be as y[x, nomatch =0] . 
#' @param splitcolname : it is taking  splitcolname to make splits and not break for same values of that col. 
#' @param keycols :  the common cols to make the merge.
#' @export
####################################

fumerge <- function(x,y,splitcolname, keycols ) {
  require(ffbase); require(data.table)
  x0 <- x[1,]
  x0 <- x0[0,]
  x <-data.table(x[,]); 
  #keycols <- c("ident","dateofonderzoek","registry") 
  setkeyv(x, keycols )
  z <- y[[splitcolname]]
  zz <- ffdf( ids = z)
  xx <-  x[[splitcolname]][]
  idx <- with(zz, ids %in% xx)
  idx <- ffwhich(idx,idx)
  
  if(length(idx) > 0 ) {
    
    #rownames(y) <- NULL  ## to avoid that row.names != NULL  problem... .
    y <-indexffdf(y,idx)
    y2 <-data.table(y[,]); 
    setkeyv(y2, keycols )
    gc()
    res <- y2[x, nomatch =0] # this makes it equivalent as marge(x,y) and sql( select ... where t1.--- = t2. --- )
    rm(x,y2,z,idx, xx,zz)
    res 
  } 
  else {
    print("no matching indexes :: returning empty data.frame")
    rm(z,idx, xx)
    return(x0)
  }
  
}

#' Merge by  two ffdf by chunks 
#'  
#' merge by is an application of 
#' fumerge, for a big data ffdf. 
#' it will split the comput by chunks 
#' of size chk. 
#' @export
#' @param x,y : ffdfs to merge. 
#' @param keycols : common columns. 
#' @param splitname : name of the column to make the splits. 
#' @param chk: chunk size (number of rows to take in a single pass)
####################################

mergeby  <- function(x,y,keycols, splitname,  chk){
  require(ffbase); require(data.table)
  chunks <- chk # size of the split.  
  res <- splitapplycpp(x, x[[splitname]],chunks ,function(x) fumerge(x,y,splitname, keycols)  )
}


#' Index a ffdf by an index vector idx (re-order or subsetting)
#' 
#' Will index a ffdf by the index idx. (reorder). 
#' avoids memory overflow if one single col is able to go into memory.... 
#' @param x : ffdf 
#' @param idx : index vector (ff vector or ram vector -not prefered-)
#' @param verbose : verbose the process. 
#' @export
############################################################

indexffdf <- function(xwe34574, idx, verbose = F ){
  require(ffbase)
  gc()
  colnames <- names(xwe34574)
  N <- ncol(xwe34574); 
  open.ffdf(xwe34574) 
  
  for( i in 1:N) {
    
    name <- colnames[i]
    if(verbose) print(paste( "making row ", name))
    z14246456457 <- xwe34574[idx, name][[name]]
    ifelse( i==1 , 
            
            df <- ffdf( z14246456457 ) , 
            df[[name]] <- z14246456457
    )
    #rm(z14246456457)
    gc()   
  }
  names(df) <- colnames
  df 
  
  
}

#' Index a ffdf by an index vector idx (re-order or subsetting)
#' 
#' Will index a ffdf by the index idx. (reorder). 
#' avoids memory overflow if one single col is able to go into memory.... 
#' avoids memory overflow by splitting the computation in several parts. 
#' @param x : ffdf 
#' @param idx : index vector (ff vector or ram vector -not prefered-)
#' @param nsplits: number of splits to run. Avoids memory issues.  
#' @param verbose : verbose the process. 
#' @export 
############################################################

indexffdfsplit <- function(datainput, idx, nsplits = 1, verbose = F ){
  require(ffbase)
  gc()
  colnames <- names(datainput)
  N <- ncol(datainput); 
  open.ffdf(datainput) 
  
  for( i in 1:N) {
    
    name <- colnames[i]
    if(verbose) print(paste( "making row ", name))
    rowend <- 0
    Nrows <- nrow(datainput)
    Nidx <- length(idx)
    splitrows <- ceiling(Nidx/nsplits)
    
    if(nsplits > 1){
      
      for( split in 1:nsplits){
        
        if(verbose) print(paste( ".... split  ", split, "/" , nsplits))
        rowbeg <- rowend + 1 
        rowend <- rowend + splitrows
        
        if(rowend > Nidx) rowend <- Nidx
        beg <- as.integer(rowbeg)
        end <- as.integer(rowend)
        indxs <- ff(idx[beg:end])
        
        y45234316 <- datainput[indxs, name][[name]]
        
        ifelse( split==1 , z45234316 <- y45234316 , {z45234316 <- ffappend(z45234316,y45234316)}   )
        
      }
      
    }else {
      
      z45234316 <- datainput[idx, name][[name]] 
    }
    
    ifelse( i==1 , 
            
            df <- ffdf( z45234316 ) , 
            df[[name]] <- z45234316
    )
    rm(z45234316)
    gc() 
    
  }
  names(df) <- colnames
  df 
  
  
}


#' Apply a function over a ffdf, in splits. 
#' 
#' will apply a function to a ffdf:
#' avoids memory overflow by splitting the computation in several parts.  
#' the function fu should return a R data.frame as applied to a R data.frame. 
#' @param datainput : the ffdf to apply the function over. 
#' @param fu : function to apply in the ffdf. It must return a data.frame, as if it is working over a R data.frame. 
#' @param splits : number of splits to process. Avoids memory issues. 
#' @param verbose : verbose the process. 
#' @export
############################################################

applyffdfsplit <- function(datainput, fu, nsplits = 1, verbose = F )
{
  
  require(ffbase)
  gc()
  colnames <- names(datainput)
  open.ffdf(datainput) 
  
  rowend <- 0
  Nrows <- nrow(datainput)
  Nidx <- Nrows
  splitrows <- ceiling(Nidx/nsplits)
  
  if(nsplits > 1){
    
    for( split in 1:nsplits){
      
      # if(verbose) print(paste( ".... split  ", split, "/" , nsplits))
      rowbeg <- rowend + 1 
      rowend <- rowend + splitrows
      
      if(rowend > Nidx) rowend <- Nidx
      beg <- as.integer(rowbeg)
      end <- as.integer(rowend)
      indxs <- beg:end
      if(verbose) print(paste( ".... split  ", split, "/" , nsplits, " ", beg, ":", end))
      
      y45234316 <- datainput[indxs, ]
      
      res3123 <- fu(y45234316)
      
      rm(y45234316)
      
      ifelse( split==1 , z45234316 <- as.ffdf(res3123), {z45234316 <- ffdfappend(z45234316,res3123)}   )
      
    }
    
  }else {
    
    y45234316 <- datainput[,]
    res3123 <- fu(y45234316)
    rm(y45234316)
    z45234316 <- as.ffdf(res3123)
    
  }
  
  
  df <- z45234316
  gc() 
  df 
  
}



#' Apply a function over a ffdf, in chunks (number of rows) 
#' 
#' will apply a function to a ffdf
#' avoids memory overflow by splitting the computation in several parts.  
#' the function fu should return a R data.frame as applied to a R data.frame. 
#' @param data  : the ffdf to apply the function over. 
#' @param fu : function to apply in the ffdf. It must return a data.frame, as if it is working over a R data.frame. 
#' @param chunksize : number rows to process by split. Avoids memory issues. 
#' @param verbose : verbose the process. 
############################################################

applyffdfchunks <- function(data, fu, chunksize = 1e6, verbose = F )
{
  require(ffbase)
  gc()
  colnames <- names(data)
  open.ffdf(data) 
  
  rowend <- 0
  Nrows <- nrow(data)
  Nidx <- Nrows
  nsplits <- ceiling(Nidx/chunksize)
  splitrows <- chunksize
  
  if(nsplits < 1) nsplits <- 1
  
  if(nsplits > 1){
    
    for( split in 1:nsplits){
      
      #if(verbose) print(paste( ".... split  ", split, "/" , nsplits))
      rowbeg <- rowend + 1 
      rowend <- rowend + splitrows
      
      if(rowend > Nidx) rowend <- Nidx
      beg <- as.integer(rowbeg)
      end <- as.integer(rowend)
      indxs <- beg:end
      #if(verbose) print(paste( ".... .... rows:   ", beg, " to " , end))
      
      if(verbose) print(paste( ".... split  ", split, "/" , nsplits, " ", beg, ":", end))
      
      y45234316 <- data[indxs, ]
      
      res3123 <- fu(y45234316)
      
      rm(y45234316)
      
      ifelse( split==1 , z45234316 <- as.ffdf(res3123), {z45234316 <- ffdfappend(z45234316,res3123)}   )
      
    }
    
  }else {
    
    y45234316 <- data[,]
    res3123 <- fu(y45234316)
    rm(y45234316)
    z45234316 <- as.ffdf(res3123)
    
  }
  
  
  df <- z45234316
  gc() 
  df 
  
}


#### functions to get the data from split info :: ################

getdatasplit <- function(data, chunkindexes, chuknumber){
  
  beg <- chunkindexes[chuknumber, "begdataindex"]; 
  end <- chunkindexes[chuknumber, "enddataindex"];
  return(data[beg:end, ])
  
}

getdatatablesplit <- function(data, chunkindexes, chuknumber){
  require(data.table)
  beg <- chunkindexes[chuknumber, "begdataindex"]; 
  end <- chunkindexes[chuknumber, "enddataindex"];
  return(data.table( data[beg:end, ]) )
  
}


#'  A split-apply-combine over and ORDERED ffdf data.base. 
#'  
#' split apply over an ORDERED data frame (or ffdf), using the cpp 
#' functions getsplitindexes and getchunksindexesforsplitlist

#'  \code{fu(x)}  will be something like : 
#'   \code{ x <- data.table(x) ; setkey(x, "ident") ; 
#'  x[ , list( max.diagnose = max(dignose)), by = ident ]
#'  }  
#'  for example....
#'  or any function that gives a data frame operating in each split of the data, 
#'  that are "respecting" the split-vector (i does not break data for same values of split-vector)
#'  @param inputdata : the input ffdf. It MUST BE ORDERED by the SPLIT VECTOR !!. 
#'  @param splitvector : the ff vector to use for the splits. 
#'  @param chksize : the number of rows to take in a single computing pass (RAM)
#'  @param fu : a function that returns a data.frame over a data.frame (for a single value of the split vector)
#'  @export
####################################################################

splitapplycpp <- function(inputdata, splitvector, chksize , fu){
  
  require(ffbase); require(data.table); require(LargeDataCppFunctions)
  print(nrow(inputdata))
  t.begin <- Sys.time()
  
  data <- data.frame( ids = as.integer(splitvector[]) ) # create a data frame with one col which is the split factor as integer...
  
  splitindexes  <- getSplitIndexes(data, "ids") # a cpp function to get the indexes for the splits... 
  print(length(splitindexes))
  
  #the size for each chunk. Setting to 3e6 rows gets good results. No mem overflow.... lower if there are mem. alloc. problems... 
  chksize <- chksize
  if(chksize > nrow(inputdata)) chksize = nrow(inputdata); #print(chksize)
  
  sp <- getChunksIndexesForSplitList(data, splitindexes, chksize) # cpp function that gives the row numbers for splits in indexes and in data.base.
  rm(data) 
  data <- inputdata   
  ##print(sp[1:2,])
  Nsplits <- nrow(sp); 
  
  print(paste("Number of Splits " , Nsplits, " / ", "Chunk Size",  chksize )) 
  
  ########## first process :: using data.table to split ###############
  #we do not need the splitindexes data ::
  rm(splitindexes) ; gc()
  
  ii <- 0 
  
  print("Beginning Splits ------ " )
  for ( i in 1:Nsplits){ 
    
    print(paste("Split ", i, "/", Nsplits))
    t.beg <- Sys.time()
    print(gc())
    splitnumber <- i
    beg <- sp[splitnumber, "begdataindex"]
    end <- sp[splitnumber, "enddataindex"]
    
    print(paste("---Working on rows  ", beg, "to ", end))
    
    dat <- data[beg:end, ] 
    print(paste("------ function"))
    ## dat <- getdatatablesplit(data,sp,splitnumber)  
    ## setkeyv(dat, c(splitname)) 
    
    res <- fu(dat)   ## fu(x) will be something like : {  x <- data.table(x) ; setkey(x, "ident") ; x[ , list( max.diagnose = max(dignose)), by = ident ] } for example.... 
    #print(nrow(res));
    print(paste("------ append  "))
    
    cont <- TRUE 
    ii <- ii+1
    exp <- (is.data.frame(res) & nrow(res) > 0) %in% FALSE 
    #print(exp)
    if(exp) { cont <- FALSE ; ii <- ii-1}
    #print(cont)
    if( ii == 1 & cont) { result <- as.ffdf(res)}
    if( ii != 1 & cont) { result <- ffdfappend(result, res)} 
    
    rm(res) 
    rm(dat) 
    t.e <- Sys.time()
    print(paste("------time elapsed in split ", i , "of", Nsplits, " = ", round(t.e - t.beg, digits = 1)))  
    print(paste("------Estimated total time ",  Nsplits * round(t.e - t.beg, digits = 1),  "E.Time.Remaining ", (Nsplits-i) * round(t.e - t.beg, digits = 1)  ))
  }
  t.end <- Sys.time()
  print(paste("total time elapsed -----  ", round(t.end - t.begin, digits = 1)) )  
  gc()
  
  return(result)  
  
}


#'  Lag a vector by 1 position, with value NA . 
#'  
#' if the original vector is \code{ x = c(0,1,2)} the result 
#' will be \code{ lagpad(x) --> c(NA,0,1)}.

#'  @export
####################################################################

lagpad <- function(x) {
  
  N <- length(x)
  if(is.factor(x)){
    lev <- levels(x)
    xlag <- factor(c(NA,as.character(x)), levels = lev)[1:N]
    #print(xlag)
    
  } else {
    xlag <- c(NA, x)[1:N]
    
  }
  xlag
}

#'  Integer to date with no need of specifying the origin. 
#'  
#' Assumes the origin to be 1970-01-01. 
#' \code{asDate(x)} is the same as \code{as.Date(x,origin="1970-01-01")} 
#'  @export

asDate <- function(x) {
  
  return ( as.Date(x, origin="1970-01-01"))
}

#' Numeric to percent with percent symbols for labelling 
#' 
#' numeric to percents with percent symbols for labelling , with "dec" = decimals to display. 
#' @examples
#' pp <- c(23.121 , 1.34, 0.345); 
#' fpp <- txt.perc(pp,2); fpp
#' fpp <- txt.perc(pp,1); fpps
#' @export 
txt.perc <- function(x, dec){ paste( format ( x, digits = 1, nsmall = dec), "%", sep = "") }


#' Fill a vector with characters to get a fixed character length for all the items. 
#'  
#' Will add 0 at the beginning for integer and numeric vectors 
#' and the "fill" character at the END for other types. 
#' @param x : a vector 
#' @param fill : a character to "fill" the vector. 
#' @export 
#' @return a character vector of fixed width (equal to the maximum input width)
fill.vec <- function(x, fill = "*") {
  
  #if(class(x) =="Date" ) return(x)
  
  
  if(is.integer(x) | is.numeric(x)){
    
    fill <- 0
    y1 <- as.character(x)
    y2 <- sub('^ +', '', sub(' +$', '', y1))  ## delete spaces only outside the element
    #print(y)
    rm(y1)
    z <- nchar(y2)
    max.char <- max(z,na.rm = TRUE)
    min.char <- min(z,na.rm = TRUE)
    if(max.char == min.char) return(y2)  ## if char is constant, then return original... 
    rm(z)
    char.print <- paste("% ",max.char,"s",sep="")
    y3 <- sprintf(char.print,y2)
    rm(y2)
    #print(y)
    
    y <- gsub(" ", fill, y3)
    rm(y3)
    #print(y)
    #rm(y)
    return(y)
  }
  
  
  y1 <- as.character(x)
  y2 <- sub('^ +', '', sub(' +$', '', y1))  ## delete spaces only outside the element
  z <- nchar(y2)
  max.char <- max(z,na.rm = TRUE)
  min.char <- min(z,na.rm = TRUE)
  if(max.char == min.char) return(y2)  ## if char is constant, then return original... 
  rm(z, y1)
  
  char.print <- paste("% -",max.char,"s",sep="")
  
  dull.replace <-"ý"
  y3 <- gsub(" ", dull.replace, y2) # change the white spaces to a "dull" character (a very strange one,,,) ...
  rm(y2)
  y4 <- sprintf(char.print,y3)      # write spaces padding . 
  rm(y3)
  y5 <- gsub(' ', fill, y4)       # now change the white spaces for the fill character.
  rm(y4)
  y <- gsub(dull.replace, " ", y5)  # recover the white spaces.
  rm(y5)
  #rm(y)
  gc()
  return(y)
}

#' Fill a vector with characters to get a fixed width for all the items. 
#'  
#' Will ALWAYS  add the  fill" character at the END for other types. 
#' @param x : a vector 
#' @param fill : a character to "fill" the vector. 
#' @export 
#' @return a character vector of fixed width (equal to the maximum input width)
fill.vec.all.same <- function(x, fill = "*") {
  
  y1 <- as.character(x)
  y2 <- sub('^ +', '', sub(' +$', '', y1))  ## delete spaces only outside the element
  z <- nchar(y2)
  max.char <- max(z,na.rm = TRUE)
  min.char <- min(z,na.rm = TRUE)
  if(max.char == min.char) return(y2)  ## if char is constant, then return original... 
  rm(z, y1)
  
  char.print <- paste("% -",max.char,"s",sep="")  # write padded spaces after to make all same length. 
  
  dull.replace <-"ý"
  y3 <- gsub(" ", dull.replace, y2) # change the white spaces to a "dull" character (a very strange one,,,) ...
  rm(y2)
  y4 <- sprintf(char.print,y3)      # write spaces padding . 
  rm(y3)
  y5 <- gsub(' ', fill, y4)       # now change the white spaces for the fill character.
  rm(y4)
  y <- gsub(dull.replace, " ", y5)  # recover the white spaces.
  rm(y5)
  #rm(y)
  gc()
  return(y)
}



#' Fill a data.frame with characters to get a fixed character length for all the items. 
#'  
#' Will add 0 at the beginning for integer and numeric vectors 
#' and the "fill" character at the END for other types. 
#' @param x : a data.frame 
#' @param fill : a character to "fill" the vectors. 
#' @param make.factors : if making the resulting columns as factors.
#' @export 
#' @return a character vector of fixed width (equal to the maximum input width)
fill.data.frame <- function(x, fill.char = "*", make.factors = F){   
  
  #y <- x 
  fill <- fill.char 
  
  for (i in 1:ncol(x)){
    
    x[,i] <- fill.vec( x[,i], fill)
    #if(class(x[[i]] == "factor"))  y[,i] <- as.factor(y[,i])
    if(make.factors) x[,i] <- as.factor(x[,i]) 
    gc()
    
  }
  
  return(x)
}


#' Fill a data.frame with characters to get a fixed width for all the items. 
#'  
#' Will ALWAYS  add the  fill" character at the END for other types. 
#' @param x : a data.frame 
#' @param fill : a character to "fill" the vectors. 
#' @param make.factors : if making the resulting columns as factors.
#' @export 
#' @return a character vector of fixed width (equal to the maximum input width)
fill.data.frame.all.same <- function(x, fill.char = "*", make.factors = F){   
  
  #y <- x 
  fill <- fill.char 
  
  for (i in 1:ncol(x)){
    
    x[,i] <- fill.vec.all.same( x[,i], fill)
    #if(class(x[[i]] == "factor"))  y[,i] <- as.factor(y[,i])
    if(make.factors) x[,i] <- as.factor(x[,i]) 
    gc()
    
  }
  
  return(x)
}


info.time.done <- function(id, uid, begin){
  
  ## id is the variable in the loop that grows while computing. 
  ## uid is the unique values of that id, the total number of steps. 
  ## begin is the beginning time as in begin <- Sys.time()
  
  #output the percent done ::::
  #calculate the time left :: 
  
  mm <- floor(id/uid*10) != floor((id+1)/uid*10)
  
  if(mm ) {
    
    perc <- round((id / uid * 100 )); 
    t.tmp <- paste(perc, "%", sep = "")
    predicted  <- (Sys.time()-begin) * (100/perc);
    nowleft <- predicted * (1-perc/100); 
    t2.tmp <- paste( "time left" , format(nowleft)  )
    t3.tmp <- paste( "total time predicted" , format(predicted)  )
    t4.tmp <- paste(t.tmp, t2.tmp, sep = " // ")
    print( paste( t4.tmp, t3.tmp, sep = " // " ) )
    
    
  }
}



grouprunningcumsum <- function (x, max) 
{
  require(ffbase)
  l <- as.integer(length(x))
  if (l == 0) {
    return(x)
  }
  x <- as.integer(x)
  max <- as.integer(max)
  result <- .C("grouprunningcumsum", x = x, l = l, max = max, 
               PACKAGE = "ffbase")
  result$x
}

ffdfgetcolumnwise <-function (x, index = NULL) 
{
  require(ffbase)
  listtodf <- function(list) {
    rows <- unique(unlist(lapply(list, NROW)))
    structure(list, class = "data.frame", row.names = seqlen(rows))
  }
  res <- list()
  if (is.null(index)) {
    for (measure in names(x)) {
      open(x[[measure]])
      res[[measure]] <- x[[measure]][]
      close(x[[measure]])
    }
  }
  else if (is.ff(index)) {
    if (vmode(index) %in% c("boolean", "logical")) {
      index <- ffwhich(index, index == TRUE)
    }
    os <- ffindexordersize(length = NROW(x), vmode = "integer")
    o <- ffindexorder(index, os$b)
    for (measure in names(x)) {
      open(x[[measure]])
      res[[measure]] <- ffindexget(x = x[[measure]], index = index, 
                                   indexorder = o)[]
      close(x[[measure]])
    }
  }
  else {
    for (measure in names(x)) {
      open(x[[measure]])
      res[[measure]] <- x[[measure]][index]
      close(x[[measure]])
    }
  }
  listtodf(res)
}



#' A simple modification of ffdfdply from ffbase package. 
#' 
#' it will make the computation more clear by outputting time-info  
#' @export

ffdfsplit <-  function (x, split, FUN, BATCHBYTES = getOption("ffbatchbytes"),   RECORDBYTES = sum(.rambytes[vmode(x)]), trace = TRUE, ...) 
{
  begin <- Sys.time() #mio. 
  require(ffbase)
  splitvmode <- vmode(split)
  if (splitvmode != "integer") {
    stop("split needs to be an ff factor or an integer")
  }
  splitisfactor <- is.factor.ff(split)
  MAXSIZE = BATCHBYTES/RECORDBYTES
  message("making the table.ff .... ")
  splitbytable <- table.ff(split, useNA = "no")
  print(gc())
  
  #m: ordering the split by table: i.e. first the one which has the bigger split- 
  #m: i do not want this one to happen, so i re-define :: 
  #m: in this way, i get the same order as the data :: 
  
  splitbytableo <- splitbytable[order(splitbytable, decreasing = TRUE)]
  if (max(splitbytableo) > MAXSIZE) {
    warning("single split does not fit into BATCHBYTES")
  }
  rm(splitbytableo)
  
  #m: so now the splits are not ordered by number of same items, by as the factors or dates, of whatever.... 
  
  tmpsplit <- grouprunningcumsum(x = as.integer(splitbytable), 
                                  max = MAXSIZE)
  nrsplits <- max(tmpsplit) #the number of splits we will do. 
  
  allresults <- NULL
  for (idx in 1:nrsplits) {
    
    beg <- Sys.time() #mio
    
    tmp <- names(splitbytable)[tmpsplit == idx] # tmp is the ids of the ones in this split.  
    
    if (!splitisfactor) {
      if (!is.null(ramclass(split)) && ramclass(split) == 
            "Date") {
        tmp <- as.Date(tmp)
      }
      else {
        tmp <- as.integer(tmp)
      }
    }
    if (trace) {
      message(sprintf("%s, working on split %s/%s , uniques : %s", 
                      #Sys.time(), idx, nrsplits, paste(tmp, collapse = ", ")))
                      Sys.time(), idx, nrsplits, length(tmp)  ) ) 
    }
    if (splitisfactor) {
      fltr <- split %in% ff(factor(tmp, levels = names(splitbytable)))
    }
    else {
      if (!is.null(ramclass(split)) && ramclass(split) == 
            "Date") {
        fltr <- split %in% ff(tmp, vmode = "integer", 
                              ramclass = "Date")
      }
      else {
        fltr <- split %in% ff(tmp, vmode = "integer")
      }
    }
    
    #message( sprintf("---ffdfgetcolumnwise ") )  # mio
    inram <- ffdfgetcolumnwise(x, fltr)
    #message( sprintf("---end  ffdfgetcolumnwise") )  # mio
    if (trace) message( sprintf("---beginning FUN call --- length data : %s", nrow(inram) ))  # mio 
    begf <- Sys.time()  # mio
    
    result <- FUN(inram, ...)
    gc() #mio ... 
    if (trace) message(paste("---end FUN call: time spent in FUN " ,round( Sys.time() - begf )) ) #mio. 
    
    if (!inherits(result, "data.frame")) {
      stop("FUN needs to return a data frame")
    }
    rownames(result) <- NULL
    if (!is.null(allresults) & nrow(result) > 0) {
      rownames(result) <- (nrow(allresults) + 1):(nrow(allresults) + 
                                                    nrow(result))
    }
    if (trace) message("--beginning append")
    if (nrow(result) > 0) {
      allresults <- ffdfappend(x = allresults, dat = result, 
                               recode = FALSE)
    }
    
    dif.time <- Sys.time()- beg #mio 
    if (trace) message(
      paste(
        "--end append // time spent in split: ", round(dif.time), 
        " // estimated time left ::", round( (nrsplits - idx) * dif.time) ) ,
      " // estimated total time ::", round( nrsplits* dif.time) 
    ) #mio 
    message(" ------------------------------------------------") #mio
    
  }
  print(paste("total time spent ::", Sys.time() - begin )) #mio 
  allresults
}



