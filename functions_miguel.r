

# load.ffdf.redir <- function(data){ 
#   
#    nn <- substitute(data)
#    nn <- all.names(nn)
#    print(nn)
#    y <- load.ffdf(nn)
#    x <- redir_ffdf(data)
#    return(x)
# }


############################
#
# substitute danish letters  

###########################
change_danish_letters <- function(y){
  
  x <- gsub("æ","a",y)
  x <- gsub("Æ","A",x)
  x <- gsub("ø","o",x)
  x <- gsub("Ø","O",x)
  x <- gsub("å","a",x)
  x <- gsub("Å","A",x)
  x  
}



#########################
# thi one allows to move "manually" a ffdf folder, saved with save.ffdf, 
# and avoid file access error, making a redir(ffdf, newdir), with newdir, the dir 
# (without counting the ffdf name dir) where we moved the ffdf. 

redir_ffdf <- function(ff, newdir = getwd()) {
  nn <- substitute(ff)
  nn <- all.names(nn)
  #print(nn)
  for (x in physical(ff)) {
    fn <- basename(filename(x))
    physical(x)$filename <- file.path(newdir,nn, fn)
  }
  return (ff)
}


redir_ffdf_ffdfsave <- function(ff, newdir = getwd()) {
  nn <- substitute(ff)
  nn <- all.names(nn)
  nn <- paste(nn,"ff", sep= "_")
  for (x in physical(ff)) {
    fn <- basename(filename(x))
    physical(x)$filename <- file.path(newdir,nn, fn)
  }
  return (ff)
}
#########################



##########################
# remove_all <- function(){
#   
#  rm(list = ls()); 
#  print(memory.size()); 
#   
# }
#########################



##########################
## will check if data is ordered by column col_name
## i.e. if col_name is exactly as 1,2,3,4,5,.... ,nrows. 

is_ordered_by_row_col <- function(data,col_name){
  
  rows <- ff(1:nrow(data)); 
  ord <- rows - data[[col_name]]; 
  lo <- ffwhich(ord, ord !=0); 
  return(length(lo) == 0)
  
}
#########################



##########################
#  quote a "list" of names : 
#  quote_names(x) were x is in the form id,dig,ers,wes,XXX,sertt
# example : 
# quote_names(ident,treatment,diagnose) will give a character vector = c("ident","treatment","diagnose")
# very useful to quote a large list of names that are not quoted (param names, var names, etc)
############################

quote_names <- function(...){
  x <- substitute(c(...))
  x <- all.names(x)
  N <- length(x)
  x <- x[2:N]
  x
  
}
# we can also call the function like this :: 
qn <- function(...){
  x <- substitute(c(...))
  x <- all.names(x)
  N <- length(x)
  x <- x[2:N]
  x
  
}

##########################
#  order_ffdf <- function(data, order_cols = names(data), splits = 1, verbose = F)
#  will order the ffdf using ffdforder and index_ffdf_split ...
# and will make the data using index_ffdf 
############################

order_ffdf <- function(data, order_cols = names(data), splits = 1, verbose = F){
  
  idx <-ffdforder(data[c(order_cols)])
  if(verbose) print(paste("lenght result ", length(idx)))
  res <- index_ffdf_split(data,idx,splits,verbose)
  return(res)
}



##########################
#  subset_ffdf <- function(data, condition_cols = names(data), ..., splits = 1, verbose = F)
#  will subset the ffdf using ffwhich over conditions ...
# and will make the data using index_ffdf 
############################

# subset_ffdf <- function(data,expression_condition, condition_cols = names(data),splits = 1, verbose = F){
#   #cond <- expression(condition)
#   idx <-ffwhich(data[condition_cols], expression_condition)
#   res <- index_ffdf_split(data,idx,splits,verbose)
#   return(res)
# }

# 
subset_ffdf <- function(data, condition_cols = names(data),condition, splits = 1, verbose = F){

  exp <-substitute( (condition) %in% TRUE)
  idx <- do.call(ffwhich, list(data, exp) ) # here is the trick: do.call !!!
  #widx <-with(data[c(condition_cols)], ...)
  #idx <- ffwhich(widx,widx %in% TRUE)  # this makes NA be FALSE ... 
  
  #exp <-substitute(condition)
  #idx <-ffwhich(data[c(condition_cols)], exp  )
  
  print(paste("length result ", length(idx)))
  res <- index_ffdf_split(data,idx,splits,verbose)
  return(res)
}

#########################


#########################33
# simple ffdfsave  : will save the data in a file named as the data . 
###########################

save_ffdf <- function(...){ ffdfsave(...,file = all.names(substitute(...))[[1]]  )}

################################
# CBIND TWO FFDF 
################################
# they must be same row length 

cbind_ffdf <- function(x,y){
  
Ncol1 <- ncol(x); 
Ncol2 <- ncol(y)
for( col in 1:Ncol2){
  name_col <- names(y)[col]
  x[[name_col]] <- y[[name_col]] 
  
}
return(x)

}

#################################
# append_ffdf will append TWO ffdfs by chunks, .... 
# ffdf is not able to do that .... 
#################################

append_ffdf <- function(x,y_,chunk_s){
  
  chunk <- chunk_s; 
  y <- y_[names(x)] # select only x cols or order them as x 
  Nsplits <- ceiling(nrow(y) / chunk )
  beg <- Sys.time(); 
  
  row_end <- 0L
  
  for(i in 1:Nsplits){
    
    beg_loop <- Sys.time()
    row_ini <-as.integer( row_end +1)
    row_end <-as.integer(i * chunk)
    
    if(row_end > nrow(y)) row_end <- nrow(y)
    z <-  y[row_ini:row_end, ][,] 
    if(i == 1){ 
     
      res <- ffdfappend(x,z )

    } else {

      res <- ffdfappend(res,z )   
    }
    row_ini <- row_end +1 
    time_loop <- Sys.time() - beg_loop
    print(paste( " done split ", i, "/", Nsplits, 
                 ", row_end ", row_end, 
                 " spent : ", round(time_loop) , 
                 ", E. total ", round(time_loop * (Nsplits)), 
                 ", E. remain  ", round( time_loop * (Nsplits -i))  
                 )
          ) 
  }

  print(paste(" Total time :", round( Sys.time()-beg , digits = 1)))
  return(res)

}

####################################
# merge two data bases, that can be ffdf
# it is using a merge in data.table 
# in the form ::  res <- y[x]
# so the result will be as y[x]
# it is taking split_col_name to make splits and not 
# break for same values of that col. 
# keycols are the common cols to make the merge.
####################################

fu_merge <- function(x,y,split_col_name, keycols ) {
  
  x0 <- x[1,]
  x0 <- x0[0,]
  x <-data.table(x[,]); 
  #keycols <- c("ident","dateofonderzoek","registry") 
  setkeyv(x, keycols )
  z <- y[[split_col_name]]
  zz <- ffdf( ids = z)
  xx <-  x[[split_col_name]][]
  idx <- with(zz, ids %in% xx)
  idx <- ffwhich(idx,idx)
  
  if(length(idx) > 0 ) {
    
    #rownames(y) <- NULL  ## to avoid that row.names != NULL  problem... .
    y <-index_ffdf(y,idx)
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

####################################
# merge by is an application of 
# fu_merge, for a big data ffdf. 
# it will split the comput by chunks 
# of size chk. 
####################################

merge_by  <- function(x,y,keycols, split_name,  chk){
  
  chunk_s <- chk # size of the split.  
  res <- split_apply_cpp(x, x[[split_name]],chunk_s ,function(x) fu_merge(x,y,split_name, keycols)  )
}


#### index ffdf ############################################
## will index a ffdf by the index idx. (reorder). 
## avoids memory overflow if one single col is able to go into memory.... 
############################################################

index_ffdf <- function(x_we34574, idx, verbose = F ){
  
  gc()
  col_names <- names(x_we34574)
  N <- ncol(x_we34574); 
  open.ffdf(x_we34574) 
  
  for( i in 1:N) {
    
    name <- col_names[i]
    if(verbose) print(paste( "making row ", name))
    z_14246456457 <- x_we34574[idx, name][[name]]
    ifelse( i==1 , 
            
            df <- ffdf( z_14246456457 ) , 
            df[[name]] <- z_14246456457
    )
    #rm(z_14246456457)
    gc()   
  }
  names(df) <- col_names
  df 
  
  
}

#### index ffdf ############################################
## will index a ffdf by the index idx. (reorder). 
## avoids memory overflow by splitting the computation in several parts.  
############################################################

index_ffdf_split <- function(data_input, idx, nsplits = 1, verbose = F ){
  
  gc()
  col_names <- names(data_input)
  N <- ncol(data_input); 
  open.ffdf(data_input) 
  
  for( i in 1:N) {
    
    name <- col_names[i]
    if(verbose) print(paste( "making row ", name))
    row_end <- 0
    Nrows <- nrow(data_input)
    Nidx <- length(idx)
    split_rows <- ceiling(Nidx/nsplits)
    
    if(nsplits > 1){
      
      for( split in 1:nsplits){
        
        if(verbose) print(paste( ".... split  ", split, "/" , nsplits))
        row_beg <- row_end + 1 
        row_end <- row_end + split_rows
        
        if(row_end > Nidx) row_end <- Nidx
        beg <- as.integer(row_beg)
        end <- as.integer(row_end)
        indxs <- ff(idx[beg:end])
        
        y_45234316 <- data_input[indxs, name][[name]]
        
        ifelse( split==1 , z_45234316 <- y_45234316 , {z_45234316 <- ffappend(z_45234316,y_45234316)}   )
        
      }
      
    }else {
      
      z_45234316 <- data_input[idx, name][[name]] 
    }
    
    ifelse( i==1 , 
            
        df <- ffdf( z_45234316 ) , 
        df[[name]] <- z_45234316
    )
    rm(z_45234316)
    gc() 
    
  }
  names(df) <- col_names
  df 
  
  
}


#### apply ffdf split  ############################################
## will apply a function to a ffdf. 
## avoids memory overflow by splitting the computation in several parts.  
############################################################

apply_ffdf_split <- function(data_input, fu, nsplits = 1, verbose = F )
{
  
  gc()
  col_names <- names(data_input)
  open.ffdf(data_input) 
  
  row_end <- 0
  Nrows <- nrow(data_input)
  Nidx <- Nrows
  split_rows <- ceiling(Nidx/nsplits)
  
  if(nsplits > 1){
    
    for( split in 1:nsplits){
      
     # if(verbose) print(paste( ".... split  ", split, "/" , nsplits))
      row_beg <- row_end + 1 
      row_end <- row_end + split_rows
      
      if(row_end > Nidx) row_end <- Nidx
      beg <- as.integer(row_beg)
      end <- as.integer(row_end)
      indxs <- beg:end
      if(verbose) print(paste( ".... split  ", split, "/" , nsplits, " ", beg, ":", end))
      
      y_45234316 <- data_input[indxs, ]
      
      res_3123 <- fu(y_45234316)
      
      rm(y_45234316)
      
      ifelse( split==1 , z_45234316 <- as.ffdf(res_3123), {z_45234316 <- ffdfappend(z_45234316,res_3123)}   )
      
    }
    
  }else {
    
    y_45234316 <- data_input[,]
    res_3123 <- fu(y_45234316)
    rm(y_45234316)
    z_45234316 <- as.ffdf(res_3123)
    
  }
  
  
  df <- z_45234316
  gc() 
  df 
  
}



#### apply ffdf chunks  ############################################
## will apply a function to a ffdf. 
## avoids memory overflow by splitting the computation in several parts of size chunk_size.  
############################################################

apply_ffdf_chunks <- function(data_, fu, chunk_size = 1e6, verbose = F )
{
  
  gc()
  col_names <- names(data_)
  open.ffdf(data_) 
  
  row_end <- 0
  Nrows <- nrow(data_)
  Nidx <- Nrows
  nsplits <- ceiling(Nidx/chunk_size)
  split_rows <- chunk_size
  
  if(nsplits < 1) nsplits <- 1
  
  if(nsplits > 1){
    
    for( split in 1:nsplits){
      
      #if(verbose) print(paste( ".... split  ", split, "/" , nsplits))
      row_beg <- row_end + 1 
      row_end <- row_end + split_rows
      
      if(row_end > Nidx) row_end <- Nidx
      beg <- as.integer(row_beg)
      end <- as.integer(row_end)
      indxs <- beg:end
      #if(verbose) print(paste( ".... .... rows:   ", beg, " to " , end))
      
      if(verbose) print(paste( ".... split  ", split, "/" , nsplits, " ", beg, ":", end))
      
      y_45234316 <- data_[indxs, ]
      
      res_3123 <- fu(y_45234316)
      
      rm(y_45234316)
      
      ifelse( split==1 , z_45234316 <- as.ffdf(res_3123), {z_45234316 <- ffdfappend(z_45234316,res_3123)}   )
      
    }
    
  }else {
    
    y_45234316 <- data_[,]
    res_3123 <- fu(y_45234316)
    rm(y_45234316)
    z_45234316 <- as.ffdf(res_3123)
    
  }
  
  
  df <- z_45234316
  gc() 
  df 
  
}


#### functions to get the data from split info :: ################

get_data_split <- function(data, chunk_indexes, chuk_number){
  
  beg <- chunk_indexes[chuk_number, "beg_data_index"]; 
  end <- chunk_indexes[chuk_number, "end_data_index"];
  return(data[beg:end, ])
  
}

get_data_table_split <- function(data, chunk_indexes, chuk_number){
  require(data.table)
  beg <- chunk_indexes[chuk_number, "beg_data_index"]; 
  end <- chunk_indexes[chuk_number, "end_data_index"];
  return(data.table( data[beg:end, ]) )
  
}


####################################################################
# split apply over an ORDERED data frame (or ffdf), using the cpp 
# functions get_split_indexes and get_chunks_indexes_for_split_list

##  fu(x) will be something like : 
##  { x <- data.table(x) ; setkey(x, "ident") ; 
##  x[ , list( max.diagnose = max(dignose)), by = ident ]
##  }  for example....
## or any function that gives a data frame operating in each split of the data, 
## that are "respecting" the split-vector (i does not break data for same values of split-vector)
####################################################################

split_apply_cpp <- function(input_data, split_vector, chk_size_ , fu){
  
  require(data.table)
  print(nrow(input_data))
  t.begin <- Sys.time()
  
  data_ <- data.frame( ids = as.integer(split_vector[]) ) # create a data frame with one col which is the split factor as integer...
  
  split_indexes  <- get_split_indexes(data_, "ids") # a cpp function to get the indexes for the splits... 
  print(length(split_indexes))
  
  #the size for each chunk. Setting to 3e6 rows gets good results. No mem overflow.... lower if there are mem. alloc. problems... 
  chk_size <- chk_size_
  if(chk_size > nrow(input_data)) chk_size = nrow(input_data); #print(chk_size)
  
  sp <- get_chunks_indexes_for_split_list(data_, split_indexes, chk_size) # cpp function that gives the row numbers for splits in indexes and in data.base.
  rm(data_) 
  data <- input_data   
  ##print(sp[1:2,])
  Nsplits <- nrow(sp); 
  
  print(paste("Number of Splits " , Nsplits, " / ", "Chunk Size",  chk_size )) 
  
  ########## first process :: using data.table to split ###############
  #we do not need the split_indexes data ::
  rm(split_indexes) ; gc()
  
  ii <- 0 
  
  print("Beginning Splits ------ " )
  for ( i in 1:Nsplits){ 
    
    print(paste("Split ", i, "/", Nsplits))
    t.beg <- Sys.time()
    print(gc())
    split_number <- i
    beg <- sp[split_number, "beg_data_index"]
    end <- sp[split_number, "end_data_index"]
    
    print(paste("---Working on rows  ", beg, "to ", end))
    
    dat <- data[beg:end, ] 
    print(paste("------ function"))
    ## dat <- get_data_table_split(data,sp,split_number)  
    ## setkeyv(dat, c(split_name)) 
    
    res <- fu(dat)   ## fu(x) will be something like : {  x <- data.table(x) ; setkey(x, "ident") ; x[ , list( max.diagnose = max(dignose)), by = ident ] } for example.... 
    #print(nrow(res));
    print(paste("------ append  "))
    
    cont_ <- TRUE 
    ii <- ii+1
    exp <- (is.data.frame(res) & nrow(res) > 0) %in% FALSE 
    #print(exp)
    if(exp) { cont_ <- FALSE ; ii <- ii-1}
    #print(cont_)
    if( ii == 1 & cont_) { result <- as.ffdf(res)}
    if( ii != 1 & cont_) { result <- ffdfappend(result, res)} 
    
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

#####################################################################

#################################################.

#################################################
# lag a vector 
##################################################
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

##################################################
## integer to date with no need of specifying the origin... 
##################################################
asDate <- function(x) {
  
  return ( as.Date(x, origin="1970-01-01"))
  
}


###################################################'
####### numeric to percent with % for labelling: 
###################################################.
# with "dec" decimals  :::  
txt.perc <- function(x, dec){ paste( format ( x, digits = 1, nsmall = dec), "%", sep = "") }
# examples :  #####
# pp <- c(23.121 , 1.34, 0.345); 
# fpp <- txt.perc(pp,2); fpp
# fpp <- txt.perc(pp,1); fpps

##  FUNCTION FILL.VEC ##################
###################################################'
###################################################

## var.l <- 10 ## this is the variable length field... 
## use :: res <- fill.vec(data, fill.char = "X", var.length = 10 )
# fill.vec <- function(x, fill.char = "*",  make.factors = F, var.length = -1 ){
#   
#   filling <- fill.char; 
#   # if it is  numeric we will use filling with zeros. 
#   if(is.numeric(x) || is.integer(x)) filling <- "0"; 
#   if(is.factor(x)) x <- as.character(x);
#   # this will keep out the spaces ## we can replace spaces too for another char,,,,, 
#   #x<- gsub(" ","", x, fixed = T) 
#   #x <- sub(' +$', '', x)  ## spaces only
#   x <- sub('^ +', '', sub(' +$', '', x))  ## spaces only outside the element
#   #repl.char <- "*" # the filling char 
#   #table(x)
#   # we find the maximum number of characters :: 
#   max.char = var.length 
#   if(max.char == -1 ) max.char = max(nchar(as.character(x)),na.rm = TRUE)   
#   # if var.length is not -1, then it will have a fixed length = var.length: 
#   
#   # the vector of diff between the max.char and the nchar ::: 
#   d.nchar <- max.char - nchar(as.character(x));
#   d.nchar[d.nchar < 0]  <- 0 
#   
#   #d.nchar[1:100]
#   #sum(d.nchar)
#   # making the vector of fills ::: 
#   fill <- sapply(d.nchar, function(x) paste(rep(filling, x), collapse=""))
#   #fill[1:10]
#   ## now putting the fills + the values to get fixed length data :: 
#   res <- paste( as.character(x), fill, sep="")  
#   if(make.factors) res <- factor(res)
#   #tab <- as.data.frame(table(res)); tab[1:10,]
#   gc()
#   return(res)
# }


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

# same but make always characters at the end. 

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



## FUNCTION TO FILL ALL A DATA.FRAME ###############'
####################################################'
####################################################

# fill.data.frame <- function(x, fill.char = "*", make.factors = F, var.lengths = NA){   
#   y <- x 
#   vlength <- var.lengths
#   fill <- fill.char 
#   for (i in 1:ncol(x)){
#     
#     ifelse (is.na(vlength),  
#             y[,i] <- fill.vec(x[,i], fill, make.factors), 
#             y[,i] <- fill.vec(x[,i], fill, make.factors, vlength[i]) 
#     ) 
#     
#   }
#   return(y)
# }

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

## to apply the function:::  #####
# 
# f.data <- as.data.frame ( lapply(data, fill.vec, "X") ) 
# head(f.data)

###################################################################
### function to output the time left and the percent done : #####
###################################################################

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
    now_left <- predicted * (1-perc/100); 
    t2.tmp <- paste( "time left" , format(now_left)  )
    t3.tmp <- paste( "total time predicted" , format(predicted)  )
    t4.tmp <- paste(t.tmp, t2.tmp, sep = " // ")
    print( paste( t4.tmp, t3.tmp, sep = " // " ) )
    
    
  }
}

## #####


##################################################
## ffdfsplit ::: a modification of ffdfdply . 
## it will make the computation more clear by outputting time-info  
##################################################

grouprunningcumsum_ <- function (x, max) 
{
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

ffdfget_columnwise_ <-function (x, index = NULL) 
{
  list_to_df <- function(list) {
    rows <- unique(unlist(lapply(list, NROW)))
    structure(list, class = "data.frame", row.names = seq_len(rows))
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
  list_to_df(res)
}

ffdfsplit <-  function (x, split, FUN, BATCHBYTES = getOption("ffbatchbytes"),   RECORDBYTES = sum(.rambytes[vmode(x)]), trace = TRUE, ...) 
{
  begin <- Sys.time() #mio. 
  
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
  
  splitbytable_o <- splitbytable[order(splitbytable, decreasing = TRUE)]
  if (max(splitbytable_o) > MAXSIZE) {
    warning("single split does not fit into BATCHBYTES")
  }
  rm(splitbytable_o)
  
  #m: so now the splits are not ordered by number of same items, by as the factors or dates, of whatever.... 
  
  tmpsplit <- grouprunningcumsum_(x = as.integer(splitbytable), 
                                  max = MAXSIZE)
  nrsplits <- max(tmpsplit) #the number of splits we will do. 
  
  allresults <- NULL
  for (idx in 1:nrsplits) {
    
    beg_ <- Sys.time() #mio
    
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
    
    #message( sprintf("---ffdfget_columnwise_ ") )  # mio
    inram <- ffdfget_columnwise_(x, fltr)
    #message( sprintf("---end  ffdfget_columnwise_") )  # mio
    if (trace) message( sprintf("---beginning FUN call --- length data : %s", nrow(inram) ))  # mio 
    beg_f <- Sys.time()  # mio
    
    result <- FUN(inram, ...)
    gc() #mio ... 
    if (trace) message(paste("---end FUN call: time spent in FUN " ,round( Sys.time() - beg_f )) ) #mio. 
    
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
    
    dif.time <- Sys.time()- beg_ #mio 
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


