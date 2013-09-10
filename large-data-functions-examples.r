######################################

# source and directory :: 
setwd("C:/Users/jkc261/Documents/Projects/Large_data_and_R") # change this directory for your computer.  

source("P:/Scripts/R/functions_miguel.r") # this contains some functions needed .
sourceCpp("P:/Projects/RLargeData/subset_df.cpp") # Rcpp functions.

require(Rcpp); require(data.table); require(ffbase); require(ggplot2); require(grid); require(inline); require(sqldf)


# 1 . MOTIVATION 

# R is an open source programming language that is widely used in statistical computations. 
# More and more researchers and Universities are using R as the main statistical analysis language. 
# R packages ("extra" functionalities) exist for almost any specific task that one would have to use.
# R graphics capabilities are exceptional, and allow  displaying data in almost any customized way. 
# This makes R one of the best options to use in statistical analysis. 

# 2.  THE PROBLEM OF LARGE DATA   

# A. MEMORY : 

# R is using RAM memory to store computed, computing and temporal data. 
# For that reason, RAM overflow errors are common when we use large data in R. 
# There is not only the limitation of the RAM capacity, but also the limitation of contiguous free RAM blocks: 
# to be able to store a vector in R, we need a contiguous free space in RAM memory for that vector. And sometimes, while having still some hundreds of megabytes of 
# free memory, we do not have any contiguous free block bigger that a few megabytes. Then, any large vector will not be able to fit into memory and we will get an error. 

# This memory problem in R is one of the main reasons why R is not used in Large Data Statistical Analysis, and solving this issue would make 
# possible the use of R for many new tasks, universities, research, and new scientific fields. 


# B. SPEED: 

# Dealing with large data needs efficient code to speed up computations. Non efficient code can result in extremly slow processes, 
# that can last hours or days instead of seconds or minutes. 
# So special care must be done to produce efficient code, using native R code or some packages and/or other languages.


# 3. SOLUTION TO LARGE DATA IN R 

# The solution we found to deal with large data in R is a combination of several procedures and new functions. 
# Here we will describe a summary of them. 

# A. Solving memory issues: 

# To solve memory problems we need to write data into disk, instead of loading into RAM memory. 
# The package ff and ffbase allow to do this and still do very efficient code. 

# Some main functions of ffbase were modified or re-coded to allow more efficiency or avoid memory problems, that still exist using the ffbase package. 

# The result is that to solve memory problems we use ffbase + some new functions, that avoid almost all the possible memory problems in R, while still keeping efficient code. 

# B. Speed in Large Data : 

# For large data analysis, speed is an important factor, because the differences between using optimized code or 
# not can make the calculations in R faster in a factor or 10, 100 or 1000 times, making a huge difference in the possible use and workflow with the data. 

# To accomplish a speed-optimized code, we have used 3 main procedures : 

# i . Efficient R code. 

#    Using vectorized functions, efficient built-in functions, etc. This will make the code several orders of magnitude faster than other codes (see details in the tutorial files) 

# ii. Use data.table package for "group by" and split analysis. 

#     data.table is a very efficient package that allows to compute "group by", splits, etc. analysis in a very efficient way. 
#     we use it for almost any "group by" analysis and for other specific tasks. 

# iii. Rcpp: C++ code in R 

# The language C++ is one of the fastests codes that we can create in computers. 
# The package "Rcpp" and "inline" , allow to create C++ code in R in an "easy" way. 
# For some tasks, Rcpp code is thousands of times faster than R code. 
# We have developed some Rcpp functions to work with large data. 
# We have also made some tutorials about how, when, and why use Rcpp into R code. 

# C : combining speed and memory : 

# All those speed-efficient techniques are combined with ffbase to compute tasks into blocks that can fit into RAM memory, write the result in disk, 
# and proceed with the next block of data loaded into RAM. 
# In this way, we obtain the benefits of speed,  avoiding memory problems.  


# 4. COMPUTERS CHARACTERISTICS . 

# Due to this memory problem in R, a good computer to work with R would be one with a good RAM memory, and working in 64bits. 
# From 16GB / 64bits, we have already a good comptuter that will be able to deal with large data. 

# The work we present here is done while working in a Windows 32bits computer with 4gb or RAM, which is almost the "worst" computer we can get nowadays to work with R. 
# Having this computer, we could work in a very efficient way with databases containing dozens of millions records and a few dozens of columns. 
# This means that using the proposed techniques and functions, any computer will be able to deal with large data in R. 
# And this opens a new possibility for the use of R with large data, for example for less rich countries or for the common desktop computers. 


# 5. EXAMPLE : 

# We provide an example that covers the main techniques and packages used in our work with large data sets. 
# It is not a comprehensive list of all the functions and techniques we have done. For more details see the tutorial files and the appendix. 


# check memory and make it bigger :: 

gc()  # this is the "garbage collector" that frees memory and display the memory usage. Use it frequently .

# used (Mb) gc trigger (Mb) max used (Mb)
# Ncells 217522  5.9     407500 10.9   350000  9.4
# Vcells 207013  1.6     905753  7.0   786432  6.0

my_memory <- 4000 # the total RAM memory of the computer (use another number for your own computer). 
memory.limit(size=my_memory)  # put the limit to the computer RAM. This allows R to use all the memory avaiable. 
memory.size() # show how much we are using

# load ffbase :: 
library(ffbase)

# ffbase is working with data almost in the same way as R, but data are written into disk
# and only some parts are loaded into memory to compute. Results are written again into disk. 
# ffdf do not have "character" vectors, but "factor" columns. So if we have some character vector in R, we must convert to factor. 
# Factor levels are loaded into RAM. So if we have, say 4 million levels in a factor vector, those levels will be in RAM. This can cause memory problems. 

# we generate data, with 20 million of rows :: 
# This is what we want to have : 

#    ident    date      diagnose    region 
#   12534   2000-01-23   F           020

# we can think of those data as diagnoses made in hospitals in some country. ident would be the identification number for the subject.

gc() # free some memory
Ndata = 20e6;  # number of observations. 20 million in this example. 

N_ids = Ndata/10 # number of unique idents. 

memory.size() #check memory

ident = factor( sample( 1000:(N_ids+1000), Ndata, replace = T)) # make ident as a sample from 1000 to 20 001 000


memory.size() # see how much is into memory now. 
size <- object.size(ident); print(size , units = "Mb") # ident is taking 145 Mb into RAM memory. 

gc() # free some memory ! 

df <- ffdf( ident = ff(ident))  # create a ffdf database with a column ff( vector). This is written into disk. 

rm(ident) # we do not need it anymore, remember to remove "!!!! 
# check df : 
str(df)  # that gives a complex list of things... 

filename(df) # where is it stored ??? 

# make the dates data :: 

fdates <- ff(sample( 0:10000, Ndata, replace = T) + as.Date("1970-10-01"))
df$date <- fdates  # here we assign that vector to df.... if we change fdates, we will change df$date. This is diff from R. 
head(df)

# check classes :: 

class(df$ident) # ff_vector. 
str(df$ident)   # it is a integer, seen for ff. But it goes into RAM as a factor (ramclass = factor.)

# levels :: 
length( levels(df$ident) ) # how many levels 
levels(df$ident)[1:10] # the first 10 levels. 

# if we subset by indexes, that are not ffvectors, we get a R data.frame (in RAM): 
data <- df[1:10, ] ; data ; class(data)

# but if we subset by ffvectors, we get ffdf (Not in RAM, but in disk) :: 

data <- df[ff(1:10), ] ; data ; class(data)

# this is very important to not to load into RAM while we do computations. !!! 

# make diagnose data :: 

fdiagnose <- ff(sample( letters, Ndata, replace = T )) # this gives an error because we are feeding character... 
fdiagnose <- ff(sample( factor(letters), Ndata, replace = T )) # this is ok, factors... 

gc()
df$diagnose <- fdiagnose
df[1:3,]

# region data :: 
fregion = ff( factor( sample( c("000","013" ,"014", "015", "020" ,"025", "030", "035" ,"040", "042", "050" ,"055" ,"060" ,"065" ,"070" ,"076", "080", "090"), Ndata, replace=T )) ) 
df$region <- fregion
df[1:3,]
gc()


# now let us save the data:: 

setwd("C:/Users/jkc261/Documents/Projects/Large_data_and_R") # change this directory for your computer.  
ffdfsave(df, filename="df")  # this is moving the data from the temporary directory to the one we point to. 

str(df[1,])
# 'data.frame':  1 obs. of  4 variables:

# $ ident   : Factor w/ 1999915 levels "1000","1001",..: 779169
# $ date    : Date, format: "1978-04-13"
# $ diagnose: Factor w/ 26 levels "a","b","c","d",..: 19
# $ region  : Factor w/ 18 levels "000","013","014",..: 17

# to use our own defined functions, we will source (load) them from files (change the path to your own path to those files) : 

require(Rcpp)  # library to code C++ into R. 
require(inline) # library to be able to load c++ functions into R code in the same R file. 

source("P:/Scripts/R/functions_miguel.r") # this contains some functions needed .
sourceCpp("P:/Projects/RLargeData/subset_df.cpp") # Rcpp functions. 

# order the data by ident and dates: 

# we use our defined function: order_ffdf: this function orders the data, avoiding memory problems taking into several splits. Also can verbose the output. 
cols <- c("ident", "date")
df_o <- order_ffdf(df,cols,2,T) # order_ffdf(data= ,order_cols=,splits=,verbose=)

df_first <- subset_ffdf(df_o,c("date"), date < as.Date("1971-10-09")) # our function  subset_ffdf(data=,condition_cols=,condition=,splits=,verbose=)
df_first[1:3,]

# apply a function to the data, splitting the data into blocks of a defined size but respecting same variable values.
# for example, splitting the data into one million rows, but not splitting rows with same "ident", i.e., keep  same idents, into a same split. 
# for use this function, data MUST BE ORDERED by the variable we want to split for. 

# split_apply_cpp(input_data=,split_vector=,chk_size_=,fu=)

# first we create a function to apply in each split. This function must give a data.frame or a data.table : 
fu_date <- function(x){
  
  date <- x$date
  before_1973 <- ifelse(date < "1973-01-01",1L,0L) # ifelse is a vecorized R function. 
  data <- data.frame(before_1973 = before_1973)
  data
  
}

# let us check with some RAM data : 
data <- df_o[1:50,]
fu_date(data)

# apply that to all the data. In splits of 1e6, respecting same idents. So we will have only aprox. 1e6 rows f data into RAM memory. 
# a good way to aprox. know the size of the split is to make some RAM data and see the size : 

data_RAM <- df_o[1:1e6,]
size <- object.size(data_RAM); print(size , units = "Mb") # 122 Mb. then we could take as much as 3e6 perhaps.. (I take around 1/10 of total ram as a safe indicator)
rm(data_RAM)

chunk_size <- 3e6 
result <- split_apply_cpp(df_o,df_o$ident, chunk_size ,fu_date) # 40 seconds.  # remember data MUST be ordered by the split variable ("ident" in this example). 

gc() # free memory

levels(df_o$diagnose)

# now, let us apply a more complex function: we want to know for each ident, if they were diagnosed with code "a", "b", or "c". 
# then we need to make a split-apply-combine procedure. 

# To make group_by tasks we will use data.table, which is very efficient. 

library(data.table)

# our function (we think we apply this to data with same "ident")

get_diag_abc <- function(x){ any(x %in% c("a","b","c"))  } # gives TRUE if there is "a","b" or "c" in the values of x. 

# a test: 
diag <- df_o$diagnose[1:30] # the first 30 diagnoses. 
get_diag_abc(diag)  # result. ok. 

# now we want to apply that to data : 

fu_diagnose_abc <- function(x){
  
  x <- data.table(x)                 # make a data.table.
  setkeyv(x, c("ident","date"))      # index (order) by ident and date . 
  y <- x[,  list(diagnose_abc = get_diag_abc(diagnose)), by = ident]   # make a vector diagnose_abc as the any(---) for each ident.
  y 
}

#test: 
data <- df_o[1:1000,]
res <- fu_diagnose_abc(data); res[1:20] # ok, works. 
#     ident diagnose_abc
# 1:  1000        FALSE
# 2:  1001        FALSE

# proceed to calculate that for all the 20 million data : 

chunk_size <- 3e6 
result <- split_apply_cpp(df_o,df_o$ident, chunk_size ,fu_diagnose_abc) # 2 minutes.  # remember data MUST be ordered by the split variable ("ident" in this example).

# the resulting data base is a ffdf object: it is written into disk and does not load into RAM memory. 
gc()

# merging data: 
# now we want to merge that result and the previous data. It is a merge as when in SQL we are merging in this way: 
# WHERE t1.ident = t2.ident , 
# i.e. we only have as results the row which have values in EACH of the databases. We eliminate rows with values NOT in both data. 
cols <- c("ident")
split <- c("ident")

merged <- merge_by(df_o,result,cols,split,1e6) # 5 minutes . # merge_by(x=,y=,keycols=,split_name=,chk=)

dim(df_o)
dim(result)
dim(merged)

# note that we have done the split and the subsequent merge. Better to do that in data.table in one single function: 

fu_diagnose_abc_2 <- function(x){
  
  x <- data.table(x)                 # make a data.table.
  setkeyv(x, c("ident","date"))      # index (order) by ident and date . 
  y <- x[,  diagnose_abc := get_diag_abc(diagnose), by = ident]  # now add that column. 
  y 
}


chunk_size <- 3e6 
result_2 <- split_apply_cpp(df_o,df_o$ident, chunk_size ,fu_diagnose_abc_2) # 3.4 minutes.  # remember data MUST be ordered by the split variable ("ident" in this example).

result_2[1:2,]
# ident          date diagnose region diagnose_abc
# 1   1000 1973-01-30        l    000        FALSE
# 2   1000 1974-10-16        e    070        FALSE


# another way to make split-apply task is to use a loop over data. 
# loops over rows are almost forbidden in R, because they are EXTREMLY slow. 
# But in Rcpp, loops over the rows are very fast. 

# here is an example of Rcpp code that is doing the equivalent of our previous code:
# As in the previous code, data MUST be ordered by "ident" before applying the function. 

require(inline)
require(Rcpp)

src <- '
  #include <string>
  #include <iostream>   //this will allow == between strings... 

  using namespace Rcpp;
  using namespace std;

  IntegerVector ident = as<IntegerVector>(ident_);
  CharacterVector diagnose_v = as<CharacterVector>(diagnose_) ;  //

  int N = ident.size();

  IntegerVector diagnose_abc(N,0);
  IntegerVector ident_idx(N,0);

  int NewId = 0; 
  int OldId = -1; 

  int ident_index = 0; 

  int diagnose_abc_int = 0; 

for (int i = 0; i < N; i++){
    
    NewId = ident[i];
    string diagnose = as<string>(diagnose_v[i]);
    //printf("diagnoses abc: %s", diagnose );

    if(NewId == OldId){
     
      ident_index += 1;
      ident_idx[ident_index] = i;

      if( diagnose == "a" || diagnose == "b" || diagnose == "c"  ) {

        diagnose_abc_int = 1; 
        //cout << "diagnoses abc: " <<  i << endl; 

      }

      // last case :: do not forget the last one if it is the same ident as the previous.

       if( i == (N-1)) { 
         
           if(diagnose_abc_int == 1) {  // if we get the result . 
              
              for( int k=0; k <= ident_index; k++) {

                 diagnose_abc[ ident_idx[k] ] = 1;  // all the diagnose_abc for that ident are going to be 1. 
                 //printf("diagnoses abc: %d", ident_idx[k] );
              }
           } 

       }

    } else {
      
       if(diagnose_abc_int == 1) {  // if we get the result . 

         for( int k=0; k <= ident_index; k++) {

            diagnose_abc[ ident_idx[k] ] = 1;  // all the diagnose_abc for that ident are going to be 1. 
            //printf("diagnoses abc: %d", ident_idx[k] );
         }

       }

       ident_index = 0; 
       ident_idx[ident_index] = i; 
       diagnose_abc_int = 0; 

       if( diagnose == "a" || diagnose == "b" || diagnose == "c"  ) {

          diagnose_abc_int = 1; 

       }

       OldId = NewId; 

    }

    
   
  }

  return diagnose_abc; // return the vector of results... 

  '

fun <- cxxfunction(signature(ident_ = "integer",diagnose_ = "integer"), src, plugin = "Rcpp") # defines the function....
gc()

ident <- df_o$ident[1:1000]
diagnose <- df_o$diagnose[1:1000]
res <- fun(ident, diagnose )
res
data <- data.frame(ident, diagnose, res)
data[1:50,]

# so the function is working and is extremly fast : 

chunk_size <- 5e6 # try with 5 million rows, and check if we do not have memory problems.

fun_diagnose <- function(x){
  
  res <- fun(x$ident,x$diagnose)
  res <- data.frame(diagnose_abc = res)
  res 
  
}
result_3 <- split_apply_cpp(df_o[c("ident", "diagnose")],df_o$ident, chunk_size ,fun_diagnose) # 40 seconds. The main computing time is used in append data and convert to ffdf. 
                                                                                               # remember data MUST be ordered by the split variable ("ident" in this example).

# as C++ is very efficient, we could also try to get the function with the data in RAM : 
gc()
ident <- df_o$ident[]  # load ident data into RAM. 
diagnose <- df_o$diagnose[] # load diganose data into RAM. 

system.time(result4 <- fun(ident,diagnose) )  # 3 seconds !!! 

diag_abc <- as.integer(result_2$diagnose_abc[])

identical(diag_abc, result4)  # they are identical ! 
rm(ident,diagnose, diag_abc)

gc()

### This was an example of the different timings we can get using efficient code in R. 
### The same computation as above, using aggregate, or plyr package, etc. makes the process longer than 30 minutes. 
### Comparing our data.table + ffdf computation (3.4 minutes) or better our 3.2 seconds for Rcpp, we see that the order of magnitude of the improvements in speed we can gain with 
### efficient coding can be of the order of 1600 / 3 = 600 times. (from 3 seconds to 30 minutes).
### If the data were larger, we would have a much bigger difference, sometimes making some "simple" tasks impossible for very large data sets if we do not use efficient code. 



############### APENDIX ##############################


# SUMMARY OF CUSTOM FUNCTIONS  :: 

# code to load the functions. Change directories to point to your local files. 

require(Rcpp)  # library to code C++ into R. 
require(inline) # library to be able to load c++ functions into R code in the same R file. 

source("P:/Scripts/R/functions_miguel.r") # this contains some functions needed .
sourceCpp("P:/Projects/RLargeData/subset_df.cpp") # Rcpp functions.



################################################
############### order_ffdf #####################
################################################

# order_ffdf(data=,order_cols=,splits=,verbose=) 
# will order a ffdf by the order column names, using splits. Can verbose the process. 
# Using splits, can avoid memory problems found when using ffdforder or ffdfsort. 

# example: 

data <- ffdf( x = ff(sample(1:10)), y = ff(factor(sample(letters, 10, replace = T))), z = ff(1:10))
data[1:3,]
#     x y  z
# 1   3 a  1
# 2   1 q  2
# 3  10 b  3

cols <- c("x","z")
res <- order_ffdf(data, cols,2,T)
res[1:3,]
#   x y z
# 1 1 q 2
# 2 2 o 5
# 3 3 a 1

################################################

################################################
############### subset_ffdf ####################
################################################

# subset_ffdf(data=,condition_cols=,condition=,splits=,verbose=)

# subset a ffdf, by the condition columns, with a condition. Makes splits if needed and verbose. 

# example: 
cols <- c("z","y")
data[1:3,]
#    x y z
# 1  3 a 1
# 2  1 q 2
# 3 10 b 3

res <- subset_ffdf(data,cols, (z %in% c(1,2,7) ) |  ( y %in% c("a","b","u")  )     ) 
res[,]
#    x y z
# 1  3 a 1
# 2  1 q 2
# 3 10 b 3
# 4  8 a 6
# 5  6 y 7


################################################
################# cbind_ffdf ###################
################################################


# cbind_ffdf(x=,y=)

# will bind columns of y to columns of x . 
# obs: if not cloned, the binded cols will still be the cols of y. 
# and then if saved without cloning, y will loose all its columns. 

# example : 

data_2 <-  ffdf( x2 = ff(sample(10:19)), y2 = ff(factor(sample(LETTERS, 10, replace = T))), z2 = ff(10:1))
data_2[1:3,]
#   x2 y2 z2
# 1 19  L 10
# 2 11  Y  9
# 3 12  W  8

res <- cbind_ffdf(data,data_2)  # will bind the columns of data_2 to the cols of data. 

res[1:5,]
#    x y z x2 y2 z2
# 1  3 a 1 19  L 10
# 2  1 q 2 11  Y  9
# 3 10 b 3 12  W  8
# 4  9 f 4 13  E  7
# 5  2 o 5 14  G  6

filename(res$x2)
filename(data_2$x2)  # they have the same name, so we need to clone if we do not want to move those columns. 
res <- clone(res)
filename(res$x2)   # a different filename. Ready to save without moving data from data or data_2 


################################################
################## append_ffdf  ################
################################################


append_ffdf(x=,y_=,chunk_s=)

# will append two ffdf using chunks from the second ffdf. 
# used mostly when ffdfappend did not append two ffdf but only one ffdf with one data.frame. 
# now, we prefer the use of ffdfappend, which is eqivalent to this one. 
# THis function can be useful when memory conditions get difficult. 
# It has also the feature to display info about the total estimated time, remaining, etc. 

# example: 
data_3  <- ffdf( x = ff(sample(1:10)), y = ff(factor(sample(letters, 10, replace = T))), z = ff(20:29))

res <- append_ffdf(data,data_3,4)

# [1] " done split  1 / 3 , row_end  4  spent :  0 , E. total  0 , E. remain   0"
# [1] " done split  2 / 3 , row_end  8  spent :  0 , E. total  0 , E. remain   0"
# [1] " done split  3 / 3 , row_end  10  spent :  0 , E. total  0 , E. remain   0"
# [1] " Total time : 0.1"
dim(res) # 20  3

# OBS: as with ffdfappend, we are adding data to the first ffdf. So the result will be the same filenames as the first ffdf: 

filename(res$x) == filename(data$x) # TRUE. 

# if we want to get a new ffdf, we must clone the first ffdf: 

res <- append_ffdf(clone(data),data_3,4)
filename(res$x) == filename(data$x) # FALSE. Now we can save without moving files from "data".
################################################



################################################
############### merge_by #######################
################################################

# merge_by(x=,y=,keycols=,split_name=,chk=)

# merge_by is used to merge two ffdf, in an efficient way. Data from the first ffdf must be ordered by the split variable. 
# the merge is done over "keycolumns" and only the data that have the same combination of keycolumns values will be retained. 
# This is a merge as in SQL in the code : WHERE t1.ident = t2.ident , etc. 
# merge_by is using a merge with data.table in this way: 

# merged <- y[x, nomatch = 0]  

# and then the "no matching" records are NOT included in the result. 

# example: 
N <- 1e5 
n <- N/2 
dat  <- ffdf( x = ff(sample(1:N)), y = ff(factor(sample(letters, N, replace = T))), z = ff(1:N))
dat2  <- ffdf( x = ff(sample(1:n)), y2= ff(factor(sample(LETTERS, n, replace = T))))

dat[1:5,]

#       x y z
# 1  7167 t 1
# 2 28861 x 2
# 3 60652 z 3
# 4 82511 z 4
# 5 73610 p 5

dat2[1:5,]

#       x y2
# 1 29099  R
# 2 22323  O
# 3 26713  K
# 4  3909  A
# 5 23463  N

keycols <- "x"
split_col  <- "x"

# data MUST be ordered, the first one: 
dat <- order_ffdf(dat, "x")

res <- merge_by(dat, dat2, keycols,split_col,N/3) 
res[1:5,]
#   x y2 y     z
# 1 1  O q 94492
# 2 2  C d 99832
# 3 3  F o 28827
# 4 4  K b 19108
# 5 5  C n 28736
dim(res); dim(dat); dim(dat2)
################################################

################################################
############## index_ffdf ######################
################################################


# index_ffdf(x=,idx=,verbose=)

# function to index a ffdf . 
# idx is a ff vector with the indexes. 
# ffdforder gives memory problems because of the use of ffdfget_columnwise. 
# to avoid those problems, we can use index_ffdf or for larger data: index_ffdf_split. 

# example: 
N <- 1e6 
n <- N/10
dat  <- ffdf( x = ff(sample(1:n, N, replace = T)), y = ff(factor(sample(letters, N, replace = T))), z = ff(1:N))

idx <- ffdforder(dat[c("x","z")])

res <- index_ffdf(dat, idx, T)
res[1:10,]
#    x y      z
# 1  1 l 134639
# 2  1 l 182189
# 3  1 c 196719
# 4  1 f 267096
# 5  1 r 353421
# 6  1 m 454636
# 7  1 c 455921
# 8  1 b 585642
# 9  1 e 587494
# 10 1 m 661377

idx <- ff(sample(1:1e5))  # take 1e5 random indexes. 
res <- index_ffdf(dat, idx, T)  # get the database of 1e5 random rows. 
res[1:5,]
#       x y     z
# 1 12198 q  8479
# 2 81870 x 82379
# 3 43026 z 51030
# 4 86046 p 29775
# 5  8335 v 37094

################################################


################################################
#################### index_ffdf_split ##########
################################################


# index_ffdf_split(x=,idx=,nsplits=,verbose=)

# function to index a ffdf . 
# idx is a ff vector with the indexes. 
# ffdforder gives memory problems because of the use of ffdfget_columnwise. 
# to avoid those problems, we can use index_ffdf or for larger data: index_ffdf_split. 
# using several splits we get into memory smaller blocks of data. 
# this allows to index huge data bases with no memory overflows... 


# example: 
N <- 1e7  # 10 million rows
n <- N/10
dat  <- ffdf( x = ff(sample(1:n, N, replace = T)), y = ff(factor(sample(letters, N, replace = T))), z = ff(1:N))

idx <- ffdforder(dat[c("x","z")])

res <- index_ffdf_split(dat, idx,3, T)

res[1:10,]
#    x y       z
# 1  1 b 1045064
# 2  1 x 4055519
# 3  1 x 4936608
# 4  1 l 6032727
# 5  1 k 6798082
# 6  1 k 6903526

################################################


################################################
################# apply_ffdf_split #############
################################################

# apply_ffdf_split(x=,fu=,nsplits=1,verbose=F)

# will apply a function fu to a ffdf. 
# avoids memory overflow by splitting the computation in several parts.
# fu must give a result as a data.frame, and operates as in "R" objects. 
# does not need any ordering in the ffdf.

# example: 
N <- 1e7  # 10 million rows
n <- N/10
dat  <- ffdf( x = ff(sample(1:n, N, replace = T)), y = ff(factor(sample(letters, N, replace = T))), z = ff(1:N))

sum10 <-  function(x){ data.frame( z10 = 10 + x$z)} 

res <- apply_ffdf_split(dat,sum10, 4, T)

res[1:10,]
class(res)
# [1] 11 12 13 14 15 16 17 18 19 20
# "ffdf"   

################################################


################################################
################# apply_ffdf_chunks ############
################################################

# apply_ffdf_chunks(data_=,fu=,chunk_size=,verbose=)

# will apply a function to a ffdf. 
# avoids memory overflow by splitting the computation in several parts of size chunk_size.
# does not need any ordering in the ffdf. 

# example: 
N <- 1e7  # 10 million rows
n <- N/10
dat  <- ffdf( x = ff(sample(1:n, N, replace = T)), y = ff(factor(sample(letters, N, replace = T))), z = ff(1:N))

values <-  function(x){ data.frame( values = x$z + x$x, rest = x$z - x$x/2. ) } 

res <- apply_ffdf_chunks(dat,values, 2e6, T)


res[1:10,]
#    values rest
# 1  482437    1
# 2   21008    4
# 3  487840    9
# 4  235405   16
# 5  693192   25
# 6   49780   36
# 7  873622   49
# 8  875988   64
# 9   23761   81
# 10 514730  100


################################################
################# split_apply_cpp ##############
################################################


#split_apply_cpp(input_data=,split_vector=,chk_size_=,fu=)

# split_apply_cpp will take a ffdf which MUST be ordered by the split vector, and will compute the function fu over the splits of size chk_size, RESPECTING the same split_vector 
# values. That means that when making splits, we will not break the data where same values of the split vector are. Same values of the split vector will be in one single split. 
# This allows to use this function with some group_by functions, over large data sets.  

# function fu must operate over the database input_data as a data.frame (when it is loaded in memory, not as ffdf). 

N <- 1e7  # 10 million rows
n <- N/10
dat  <- ffdf( x = ff(sample(1:n, N, replace = T)), y = ff(factor(sample(letters, N, replace = T))), z = ff(1:N))

# order the data by the split vector !!!! 
res <- order_ffdf(dat,"x")
res[1:5,]

#   x y       z
# 1 1 f  179992
# 2 1 b  938613
# 3 1 d 3160421
# 4 1 z 6188453
# 5 1 d 7054255

fun.900000 <- function(x){
  
  zz <- x$z 
  res <- ifelse(zz > 900000, 1, 0)
  res <- data.frame(g.than.900000 = res)
  res # we return a data.frame.  
}

# test to apply over a sample: 
df <- dat[1:10,]; fun.900000(df) # ok. 

# apply to all the data: 

chuk_size = 1e6 
res <- split_apply_cpp(res,res$x,chuk_size,fun.900000) # 20 seconds for 10 million rows.



################################################

##########################################################
#        SOME UTILITIES     ##############################
##########################################################



################################################
###### REDIR_FFDF ##############################
################################################

# redir_ffdf(ffdf=,newdir= ) 

# will redefine a directory for a ffdf database which was saved in a directory and moved manually to the present one.   
# This function will allow to avoid the error filename == 0 , allowing the use of the ffdf again. 
# This function works for data saved with save.ffdf within the SAME directory name as the name of the database.  



# example: 
setwd("C:/Users/jkc261/Documents/Projects/Large_data_and_R")
data_redir <- ffdf( one = ff(1:10), two = ff(11:20))
save.ffdf(data_redir, dir = "data_redir")  # OBS: saved data to a directory named SAME as the name of the database.

# now suppose that we move manually (not using R) the files into the directory "P:/Projects/RLargeData/"
# to ba able to work with the data again, we will do this: 

setwd("P:/Projects/RLargeData/")  # change the directory. 
load.ffdf("data_redir")           # load the database
data.p <- redir_ffdf(data, "P:/Projects/RLargeData")  # change the path names of the files to correct for the move. 
data.p  # now we can use it without access errors. 

################################################


################################################
###### redir_ffdf_ffdfsave  ####################
################################################


#redir_ffdf_ffdfsave(ff=,newdir=)   

# will redefine a directory for a ffdf database which was saved in another directory and moved manually to the present one.   
# This function will allow to avoid the error filename == 0 , and we will be able to use the ffdf again. 
# This function works for data saved with ffdfsave with the SAME file name as the name of the database

# example: 

setwd("C:/Users/jkc261/Documents/Projects/Large_data_and_R")
data_r <- ffdf( one = ff(1:10), two = ff(11:20))
ffdfsave(data_r, file = "data_r")   # saved data to a file named SAME as the name of the database.

# now suppose that we move manually (not using R) the files into the directory "P:/Projects/RLargeData/"
# to be able to work with the data again, we will do this: 

dirP <- "P:/Projects/RLargeData"
setwd(dirP)  # change the directory. 
load("data_r")           # load the database
data.p <- redir_ffdf_ffdfsave(data_r, dirP)  # change the path names of the files to correct for the move. 
data.p  # now we can use it without access errors. 
################################################



################################################
############## is_ordered_by_row_col ###########
################################################


# is_ordered_by_row_col(data=,col_name=) 

# will give TRUE if in a ffdf database, a column named col_name is like the row numbers. 
# That column MUST be in the form 1,2,3,4,5,.... N , as if it was the row indexes in order. 

# example: 

data <- ffdf( x = ff(1:100), y = ff(sample(1:2354, 100) ) ) # make a ffdf database. 
data[1:10,]

is_ordered_by_row_col(data,"x") # TRUE 
is_ordered_by_row_col(data,"y") # FALSE 

# this function is used frequently when making split-apply-combine using data_table and keeping a "reference" column as the row number. 
# The function will verify if we need to reorder the result to make a new column as the result. 

################################################


################################################
########## quote.names , qn  ###################
################################################


quote_names()                       # will make a vector of the names that appear in the expression. Very useful. 
qn()                                # abbreviation of the same function. 

# example: 

quote_names(ident,oyo,pea, fre, wer, q_32rer)
# gives: 
# "ident"   "oyo"     "pea"     "fre"     "wer"     "q_32rer"

qn(ident,region,ale) #  "ident"  "region" "ale"   

# application for data: 
data <- data.frame( x= 1:10, y = letters[1:10] , ww = LETTERS[1:10], zz = letters[10:1], aa = 20:11)
data[1:5,]

sub <- data[qn(x,y,zz)]  # we use qn(x,y,zz) to specify the column names... same as c("x","y","zz") which is longer to write down. 
sub[1:5,]

################################################


################################################
################# lag_pad         ##############
################################################

# lagpad(x=)

# will insert a NA value at the beginning of the vector x, keeping the vector length (removing the last item)

x <- 1:20 
lagpad(x)
x <- factor(letters[1:20]); lagpad(x)


################################################
################# asDate          ##############
################################################

# asDate(x=)

# will take a number x and transform it to a date, with origin 1970-01-01 (R default)

asDate(0) # "1970-01-01"
asDate(2000) # "1975-06-24"
asDate(5034) #  "1983-10-14"

################################################
################# txt.perc        ##############
################################################

txt.perc(x=,dec=)

# will tranform x (a number) to a string of percent with "dec" precision. 
# Useful for labelling graphics. 

txt.perc(0.456,2) # "0.46%"
txt.perc(23.67334,1) #  "23.7%"
txt.perc(99.45,0) # " 99%"

################################################
################# fill.vec        ##############
################################################


#fill.vec(x=,fill="*")

# will fill a vector "x" with characters "fill", to the maximum length of the items in x.  
# In integer or numeric cases, it will add "0" at the beginning of the vector. 
# In other cases will add "fill" at the end. 
# It will delete blank spaces at the beginning and at the end of the items. But will respect the "inner" blank spaces. 
# Used to export data bases as fixed column data with custom fills, to easy import in SQL, etc. 


x <- c(123,45,01,34567)
fill.vec(x)  # "00123" "00045" "00001" "34567"

x <- factor( c("123", "AQw", "0"))
fill.vec(x)  # "123" "AQw" "0**"
fill.vec(x,"@") # "123" "AQw" "0@@" 

x <-  c("AD  AN   ", "A Qw  ", "EDEN ")
fill.vec(x) #  "AD  AN" "A Qw**" "EDEN**"
fill.vec(x,"_") # "AD  AN" "A Qw__" "EDEN__"


################################################
############## fill.vec.all.same  ##############
################################################

# fill.vec.all.same(x=,fill="*") 

# same as fill.vec, but will fill all the vectors with end character fills (as if they were not numeric). 

x <- c(123,45,01,34567) 
fill.vec.all.same(x)     # "123**" "45***" "1****" "34567"
fill.vec.all.same(x,"_") # "123__" "45___" "1____" "34567" 

################################################
############## fill.data.frame    ##############
################################################


# fill.data.frame(x=,fill.char="*",make.factors=FALSE)

# Will fill all the columns of a data frame, as fill.vec is doing. 
# In case make.factors = T , it will make all the colums as factors (to be used with ffdf)
x <- c(12,45,01,1209767)
y <- c("hi","hello", "goodbye", "not")
df <- data.frame(x,y)
df
#         x       y
# 1      12      hi
# 2      45   hello
# 3       1 goodbye
# 4 1209767     not

fill.data.frame(df,"^")
#           x       y
#   1 0000012 hi^^^^^
#   2 0000045 hello^^
#   3 0000001 goodbye
#   4 1209767 not^^^^



################################################
######    fill.data.frame.all same    ##########
################################################


x <- c(12,45,01,1209767)
y <- c("hi","hello", "goodbye", "not")
df <- data.frame(x,y)
fill.data.frame.all.same(df,"_")
#          x       y
# 1 12_____ hi_____
# 2 45_____ hello__
# 3 1______ goodbye
# 4 1209767 not____



###############################################
#  PREFORMANCE COMPARISON 
###############################################



# We compute the same task following different methods and finding their computational duration. 

# Task 1: 
# in a large database, label records whith  date_min <= date < date_max and diagnose == "a" or "b" or "c" as "positive" and others as "negative". 

# Task 2: 
# in a large database, for each ident, find the number of cases with diagnoses "d" , "e" or "f". If n == 0 label case as "none", if 0< n <= 10 label = few , others: label = many. 


# 1- build the database:

# first, remove from memory all the variables: 

rm(list = ls())
gc()

# source and directory :: 
setwd("C:/Users/jkc261/Documents/Projects/Large_data_and_R") # change this directory for your computer.  

source("P:/Scripts/R/functions_miguel.r") # this contains some functions needed .
sourceCpp("P:/Projects/RLargeData/subset_df.cpp") # Rcpp functions.

require(Rcpp); require(data.table); require(ffbase); require(ggplot2); require(grid)

# make the data :: 

gc() #   memory
Ndata = 20e6;  # number of observations. 20 million in this example. 
N_ids = Ndata/10 # number of unique idents. 
ident = factor( sample( 1000:(N_ids+1000), Ndata, replace = T)) # make ident as a sample from 1000 to 20 001 000
df <- ffdf( ident = ff(ident))  # create a ffdf database with a column ff( vector). This is written into disk. 
rm(ident) 
fdates <- ff(sample( 0:10000, Ndata, replace = T) + as.Date("1970-10-01"))
df$date <- fdates  # here we assign that vector to df.... if we change fdates, we will change df$date. This is diff from R. 
fdiagnose <- ff(sample( factor(letters), Ndata, replace = T )) # this is ok, factors... 
df$diagnose <- fdiagnose
fregion = ff( factor( sample( c("000","013" ,"014", "015", "020" ,"025", "030", "035" ,"040", "042", "050" ,"055" ,"060" ,"065" ,"070" ,"076", "080", "090"), Ndata, replace=T )) ) 
df$region <- fregion

str(df[1,])
df[1:3,]

# export data to use with SAS :: 
write.table.ffdf(df, file = "data.csv",quote= F, sep=",", row.names = F) # approx. 500 Mb. 



# TASK 1 : 

# label records whith  date_min <= date < date_max and diagnose == "a" or "b" or "c" as "positive" and others as "negative"

# A. loop over the rows of the database. 
# B. mapply 
# C. vectorized function
# D. data.table
# E. Rcpp 


# A. LOOP OVER THE ROWS (this is an almost forbidden approach, because it is extremely slow)

# take a 1 million db (avoiding the use of ffdf, and clarifying the results)

idx <- sample(1:1e6)
open.ffdf(df)
data <- df[idx,]
data[1:10,]
asDate(max.ff(df$date))
asDate(min.ff(df$date))

N <- nrow(data)

# make a test database to avoid  too long computations: 

test <- data[1:1e5,] #  100 000 rows 
res <- rep(as.character(NA),nrow(test))

date_min <- as.Date("1995-01-01")
date_max <- as.Date("1998-01-01")

test$date[10] > date_min & test$date[10] <= date_max & test$diagnose[10] %in% c("a","b","c")

class(test$date)

cond_date <- function(x,y){
ifelse(x > date_min & x <= date_max & y %in% c("a","b","c") , "positive", "negative"  ) 
}

cond_date(test$date[10:20], test$diagnose[10:20])

#  A loop over the rows :: 

begin.t <- Sys.time()
for ( i in 1:nrow(test) )
  {
  res[i] <- cond_date(test$date[i], test$diagnose[i])
}
print(paste("time spent loop : ", Sys.time()- begin.t)) 


#   1.1 minutes for 1e5 rows . Expected 11 minutes for 1e6, and 220 (almost 4 hours) for 20e6 rows.   
time_loop_rows <- 1.088 * 60


# B - using apply, mapply, sapply : better, but still can be slow : 

begin.t <- Sys.time()
res<- mapply(
  function(x,y){ ifelse( cond_date(x,y) ) }
  ,test$date,
   test$diagnose
)
print(paste("time spent mapply : ", Sys.time()- begin.t)) #  1.095 minutes 

time_mapply <- 1.095 * 60 

# B2. using apply over the rows :: 

begin.t <- Sys.time()
res <- apply(test[c("date", "diagnose")], 1 , function(x){  cond_date(x[1],x[2]) }  )
print(paste("time spent apply : ", Sys.time()- begin.t)) # 27.64 seconds 

time_apply <- 27.64

# C - vectorize the computation :

begin.t <- Sys.time()
res <- vector("character", nrow(test))
res[] <- "negative"
idx <- with(test, cond_date(date,diagnose))
res[idx] <- "positive"
print(paste("time spent vectorized : ", Sys.time()- begin.t)) # 0.0600008964538574

res[1:10]
time_vectorized <- 0.060 



# D - use packages as data.table, sqldf, etc : 

require(data.table)
DT <- data.table(test); 
begin.t <- Sys.time()
res <- DT[ , list( valid = cond_date(date,diagnose)) ]
print(paste("time spent data.table : ", Sys.time()- begin.t))   # 0.0500

time_data_table <- 0.050 

# E- Rcpp: 
require(Rcpp); require(inline)

src <- '
  #include <string>
  #include <iostream>   //this will allow == between strings... 

  using namespace Rcpp;
  using namespace std;

  IntegerVector dates(date_);
  CharacterVector diagnose = as<CharacterVector>(diagnose_);
  int date_min = as<int>(date_min_); 
  int date_max = as<int>(date_max_);

  int N = dates.size();

  CharacterVector result(N, "negative");

for (int i = 0; i < N; i++){
    
    //string diagnose = as<string>(diagnose[i]);
    int date = dates[i]; 

    if( (diagnose[i] == "a" || diagnose[i] == "b" || diagnose[i] == "c") && date <= date_max && date > date_min ) {

      result[i] = "positive"; 
    } // else result[i] = "negative";

  }

  return result; // return the vector of results... 

  '

fun <- cxxfunction(signature(date_ = "integer",diagnose_ = "integer", date_min_ = "integer", date_max_ = "integer"), src, plugin = "Rcpp") # defines the function....


test <- data[1:3e6,]

system.time(res <- fun(test$date, test$diagnose, date_min,date_max) )  #  0.25 for 3e6 : gives 0.00833 for 1e5 . 
0.25 / 3e6 * 1e5 #  0.00833

time_Rcpp <- 0.0083 


####### RESULTS ####### 
methods <- c("loop rows", "mapply", "apply", "vectorized", "data.table", "Rcpp")
method <- factor(methods, levels = methods, ordered = T)
time <- c(time_loop_rows,time_mapply,time_apply,time_vectorized,time_data_table,time_Rcpp)
time_20e6_sec <- time * 10 * 20 
time_relative <- round( time / time_Rcpp ) 
time_20e6_sec / 3600
time_20e6 <- c(" 3.62 hours", "3.65 hours", "1.5 hours", "12 seconds", "10 seconds", "1.7 seconds" )

speed <- data.frame( method , time , time_relative, time_20e6_sec, time_20e6 )

speed

## graph the results :: 


plot <- ggplot( speed,aes(x=method, y = time_relative,fill = method ,group= 1 )   ) 
plot <- plot +
         geom_bar(stat="identity") + 
         xlab('computing methods')  +  
         ylab('relative computing time')+ 
         ggtitle("Relative computing times for different methods in R  \n over a 20 million row data frame ") 

plot2 <- plot + 
      geom_text(data=speed,aes(x=method,y=time_relative,label= time_20e6),vjust=-0.5, size = 4)  + 
      theme( 
         plot.title=element_text(size=16, vjust = 1) 
         ,legend.position=c(0.82,0.67) 
         ,axis.title.x  = element_text(vjust=-1, size = rel(1.2))
         ,axis.title.y  = element_text(angle=90, vjust=0, size = rel(1.2))
         ,plot.margin=unit(c(1, 1, 1, 1), "lines")
        )

plot2
  
setwd("C:/Users/jkc261/Documents/Projects/Large_data_and_R") # change this directory for your computer.  
### saving to tiff :: 
tiff(file = "speed.tif",width = 7, height = 5, units = "in", res = 500, compression="lzw")
#pdf(file = "P:/HorizonData/Bar Plots/BarPlotsAptima.pdf",width = 10, height = 7.5)
plot2
dev.off()


### applying this to the full data (20 million rows), will need for some computers, go into ffdf. 
### Here we compare different methods to apply in ffbase and custom functions. 

class(df) # "ffdf" 


cols <- qn(date,diagnose)
test <- df[cols]

### use with() + function vecorized :: 

system.time(
res <- with(test, factor(cond_date(date, diagnose))) # this will make the calculus in RAM blocks that can fit into memory.
)  # 38.46  secs. 

# res <- cond_date(df$date[], df$diagnose[])  # All into RAM !!! 
#  The command above gives and error:: 
# Error: cannot allocate vector of size 76.3 Mb

dim(test)
class(res)


date_min <- as.Date("1995-01-01")
date_max <- as.Date("1998-01-01")

cond_date <- function(x,y){
  ifelse(x > date_min & x <= date_max & y %in% c("a","b","c") , "positive", "negative"  ) 
}


cond_date_df <- function(x){
  
  zz <- factor(cond_date(x$date,x$diagnose))
  zz <- data.frame(result = zz)
  zz 
  
}


load("df")
df[1,]; dim(df)
class(df)

dfo <- order_ffdf(df, "ident") # order the data before going into split_apply_cpp ::

cols <- qn(ident,date,diagnose)
test <- dfo[cols]

### SPLIT_APPLY_CPP : 
res <- split_apply_cpp(test, test$ident,3e6,cond_date_df) #  47.4 seconds 


### FFDFSPLIT : 


bytes <- getOption("ffbatchbytes") * 3 ; bytes # around 64 Mb. 

system.time(
 res <-  ffdfsplit(test,test$ident,cond_date_df, BATCHBYTES= bytes) # 79.36  seconds 
  )

### FFDFDPLY 
system.time(
  res <-  ffdfdply(test,test$ident,cond_date_df, BATCHBYTES= bytes) #  77.78  seconds 
)


#### with + Rcpp function. 

fun_cpp <- fun 

cond_date_cpp <- function(date,diagnose){
  
  zz <- factor(fun_cpp(date,diagnose, date_min,date_max)) 
  zz 
}

test <- dfo[qn(date,diagnose)]

system.time(
  res <- with(test, cond_date_cpp(date,diagnose)) # this will make the calculus in RAM blocks that can fit into memory.
)  # 17 seconds. 


### apply_ffdf_split :: 

test <- df[qn(date,diagnose)] # no need to order data. 


cond_date_cpp_df <- function(x){
  
  zz <- factor(fun_cpp(x$date,x$diagnose, date_min,date_max)) 
  zz <- data.frame(zz)
  zz
}
system.time(
res <- apply_ffdf_split(test,cond_date_cpp_df,5,T) # 24 sec. 
)


### apply_ffdf_split :: 

test <- df[qn(date,diagnose)] # no need to order data. 


cond_date_cpp_df <- function(x){
  
  zz <- factor(fun_cpp(x$date,x$diagnose, date_min,date_max)) 
  zz <- data.frame(zz)
  zz
}
system.time(
  res <- apply_ffdf_chunks(test,cond_date_cpp_df,3e6,T) #  24 sec. 
)


### SPLIT_APPLY_CPP  + Rcpp : 

cond_date_cpp_split <- function(x){
  
  zz <- factor(fun_cpp(x$date,x$diagnose, date_min,date_max)) 
  data.frame(result = zz) 
  
}

cols <- qn(ident,date,diagnose)
test <- dfo[cols]
res <- split_apply_cpp(test, test$ident,4e6,cond_date_cpp_split) #  38 seconds 



######## GROUP_BY task #############

# Task 2: 
# in a large database, for each ident, find the number of cases with diagnoses "d" , "e" or "f". 
# If n == 0 label case as "none", if 0< n <= 6 label = few , others: label = many. 

### as a general rule, timings for group_by computations will depend on the number of levels for the variable grouped. 
### for same number of levels, the fact that data is ordered or not is not very important for some methods. 
### for other methods, the ordering can improve the speed (as for ffdfdply for example).


# we have to make those function over same ids :: 

# make the data :: 

gc() #   memory
Ndata = 20e6;  # number of observations. 20 million in this example. 
N_ids = Ndata/10 # number of unique idents. 
ident = factor( sample( 1000:(N_ids+1000), Ndata, replace = T)) # make ident as a sample from 1000 to 20 001 000
df <- ffdf( ident = ff(ident))  # create a ffdf database with a column ff( vector). This is written into disk. 
rm(ident) 
fdates <- ff(sample( 0:10000, Ndata, replace = T) + as.Date("1970-10-01"))
df$date <- fdates  # here we assign that vector to df.... if we change fdates, we will change df$date. This is diff from R. 
fdiagnose <- ff(sample( factor(letters), Ndata, replace = T )) # this is ok, factors... 
df$diagnose <- fdiagnose
fregion = ff( factor( sample( c("000","013" ,"014", "015", "020" ,"025", "030", "035" ,"040", "042", "050" ,"055" ,"060" ,"065" ,"070" ,"076", "080", "090"), Ndata, replace=T )) ) 
df$region <- fregion


# let us order the data by ident, to take as input the same database for all the methods, and also compare ordered vs non-ordered data: 

dfo <- order_ffdf(df,"ident")
#df <- dfo

str(df[1,])
gc()

# take a subset of 5e5 (500 000) rows to operate in RAM, and have not too long timings: 

N <- 5e5

data_o <- dfo[1:N,]
data <- data_o[sample(1:nrow(data_o)),]  # create an non ordered database, with the same content as the ordered. 

data[1,]
data$ident <- factor(data$ident) # take out the non present values. 
data_o$ident <- factor(data_o$ident) # take out the non present values.

length(levels(data$ident)) # 4972, this is important, as is the main parameter for the computation speed. 
length(levels(data_o$ident)) # 4972

# the function to apply over same "ident" :: 


get_task_3 <- function(x){
  nn <- sum(as.integer( x %in% c("a","b","c") ) ) 
  res <- ifelse(nn == 0, "none", ifelse(nn <= 6, "few", "many" ))
  res
}

res <- get_task_3(data$diagnose)
res # works.


###### TAPPLY :: 

system.time(
  res <- tapply(data$diagnose,data$ident, get_task_3, simplify=T) #  54.79  sec. 
)

# over ordered data ::

system.time(
res <- tapply(data_o$diagnose,data_o$ident, get_task_3, simplify=T) #  53.54 sec. 
)

# almost no difference between the two. 

res[1:10]
table(res)


###### AGGREGATE ::

# aggregate will generate data.frames :: 

system.time(
res <- aggregate(x = data["diagnose"], by = list(ident = data$ident), FUN = get_task_3) # 55.96 sec. 
)

# over ordered data : 

system.time(
  res <- aggregate(x = data_o["diagnose"], by = list(ident = data_o$ident), FUN = get_task_3) # 54.74  sec. 
)

res[1:3,]
#   ident diagnose
# 1  1000      few
# 2  1001     none
# 3  1002      few



####### DDPLY ::: 
# plyr package :::::::::::::::::::::::::::::::::
  
# plyr package is used to compute group by functions and return structured data. It has a lot of functions and can be very useful. 
# the main problem is that it can get VERY slow for large data sets.
  
library(plyr)

system.time(
  res <- ddply(                           # 
    data[c("ident","diagnose")],  # the data to take. 
    .(ident),                  # the split variable 
    summarize,                 # summarize will make a df from the next parameters: 
    d_abc = get_task_3(diagnose)  
  )
) 
#  225.12 sec. 


# over ordered data: 

system.time(
  res <- ddply(                           # 
    data_o[c("ident","diagnose")],  # the data to take. 
    .(ident),                  # the split variable 
    summarize,                 # summarize will make a df from the next parameters: 
    d_abc = get_task_3(diagnose)  
  )
) #  224.02


##### DATA.TABLE ### 

# data.table package :::::::::::::::::::::::::::::::
# ::::::::::::::::::::::::::::::::::::::::::::::::::

# data.table package gives the fastest (by far, excepting Rcpp) way to split-apply-combine data. 
# It has another "point of view" over data frames and can be confusing at the begining. 
# But once we grasp it, it is extremly usefull when dealing with large data or intense computations. 

library(data.table)

dataT <- data.table(data)  # this will make data as a data.table. 

dataT[1:3]  # calling rows 1 to 3 
setkey(dataT, ident)  # this is telling that we will order and group by ident. 
#setkeyv(dataT, c("ident", "date"))  # we also can order and group by more than one ... 
dataT[1:3]

# make groups and compute something:: 
system.time(
  res <- dataT[ , 
               list(
                 d_abc = get_task_3(diagnose)
               ), 
               by = ident 
               ] # same as above, done in 1.7 sec ::!!!! , 39 times faster than tapply and aggregate. 
)
res[1:10]

#### comparison with larger data :: 
#### some task are not linear in time, i.e. computation time is not a linear function of the size of the data, but can be geometric, S * ln(S), etc, which makes 
#### sometimes working with large data impossible with those methods. 

N <- 1e6 # double size. 
data <- dfo[1:N, ] # 1 million rows. 2x bigger than before.... 

system.time(
  res <- aggregate(x = data["diagnose"], by = list(ident = data$ident), FUN = get_task_3) #  336 sec. --> 6.2 times slower than for 1/2 sized data. relation NOT linear !!! 
)


begin.t <- Sys.time()
cols <- qn(ident, diagnose)
dataT <- data.table(data[cols])  # this will make data as a data.table. 
setkey(dataT, ident)  # this is telling that we will order and group by ident. 
system.time( 
  res <- dataT[ , list(d_abc = get_task_3(diagnose) ),  by = ident  ] 
)
print(paste("time spent data.table : ", Sys.time()- begin.t)) #   5.6 seconds. --> 60 times faster than aggregate. 

time.data.table <- 5.6 
time.aggregate <-  336
time.aggregate / time.data.table # 60 !!!! 

##### Rcpp ##### 

# To code group_by in Rcpp we will use loops over data ordered by the split variable. 
# in this case the code will be :: 


require(inline)
require(Rcpp)

src <- '
  
  #include <string>
  #include <iostream>   //this will allow == between strings... 
  
  using namespace Rcpp;
using namespace std;

IntegerVector ident = as<IntegerVector>(ident_);
CharacterVector diagnose_v = as<CharacterVector>(diagnose_) ;  //
  
// this will compute the maximum length of the "same ident" block:   
//std::set<int> s( ident.begin(), ident.end() );
//int uniqueN = s.size();
// it can be used to make vectors of size uniqueN and 
// skip the last part of the code (make resulting vectors). 


int N = ident.size();

CharacterVector diagnose_abc(N, "NA");
IntegerVector idents_u(N);

int NewId = 0; 
int OldId = -19896; 

int n_abc = 0; 

int uniques = -1; // -1 because it will add one for the first... 

for (int i = 0; i < N; i++){
  
  
  // for each i do (*A ) :: 
  NewId = ident[i];
  string diagnose = as<string>(diagnose_v[i]);
  
  
  // when we are in the same ids : 
  if(NewId == OldId){
    
    // conditions and functions for each row in same ids (*B ): 
    if( diagnose == "a" || diagnose == "b" || diagnose == "c"  ) n_abc +=1; 
    
    // last case :: do not forget the last one if it is the same ident as the previous.
    
    // output for the last case in case it is a same id as the previous one (*C ): 
    if( i == (N-1)) { 

      if(n_abc == 0) diagnose_abc[uniques] = "none"; else if(n_abc <= 6) diagnose_abc[uniques] = "few"; else diagnose_abc[uniques] = "many";
      idents_u[uniques] = OldId; 
    }
    
    
    // when there is a different id :: 
    
  } else {
    
    // output for the same ids block (it is the same function as the one for the last) (*C) : 
    if( i != 0){ 

      if(n_abc == 0) diagnose_abc[uniques] = "none"; else if(n_abc <= 6) diagnose_abc[uniques] = "few"; else diagnose_abc[uniques] = "many";
      idents_u[uniques] = OldId; 
      
    }
   
    
        
    // initial condition for a diff id (*D): 
    n_abc = 0; 
        
    // update old_id  (*E): 
    OldId = NewId;
        
    // conditions and functions for each row in same ids : (*B)
    if( diagnose == "a" || diagnose == "b" || diagnose == "c"  ) n_abc +=1 ;
      
        
    // update uniques  (*F) : 
    uniques +=1; 

    // if the last case is a single one with diff id : 

    // output for the same ids block (*C) : 
    if( i == (N-1) ){ 

      if(n_abc == 0) diagnose_abc[uniques] = "none"; else if(n_abc <= 6) diagnose_abc[uniques] = "few"; else diagnose_abc[uniques] = "many";
      idents_u[uniques] = OldId; 
      cout << "done last: " << OldId << " " << n_abc << " " << i <<" uniques " << uniques <<  endl;
      
    }
      
        
  }
  
  
  
}

// end loop. 

printf("... uniques :  %d \\n", uniques);

//printf("... uniqueN :  %d \\n", uniqueN);

  // build the resulting vectors to return :: 

  int Nuniques = uniques + 1; 
  CharacterVector result(Nuniques);
  IntegerVector ident_uniques(Nuniques);

  for (int i = 0; i < Nuniques; i++){ 

     result[i] = diagnose_abc[i]; 
     ident_uniques[i] = idents_u[i];
     string dabc = as<string>(diagnose_abc[i]); 

     //printf("... ident :  %d  , diagnoses : %s \\n", ident_uniques[i],  dabc.c_str() );

  }



// in case it was a factor, recover it :: 

if (Rf_isFactor(ident_)) { // Factor column.
                           
                           CharacterVector levelNames = Rf_getAttrib(ident_, R_LevelsSymbol); 
                           ident_uniques.attr("levels") =  levelNames;  
                           ident_uniques.attr("class") = "factor";
}

// return a data frame :: 

DataFrame DF = DataFrame::create( 
  
  Named("ident")=ident_uniques, 
  Named("diagnose_abc")= result 
);

return DF;
'


fun <- cxxfunction(signature(ident_ = "integer",diagnose_ = "integer"), src, plugin = "Rcpp") # defines the function....

#dfo <- order_ffdf(df,"ident") # remember data must be ordered ! 


# make a test :: 
data <- dfo[1:100,] # test. 
res <- fun(data$ident,data$diagnose)
res

# for 1/2 million rows (ordered data) : 

N <- 5e5 
ids <- dfo$ident[1:N]
diag <- dfo$diagnose[1:N]

system.time( res <- fun(ids,diag) ) #  %  0.11 seconds for 1/2 million rows !!! .
res[1:10,]

N <- 1e6 
ids <- dfo$ident[1:N]
diag <- dfo$diagnose[1:N]

system.time( res <- fun(ids,diag) ) #  %  0.19 seconds for 1 million rows, here the relation IS linear.
res[1:10,]

# for all the database. Try first with ALL in RAM : 

ids <- dfo$ident[]
diag <- dfo$diagnose[]

system.time( res <- fun(ids,diag) ) #  % 9 seconds for 20 million rows !!! . 

rm(ids,diag)
gc()
rm(data)

# timings: 
# for N = 5e5 : 

time.tapply <- 54 
time.aggregate <- 55
time.ddply <- 224 
time.data.table <- 1.7
time.rcpp <- 0.11 

# let us make the same with 2e6 (4x) if possible: 

N <- 2e6
data <-dfo[1:N, ]

time.aggregate.4 <- system.time(res <- aggregate(x = data["diagnose"], by = list(ident = data$ident), FUN = get_task_3))
t.aggregate.4 <- time.aggregate.4[["elapsed"]] #  1558.31 

time.tapply.4 <-  system.time( res <- tapply(data$diagnose,data$ident, get_task_3, simplify=T)  )
t.tapply.4 <- time.tapply.4[["elapsed"]] #  1476.97

time.ddply.4 <- system.time(res <- ddply(data[c("ident","diagnose")],.(ident),summarize,d_abc = get_task_3(diagnose))   )

time.rcpp.4  <- system.time( res <- fun(data$ident,data$diagnose) )
t.rcpp.4 <- time.rcpp.4[["elapsed"]]

time.rcpp.4[["elapsed"]] # 0.37 

# data.table : 
begin.t <- Sys.time()
cols <- qn(ident, diagnose)
dataT <- data.table(data[cols]) 
setkey(dataT, ident)
res <- dataT[ , list(d_abc = get_task_3(diagnose) ),  by = ident  ] 
time.data.table.4 <- Sys.time()- begin.t

t.data.table.4 <- as.numeric(time.data.table.4) #  7.443745 

times.4 <- c(t.tapply.4,t.aggregate.4,t.data.table.4,t.rcpp.4)
round(times.4, digits =2) # 1476.97 1558.31    7.44    0.37

times.relative.4 <- round(times.4/t.rcpp.4); times.relative.4 #  3992 4212   20    1



####### SQLDF #############


library(sqldf)


N <- 2e6
data <-dfo[1:N, ]
data$ident <- factor(data$ident)
data$result <- rep("none", nrow(data))

sql_1 <- "SELECT ident ,  
          CASE 
           WHEN   Sum( CASE 
                            WHEN( diagnose='a' OR diagnose='b' OR diagnose = 'c')
                            THEN 1 ELSE 0 END
                      )  = 0 
           THEN 
              'none' 
           ELSE 
              CASE WHEN
                     Sum( CASE 
                            WHEN( diagnose='a' OR diagnose='b' OR diagnose = 'c')
                            THEN 1 ELSE 0 END
                      )  <= 6 
              THEN 
                  'few'
              ELSE 
                  'many'
              END 

           END 

          `diagnose_abc`

          FROM data 
          group by ident"

dim(data)


t.sql <- system.time(res_sql <- sqldf(sql_1)) #    6.35 seconds ! 
time.sql.4 <- t.sql[["elapsed"]]
time.sql.4
res_sql <- res_sql[order(res$ident),]
#table(res$diagnose_abc)
res_sql[1:10,]

###########################



####### timing numbers (evaluated previously) ###### 

time.names <- c("tapply","aggregate", "data.table", "sqldf", "Rcpp")
time.names <- factor(time.names, levels=time.names,ordered=T)
times.4 <- c(1476.97,1558.31,7.44,6.35,0.37)
tt <- round(times.4, digits =2) # 1476.97 1558.31    7.44    0.37
times.relative.4 <- round(times.4/0.37); times.relative.4 #  3992 4212   20    1
time_label <- c("27 minutes","26 minutes","7 seconds","6.3 seconds","0.37 seconds")
speed2 <- data.frame( method = time.names , time = times.4, time_relative = times.relative.4 , time_label)

tt/60*10/60
tt/60*10
tt*10


plot <- ggplot( speed2,aes(x=method, y = time_relative,fill = method ,group= 1 )   ) 
plot <- plot +
  geom_bar(stat="identity") + 
  xlab('computing methods')  +  
  ylab('relative computing time')+ 
  ggtitle("Relative computing times for different methods in R  \n over a 2 million row data frame ") 

plot2 <- plot + 
  geom_text(data=speed2,aes(x=method,y=time_relative,label= time_label),vjust=-0.5, size = 4)  + 
  theme( 
    plot.title=element_text(size=16, vjust = 1) 
    ,legend.position=c(0.82,0.67) 
    ,axis.title.x  = element_text(vjust=-1, size = rel(1.2))
    ,axis.title.y  = element_text(angle=90, vjust=0, size = rel(1.2))
    ,plot.margin=unit(c(1, 1, 1, 1), "lines")
  )

plot2


#setwd("C:/Users/jkc261/Documents/Projects/Large_data_and_R") # change this directory for your computer.  
### saving to tiff :: 
tiff(file = "C:/Users/jkc261/Documents/Projects/Large_data_and_R/speed2.tif",width = 7, height = 5, units = "in", res = 500, compression="lzw")
#pdf(file = "P:/HorizonData/Bar Plots/BarPlotsAptima.pdf",width = 10, height = 7.5)
plot2
dev.off()

