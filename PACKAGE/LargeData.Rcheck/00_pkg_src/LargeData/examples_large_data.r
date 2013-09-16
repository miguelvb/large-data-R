########################
#
# examples LargeDataR 
#
########################


## append_ffdf : 

require(ffbase)
require(LargeDataFunctions)

N1 <- 1e6  # data size, change if needed.
N2 <- 1e6 

df <- as.ffdf(data.frame(x=1:N1, y= sample(1:100,N1,replace=T)))
df[1:2,]
df2 <-  as.ffdf(data.frame(x=sample(1:1000,N2,replace=T) , y=1:N2 + 132423) )
df2[1:2,]

res <- append_ffdf(df,df2,1e5) 


##  apply_ffdf_chunks : 

require(LargeDataFunctions)

N <- 1e7  # 10 million rows
n <- N/10
dat  <- ffdf( x = ff(sample(1:n, N, replace = T)), y = ff(factor(sample(letters, N, replace = T))), z = ff(1:N))

values <-  function(x){ data.frame( sum = x$z + x$x, rest = x$z - x$x/2. ) } 

res <- apply_ffdf_chunks(dat,values, 2e6, T)
res[1:3,]
dim(res)


##  apply_ffdf_split : 

N1 <- 1e7  # data size, change if needed.
N2 <- 1e5 

df <- as.ffdf(
  data.frame(
    ident= sample(1:N2,N1,replace=T),
    diagnose = as.factor(sample(letters[1:10],N1, replace=T))
  )
)

df[1:2,]


fu <- function(x){ 
  data.frame( 
  ident = x$ident,
  health =ifelse(
          x$diagnose %in% c("a","b","c") , "healthy", "sick" )
  )
}


res <- apply_ffdf_split(df,fu, 4, T)
res[1:10,]

#### asDAte 

date <- as.Date("2000-01-12")
i.date <- as.integer(date); i.date # 10968
asDate(i.date)


## cbind_ffdf : 

# obs: if not cloned, the binded cols will still be the cols of y. 
# and then if saved without cloning, y will loose all its columns. 

data <-  ffdf( a = ff(sample(1:10)), b = ff(factor(sample(letters, 10, replace = T))) )
data_2 <-  ffdf( x2 = ff(sample(10:19)), y2 = ff(factor(sample(LETTERS, 10, replace = T))), z2 = ff(10:1))
data[1:3,]
data_2[1:3,]

res <- cbind_ffdf(data,data_2)  # will bind the columns of data_2 to the cols of data. 
res[1:3,]

filename(res$x2); filename(data_2$x2) # they are the same !, so clone if you want to make a single data frame: 
res <- clone(res)
filename(res$x2); filename(data_2$x2) # now they are different





