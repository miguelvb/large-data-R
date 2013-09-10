LARGE DATA R 
============
### NEW R FUNCTIONS AND TECHNIQUES THAT ALLOW WORKING WITH LARGE DATA IN R ###
miguelvb@yahoo.com

More info on 
[repidemiology](url:https://repidemiology.wordpress.com/)

----

Set of functions and examples to deal with large data in R. 

----

## LIST OF FUNCTIONS (summary)

----

### order_ffdf(data=,order_cols=,splits=,verbose=)

 will order a ffdf by the order column names, using splits. Can verbose the process.
 Using splits, can avoid memory problems found when using ffdforder or ffdfsort.

----

### subset_ffdf(data=,condition_cols=,condition=,splits=,verbose=)

 subset a ffdf, by the condition columns, with a condition. Makes splits if needed and verbose.

### cbind_ffdf(x=,y=)

 will bind columns of y to columns of x .
 obs: if not cloned, the binded cols will still be the cols of y.
 and then if saved without cloning, y will loose all its columns.

----------------

### append_ffdf(x=,y_=,chunk_s=)

 will append two ffdf using chunks from the second ffdf.
 used mostly when ffdfappend did not append two ffdf but only one ffdf with one data.frame.
 now, we prefer the use of ffdfappend, which is eqivalent to this one.
 THis function can be useful when memory conditions get difficult.
 It has also the feature to display info about the total estimated time, remaining, etc.

----------------

###  merge_by(x=,y=,keycols=,split_name=,chk=)

 merge_by is used to merge two ffdf, in an efficient way. Data from the first ffdf must be ordered by the split variable.
 the merge is done over "keycolumns" and only the data that have the same combination of keycolumns values will be retained.
 This is a merge as in SQL in the code : WHERE t1.ident = t2.ident , etc.
 merge_by is using a merge with data.table in this way:
 merged <- y[x, nomatch = 0]
 and then the "no matching" records are NOT included in the result.

----------------

### index_ffdf(x=,idx=,verbose=)

 function to index a ffdf .
 idx is a ff vector with the indexes.
 ffdforder gives memory problems because of the use of ffdfget_columnwise.
 to avoid those problems, we can use index_ffdf or for larger data: index_ffdf_split.

----------------

### index_ffdf_split(x=,idx=,nsplits=,verbose=)

 function to index a ffdf .
 idx is a ff vector with the indexes.
 ffdforder gives memory problems because of the use of ffdfget_columnwise.
 to avoid those problems, we can use index_ffdf or for larger data: index_ffdf_split.
 using several splits we get into memory smaller blocks of data.
 this allows to index huge data bases with no memory overflows...

----------------


### split_apply_cpp(input_data=,split_vector=,chk_size_=,fu=)

 split_apply_cpp will take a ffdf which MUST be ordered by the split vector, and will compute the function fu over the splits of size chk_size, RESPECTING the same split_vector
 values. That means that when making splits, we will not break the data where same values of the split vector are. Same values of the split vector will be in one single split.
 This allows to use this function with some group_by functions, over large data sets.

 function fu must operate over the database input_data as a data.frame (when it is loaded in memory, not as ffdf).

----------------
## SOME UTILITIES 
----------------

----------------

### redir_ffdf(ffdf=,newdir= )

 will redefine a directory for a ffdf database which was saved in a directory and moved manually to the present one.
 This function will allow to avoid the error filename == 0 , allowing the use of the ffdf again.
 This function works for data saved with save.ffdf within the SAME directory name as the name of the database.

----------------

### redir_ffdf_ffdfsave(ff=,newdir=)

 will redefine a directory for a ffdf database which was saved in another directory and moved manually to the present one.
 This function will allow to avoid the error filename == 0 , and we will be able to use the ffdf again.
 This function works for data saved with ffdfsave with the SAME file name as the name of the database

----------------

### is_ordered_by_row_col(data=,col_name=)

 will give TRUE if in a ffdf database, a column named col_name is like the row numbers.
 That column MUST be in the form 1,2,3,4,5,.... N , as if it was the row indexes in order.

----------------

### quote_names()
                  
 will make a vector of the names that appear in the expression. Very useful.

### qn()

 abbreviation of the same function.

----------------

### lagpad(x=)

 will insert a NA value at the beginning of the vector x, keeping the vector length (removing the last item)

----------------

### asDate(x=)

 will take a number x and transform it to a date, with origin 1970-01-01 (R default)

----------------

### txt.perc(x=,dec=)

 will tranform x (a number) to a string of percent with "dec" precision.
 Useful for labelling graphics.

----------------

### fill.vec(x=,fill="*")

 will fill a vector "x" with characters "fill", to the maximum length of the items in x.
 In integer or numeric cases, it will add "0" at the beginning of the vector.
 In other cases will add "fill" at the end.
 It will delete blank spaces at the beginning and at the end of the items. But will respect the "inner" blank spaces.
 Used to export data bases as fixed column data with custom fills, to easy import in SQL, etc.

----------------

###  fill.vec.all.same(x=,fill="*")

 same as fill.vec, but will fill all the vectors with end character fills (as if they were not numeric).

----------------

### fill.data.frame(x=,fill.char="*",make.factors=FALSE)

 Will fill all the columns of a data frame, as fill.vec is doing.
 In case make.factors = T , it will make all the colums as factors (to be used with ffdf)

### fill.data.frame.all.same(df,"_")

 same as fill.data.frame, but will fill all the vectors with end character fills (as if they were not numeric). 
