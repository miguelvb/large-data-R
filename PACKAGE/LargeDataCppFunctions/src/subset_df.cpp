#include <Rcpp.h>
//#include <RcppArmadillo.h>
#include <string>    // for std::string and std::getline

using namespace Rcpp;
using namespace std;

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/*
template <int RTYPE>
Vector<RTYPE> roll_vector_na(const Vector<RTYPE>& x) {
  
    int n = x.size() ;

    Vector<RTYPE> out = clone(x) ;
    
    for( int i=1;   i<n; i++){
        
        //printf("elem %d", i); 
        
        if( Vector<RTYPE>::is_na( x[i] ) ) 
          out[i] = out[i-1];
        else 
          out[i] = x[i];
    }
    
    return out ;
}


// [[Rcpp::export(na_roll_cpp)]]
SEXP na_omit(SEXP x) {
    switch(TYPEOF(x)) {
        case INTSXP:
            return roll_vector_na<INTSXP>(x);
        case REALSXP:                   
            return roll_vector_na<REALSXP>(x);
        case LGLSXP:
            return roll_vector_na<LGLSXP>(x);
        default:
            stop("unsupported data type");
    }
}

// [[Rcpp::export]]
SEXP roll_vector_nas( SEXP x ){
     RCPP_RETURN_VECTOR( roll_vector_na, x ) ;   
}


*/

IntegerVector  roll_vector_int(IntegerVector x) {
IntegerVector  v(x) ;
int Nidx = v.size();
IntegerVector result = clone(v); 
for( int i = 1; i < Nidx; i++){
if(IntegerVector::is_na( result[i] )) result[i] = result[i-1] ;  
}
return result;
}


NumericVector  roll_vector_num(NumericVector x) {
NumericVector  v(x) ;
int Nidx = v.size();
NumericVector result = clone(v); 
for( int i = 1; i < Nidx; i++){
  if(NumericVector::is_na( result[i] )) result[i] = result[i-1] ;  
}
return result;
}

CharacterVector  roll_vector_char(CharacterVector x) {
CharacterVector  v(x) ;
int Nidx = v.size();
CharacterVector result = clone(v); 
for( int i = 1; i < Nidx; i++){
  if(CharacterVector::is_na( result[i] )) result[i] = result[i-1] ;  
}
return result;
}

LogicalVector  roll_vector_log(LogicalVector x) {
LogicalVector  v(x) ;
int Nidx = v.size();
LogicalVector result = clone(v); 
for( int i = 1; i < Nidx; i++){
  if(LogicalVector::is_na( result[i] )) result[i] = result[i-1] ;  
}
return result;
}



// [[Rcpp::export(roll_vector_nas)]]
SEXP roll_vector_nas(SEXP x) {
    switch(TYPEOF(x)) {
        case INTSXP:
            return roll_vector_int(x);
        case REALSXP:                   
            return roll_vector_num(x);
        case LGLSXP:
            return roll_vector_log(x);
        case  STRSXP:
            return roll_vector_char(x);
        default:
            return x;
    }
}


/*
// [[Rcpp::export]]
SEXP roll_vector_nas( SEXP x ){
     RCPP_RETURN_VECTOR( roll_vector_na, x ) ;   
}

*/




// [[Rcpp::export]]
List subsetCpp( DataFrame data, IntegerVector indxs){
  
  // this function will subset a DataFrame, by the vector of indexes 
  // indxs. 
  // It returns a List, because returning a DataFrame takes aprox 100 x more. 
  //cout << "begin subset" << endl; 
  DataFrame df(data); 
  int ncol = Rf_length(df);
  CharacterVector colNames(ncol); 
  SEXP names = Rf_getAttrib(df, R_NamesSymbol);
  SEXP colData = VECTOR_ELT(df,0); // First column of data.
  //int nrow = Rf_length(colData);
  // IntegerVector indexes = IntegerVector::create(1,5,10); 
  IntegerVector indexes = clone(indxs);  
  indexes = indexes -1 ; 
  int Nidx = indexes.size(); 
  
  List result(ncol); 
  
  for (int i=0; i < ncol; i++) {
    
    colNames[i] = std::string(CHAR(STRING_ELT(names,i)));
    
    SEXP colData = df(i);
    
    bool isDateClass = false;
    SEXP classname = Rf_getAttrib(colData, R_ClassSymbol);

    if (classname != R_NilValue)
      isDateClass = (strcmp(CHAR(STRING_ELT(classname,0)),"Date") == 0);   
      
    if (Rf_isReal(colData)) { // REAL 
      
      NumericVector subsetData(Nidx);
      NumericVector v = as<NumericVector>(colData); 
      
      if (isDateClass) subsetData.attr("class") = "Date";
      
        for (int j=0; j < Nidx; j++) {
                  int idx = indexes[j]; 
                  subsetData[j] = v[idx];
        }
        result[i] = subsetData;
        //cout << "Real Col " << i << endl; 
    }   
    
    else if (Rf_isInteger(colData)) {  // INTEGER 
      
         IntegerVector subsetData(Nidx);
         IntegerVector v = as<IntegerVector>(colData);
         
         if (isDateClass) subsetData.attr("class") = "Date";
         
         for (int j=0; j < Nidx; j++) {
                  int idx = indexes[j]; 
                  subsetData[j] = v[idx];
         }
         result[i] = subsetData;
         //cout << "Int Col " << i << endl;
    }  
    
     else if (Rf_isString(colData)) { // Non-factor string column
        CharacterVector subsetData(Nidx);
         CharacterVector v = as<CharacterVector>(colData);
           for (int j=0; j < Nidx; j++) {
                  int idx = indexes[j]; 
                  subsetData[j] = v[idx];
         }
         result[i] = subsetData;
     } 
    
    
     else if (Rf_isFactor(colData)) { // Factor column.
     
          IntegerVector subsetData(Nidx);
          IntegerVector v = as<IntegerVector>(colData);
          
          SEXP names = Rf_getAttrib(colData, R_LevelsSymbol);
          int numLevels = Rf_length(names);
          //std::string *levelNames = new std::string[numLevels];
          CharacterVector levelNames = Rf_getAttrib(colData, R_LevelsSymbol); 
          //for (int k=0; k < numLevels; k++)
          //levelNames[k] = std::string(CHAR(STRING_ELT(names,k)));
          
          //for (int k=0; k < numLevels; k++) cout << levelNames[k] ; 
          //cout << endl; 
          //cout << " factor col" << endl; 
          subsetData.attr("levels") =  levelNames;  
          subsetData.attr("class") = "factor";
          
           for (int j=0; j < Nidx; j++) {
                  int idx = indexes[j]; 
                  subsetData[j] = v[idx];
          }
          result[i] = subsetData;
          //delete [] levelNames;
     }
      
      else if (Rf_isLogical(colData)) {  // LOGICAL 
          LogicalVector subsetData(Nidx);
          LogicalVector v = as<LogicalVector>(colData);
          for (int j=0; j < Nidx; j++) {
                  int idx = indexes[j]; 
                  subsetData[j] = v[idx];
          }
          result[i] = subsetData;
     }
     
     else {   // WHEN COL IS NOT TREATED 
        
       
       CharacterVector vv(Nidx, "ERROR"); 
       result[i] = vv; 
       
     }
    
  }

  result.attr("names") = colNames;
  //result.attr("class") = "list"; 
  //Rcpp::DataFrame res(result);  // this makes it 100x slower than List .... 
  //cout << "end subset" << endl;
  //return(res); 
  return(result); 
  
}


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
List getSubsetSplit(DataFrame Dsexp, List split_idx, int beg_index, int end_index, Function fun )
{

// get_split will apply function fun to each one of the split-indexes in the split_idx list. 
// beg_index and end_index can be obtained from get_chunks_indexes_for_split_list, 
// and will make possible to work with chunks of data. 
// the function to apply MUST be working with a list of lists...
// so we should have like fu_apply <- function(x){ lapply(x, fu )} 
// fu <- function(x) {  c(min.date = min( x["date"] )  }  for example.... 

DataFrame data = DataFrame(Dsexp);
Function fu  = Function(fun);
List split_id = clone(split_idx);

int Napply = end_index - beg_index + 1 ; 
int beg = (beg_index -1) ; 
int end = (end_index) ; 
List sol(Napply); 
IntegerVector first_split = split_id[beg]; 
int first_index = first_split[0]; 

int lag_indexes = 0; 
if(first_index != 1) { lag_indexes = first_index  -1; }; 

//cout << "lag index for data : " << lag_indexes << endl; 

int sol_index = 0; 

for (int i=beg ; i < end ; i++){
   IntegerVector idx = split_id(i) ;
   IntegerVector idx_ = idx - lag_indexes;       // take into account that the data can be subsetted , so first index of all is set to 1. 
   //cout << "f index " << idx_[0] << endl; 
   List sub_data = subsetCpp(data, idx_); 
   cout << "split " << i+1 << "/" << end << endl; 
   //indexes.push_back( idx );  
   sol[sol_index] = fu(sub_data); 
   sol_index += 1; 
}

//sol = Lapply(indexes, fu); 

//List examp = sol[0]; 
//CharacterVector names = Rf_getAttrib(examp, R_NamesSymbol);
//sol.attr("names") = names; 
//DataFrame res(sol);
//return(res);
return(sol); 
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
Rcpp::List getSplitIndexes(Rcpp::DataFrame Dsexp, SEXP split_row_name)
{


// it will get the split indexes by same values of split_row_name. 
// the result of this feeded into get_split will make a split-apply-combine. 

Rcpp::DataFrame data = Rcpp::DataFrame(Dsexp);
//Rcpp::Function fu = Rcpp::Function(fun);
string split_name = Rcpp::as<string>(split_row_name);
Rcpp::IntegerVector ids = data[split_name];


int id_i =  ids[0];
int N = ids.size();
Rcpp::IntegerVector indexes ; 

std::set<int> s( ids.begin(), ids.end() );
int uniqueN = s.size(); 

// a way to call R functions:: 
//Environment base("package:base");
//Function rm = base["rm"];
//rm("s"","ids); 

// another one :: 
//Rcpp::Function length("length");
//int ll =  Rcpp::as<int>(length(ids));
//cout << ll << endl; 

printf("data size :: %d , uniques : %d \n", N, uniqueN);

Rcpp::List sol(uniqueN);
int nrows = 0; 
int id_int = 0;

for (unsigned int i=0; i < (unsigned int) N; i++){
  nrows += 1;
  
  if(nrows%1000000 == 0) printf(" rows done :: %d\n", nrows); 
  
  int new_id = ids[i] ; // is there a new value for the split ??  
  
    if( new_id == id_i) { 

      indexes.push_back(i+1);
      
      if(i ==  N-1 ){
        sol[id_int] =  indexes; 
        //sol[id_i-1] = indexes; 
      };
    };
    // change in id  value..   
    
    if( new_id != id_i){
       
      //printf(" id :  %d\n", new_id);
      int ll = indexes.size(); 
      //printf(" indexes size  :  %d\n", ll);
      sol[id_int] = indexes;
      id_i = new_id;
      id_int +=1;
      Rcpp::IntegerVector indxx ;
      indexes = indxx; 
      //indexes.clear(); 
      indexes.push_back(i+1);
      if(i ==  N-1) sol[id_int] = indexes;
      
    };
  
}
    return(sol);

}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////


// [[Rcpp::export]]
List Lapply(List data, Function fun){
  List input(data);
  Function f(fun);
  List output(input.size());
  transform( input.begin(), input.end(), output.begin(),f);
  output.names() = input.names();
  return output;
}


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
DataFrame getChunksIndexesForSplitList(DataFrame data_, List split_indx, int chk_size){
  
  // will make a DataFrame with info for indexes on split_indx list and DataFrame indexes
  // to work on different chunks
  // this is the output :: 
  //#     chunk_number beg_split_index end_split_index beg_data_index end_data_index
  //# 1             1               1            6586              1         100018
  //# 2             2            6587           13372         100019         200001
  List split_indexes(split_indx); 
  DataFrame data = data_; 
  int chuk_size  = chk_size; 
  int chuk_row =  chuk_size; 
  int Nsplits = ceil( data.nrows()/ ((double)chuk_size ) ) ; 
  // if(Nsplits == 0) Nsplits = 1; 
  //
  
  printf("Nsplits %d , Nrows %d , chk_size %d \n", Nsplits,data.nrows(), chuk_size ) ; 
  int Nindexes = split_indexes.size(); 
  IntegerVector chuk_split_index(Nsplits); 
  IntegerVector chuk_begin_data(Nsplits); 
  IntegerVector chuk_end_data(Nsplits); 
  //printf("Nsplits %d \n", Nsplits) ;
  int split_number =  0 ; 
  //int last_i =  0 ; 
  
  for(int i = 0 ;  i <  Nindexes ; i++){
  //printf("loop %d \n", i) ;  
    IntegerVector split = split_indexes(i) ; 
    int maxi =  max(split); 
    
    if(maxi >= chuk_row ){ 
      
      chuk_split_index[split_number] = i; 
      printf("split n %d chuk_row %d chuk_split_index %d \n" ,split_number, chuk_row, i); 
      chuk_row =  chuk_row + chuk_size; 
      split_number = split_number + 1; 
      //last_i = i ; 
    }
    
  }
  //printf("done splits, split_number %d \n", split_number) ;
  // if(split_number == Nsplits-1 & Nsplits != 1) {
  if(split_number == Nsplits-1 ) {  

    chuk_split_index[split_number] =  Nindexes -1;  //  case when we got until the end, but there were some remaining indexes ... 
    
  }   
  
  // make the vector of the begin indexes :: 
  IntegerVector chunk_number = seq_len(Nsplits);  
  IntegerVector chuk_beg(Nsplits);
  chuk_beg[0] = 0 ; 
  
  for(int i = 1;  i <  Nsplits ; i++){
    
    chuk_beg[i] = chuk_split_index[i-1] + 1; 
  }

  // now make the vector of the beg and end data indexes ::: 
  
  for(int i = 0;  i <  Nsplits; i++){
    int chu = chuk_beg[i]; 
    int idx = chuk_split_index[i]; 
    //if(idx == )
    IntegerVector spl_b  =   split_indexes(chu); 
    IntegerVector spl_e  =   split_indexes(idx);
    chuk_begin_data[i] = min(spl_b); 
    chuk_end_data[i] = max(spl_e);
  }
  
  
  DataFrame DF = Rcpp::DataFrame::create( 
    Named("chunk_number")=chunk_number, 
    Named("beg_split_index")=chuk_beg +1 , 
    Named("end_split_index")=chuk_split_index +1 ,
    Named("beg_data_index")=chuk_begin_data,
    Named("end_data_index")=chuk_end_data 
  );
  //return(  data.frame(chunk_number =chunk_number, beg_split_index = chuk_beg, end_split_index = chuk_split_index, beg_data_index = chuk_begin_data, end_data_index = chuk_end_data  ))
  return(DF); 
  
}


          
