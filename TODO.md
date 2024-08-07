

1. TEST SUITE - create
   + We have example rda files 
   + we can use toc() and lload() for each and then compare the results.
      + when getting the element from the toc(), make certain to unclass to avoid [[.RDAToc
	  + but can also compare the results by getting the object.

1. toc(  "inst/sampleRDA/utilsNamespace.rda")

1. Error for toc( "inst/sampleRDA/NULL.rda")
```
Error in names(ans)[name] = tag : 
  'names' attribute [1] must be the same length as the vector [0]
```
   + Need to decide what to do when 
      + top-level object is NULL
	  + element of a pair-list is a NULL at any depth/level
   + Problem is that we are populating a list() and the first value is 
   NULL. When we insert that with `ans[[1]] = NULL`, we end up with an empty list.
   Then we set the name and there is no element.
   We can use `list(NULL)` for the first element.
   
1. [check result] Error `toc("inst/sampleRDA/emptyenv.rda")`

1. [check result] Error `toc("inst/sampleRDA/symbol.rda")`
   + Can't put attributes on a symbol. Need a singleton object.
   + See wrapObject() and setOffset()

1. Check examples
```
rdas = list.files("../inst/sampleRDA", pattern = "\\.rda", full = TRUE)
tmp = structure(lapply(rdas, function(x) try(toc(x))), names = rdas)
err = sapply(tmp, inherits, 'try-error')
table(err)

tt = table(unlist(lapply(tmp[!err], function(x) as.data.frame(x)$type)))
setdiff(names(RDAInfo:::SEXPMap), names(tt))
```
   + Currently errors for 3 of 43
   
   + function_with_attrs.rda - lload() messes up the function - no body.

1. 18 SEXP types remaining
  + Actually 3 if compare
  +  BCODESXP =     21    
  + √ S4SXP =        25    
  + check SPECIALSXP =    7	  
  + check  BUILTINSXP =    8	  
  + √ CLOSXP = 	     3	  
  + PROMSXP = 	     5	  
  + DOTSXP = 	    17	  
  + WEAKREFSXP =   23    

  + NILSXP
  + SYMSXP
  + LISTSXP
  + PROMSXP
  + SPECIALSXP
  + CHARSXP
  + DOTSXP
  + BCODESXP
  + WEAKREFSXP
  + RAWSXP
  + REFSXP
  + NILVALUE_SXP
  + UNBOUNDVALUE_SXP
  + MISSINGARG_SXP
  + BASENAMESPACE_SXP
  + NAMESPACESXP
  + PACKAGESXP
  + PERSISTSXP
  + CLASSREFSXP
  + GENERICREFSXP
  + BCREPDEF
  + BCREPREF
  + EMPTYENV_SXP
  + BASEENV_SXP
  + ATTRLANGSXP
  + ATTRLISTSXP
  + ALTREP_SXP


  + Not mentioned in serialize.c
     + X ANYSXP = 	    18	    shouldn't see these
     + X NEWSXP =       30    
     + X FREESXP =      31    
     + X FUNSXP =       99    

  + √ EXPRSXP = 	    20	    check
  + √ VECSXP = 	    19	  
  + √ LANGSXP = 	     6	  
  + √ ENVSXP = 	     4	  
  + √ SYMSXP                         <<<<<<<<<<<<< not in test cases

1. See traceSEXP.R for determining which SEXP types were processed.

1. √ Change in R-devel for OBJSXP and not S4SXP

1. BCODESXP
   + √ "inst/sampleRDA/trimws_function.rda"
   + "~/OGS/SUForm/ProcessForm/June8.rda"
      +   arguments imply differing number of rows: 1, 0
   
1. ALTREP_SXP
   + "~/OGS/COVID/WinterInstruction/AllCourses/AllCourses.rda"
   + Problem in reading the second element - the state.
     + This is a LISTSXP. We can read the first element - the character(264) vector and then
       integer.
	   + Then we get to another LISTSXP
```
sexp type = 2 depth = 0 hastag = 1 hasattr = 0
sexp type = 1 depth = 1 hastag = 0 hasattr = 0
sexp type = 9 depth = 2 hastag = 0 hasattr = 0
sexp type = 238 depth = 1 hastag = 0 hasattr = 0
sexp type = 2 depth = 2 hastag = 0 hasattr = 0
sexp type = 1 depth = 3 hastag = 0 hasattr = 0
sexp type = 9 depth = 4 hastag = 0 hasattr = 0
sexp type = 2 depth = 2 hastag = 0 hasattr = 0
sexp type = 1 depth = 3 hastag = 0 hasattr = 0
sexp type = 9 depth = 4 hastag = 0 hasattr = 0
sexp type = 2 depth = 2 hastag = 0 hasattr = 0
sexp type = 13 depth = 3 hastag = 0 hasattr = 0
sexp type = 254 depth = 2 hastag = 0 hasattr = 0
sexp type = 2 depth = 2 hastag = 0 hasattr = 0
sexp type = 16 depth = 3 hastag = 0 hasattr = 0
sexp type = 9 depth = 4 hastag = 0 hasattr = 0
   ....  repeated for 264 elements
sexp type = 13 depth = 2 hastag = 0 hasattr = 0            <<<<<<<<<<<<<<<<
sexp type = 2 depth = 2 hastag = 1 hasattr = 0             <<<<<<<<<<<<<<<<
sexp type = 1 depth = 3 hastag = 0 hasattr = 0
sexp type = 9 depth = 4 hastag = 0 hasattr = 0
sexp type = 16 depth = 3 hastag = 0 hasattr = 0
```
   In the R code, this last entry does not have a tag. The value is simply 2.
   So reading the next entry goes wrong.
   This is the start of the readPairList
   
   + When we read the 13 as the next element of the state pair list after the character vector,
      we have a ty of 13, but then call ReadItem which reads the type.  We already have the type.
	  
1. References at the toplevel
   + save(a = obj,  b = obj) - same object.
   + capture concept that restoring b means binding to the value of a.

1. [test] Capture the references.
   + Need to them for √ symsxps, √ environments, PACKAGESXP, PERSISTSXP, NAMESPACESXP, EXTPTRSXP, WEAKREFSXP, ....
   + √ See Expression.rda.  The second call has "20" where a should be.

1. Handling character encoding.
   + The note in serialize.c "  strings without an encoding flag will be converted to the current native  encoding" indicates that the CHARSXP?/STRSXP may contain an encoding instruction.
   + See ReadChar in serialize.c.  The encoding is in the levs flag.

1. get file name from the connection
   + summary(con)$description
   + add it to the RDAToc object returned by toc() if we start with a connection.
     + already do add file so that would be the connection and we can reopen it
       if we are using it in, e.g., [[ or [
	   + assuming it is a local file - could be a stream that we cannot retrieve again, but hopefully the description
	     will allow us to at least understand its provenance.

1. FIX:  doesn't work.  But `lapply(unclass(info), ...)` does.
```
f = "inst/sampleRDA/class_named_integer_logical_character_uncompress.rda"
info = toc(f)
sapply(info, function(x) x$offset)
sapply(unclass(info), function(x) x$offset) # works
```
  + The lapply is using the [[ and so is deserializing the individual object!
  + A method for lapply()? for RDA.toc

1. [ok but could be better]  seek() on a gzfile to an offset is not working reliably. 
  + Fine for uncompressed save() files.
  + This may not be possible, or we have to go in smaller steps.
      + https://stackoverflow.com/questions/30834963/seeking-on-a-gz-connection-is-unpredictable
      + explore if this is something we can "fix" in R or is it a characteristic of gunzip.
  + We can read from the start to the offset and discard what we read just to get to the offset.
      + using more memory than we should and need but will work around problem.


## Problem Files

+ Look at the .RData files
   + ~/OGS/PRCC/GTTP/.RData - seg faults. Not now. Gives an error `d$offset = p : object of type
     'symbol' is not subsettable` which we see when reading a simple symbol.
	 + Now works 
   + now name for the "cookie" element is "" - ty after reading getPRM function has hastag == 0. It
     is type 2 for the next element of the pairlist. So somehow messed up the tag.s.
   + we have an extra element (25), and  an attribute is list() at depth 7
      which seems wrong.
   + element o comes before getPRM.	  Is that where things are going wrong.
     + XX o also has hasattrs == 0 but has attributes. The info is for the next pairlist which don't have attributes
   + for getPRM element, info has hasattr = 0, but it does have a srcref attribute. So this looks wrong.	 
   + √ Now get an error with a reference for the external pointer.
      + in readREFSXP, if not in the hdr$references, return NULL.
      + docs element is a list of HTLInternalDocument objects, so externalptr
```
11: readRDA.R#39: readExternalPointer(con, elInfo, skipValue, hdr, de
12: readRDA.R#688: ReadItem(con, skipValue, hdr, depth + 1)
13: readRDA.R#39: readREFSXP(con, ty, hdr)
14: readRDA.R#751: get(as.character(ref - 1), hdr$references)
```
   + √ Fixed this. Need to read size of buffer in chunks when the string is too large.
      + ¿ A large vector whose length we get as a shorter integer and so is wrong.
      + Seg faulting in R_eatCharVectorElements
      + X Is this a different RDA format? NO
      + The file is 38734 bytes
      + The totalChars at the segfault is 113744 and so is nchars
      + The .RData contains 11 functions which capture the R_GlobalEnv.


## Extra Information

+ Do we want the length of the body for a function??
  + would have to change ReadItem for this object to collect the number of expressions or 
	    we can set skipValue = FALSE and reconstruct the body.

+ Do we want the attributes on an expression
  + See inst/sampleRDA/Expression.rda and srcref, wholeref

+ Do we want the srcref for functions?


## Performance

+ measure time when using toc() on a streaming download from a URL.

## Check

+ √ S4 - putting the tag on an empty ans in readPairList `names(ans)[name] = tag`

+ √ In addDescAttrs(), ignore values that have length 0.
   + See "../inst/sampleRDA/dotsFunction.rda"            

+ √ In readEnvironment(), wrap the ans$names value in a list() since a data.frame with 1 row.


## Verify

+ √ o$d giving NULL for o = toc("../inst/sampleRDA/test.rda")
  + but the tag for that element in [[.RDAToc has the actual value. So off by an element.
  + are we recording the offset incorrectly for this element.
  + was getting all the remaining elements after this object.

+ √ factor() example.
   + see toc("inst/sampleRDA/factor.rda")

+ √ regular list objects 
  + √ add the names of the elements
  + √ the type of each element
  + √ the length of each element?
  + 
```
o = toc("inst/sampleRDA/list.rda")
```

+ For function/closxp object, record
  + √ Was Returning a data.frame with a row for each parameter, for better or worse.
     + srcref and offset repeated.
	 + now back to one row and list() items for paramNames, hasDefaultValue, no repeats.
	 + either approach works.
  + √ # args
  + √ has ...
  + √ names of parameters


+ EXTPTRSXP =    22  
      + √ fixed - with attributes - not supposed to put them on the externalptr itself, but legal.
         + `z = toc("inst/sampleRDA/externalpointer_withAttrs.rda")`
         + finishes the file but has 2 entries.
		 + load() has ptr with an attribute named bob with a value of 1
		 + toc() gives 2 elements
      + had to look at serialize.c to see what should happen.
	     + read attributes after the switch() for many types.

1. Example objects to test.
   + √ CLOSXP with no default values, attributes, ...
   + √ calls with missing arguments - "../inst/sampleRDA/call_missing_arg.rda"



## Validate Package Code

+ √ Find all functions in this package's code that have a depth parameter and 
     find all calls to those functions that don't include the depth.
   + see depthArg.R

## Done (?)

1. √ Do we handle NAs in character vectors. 
    + Written as -1  and no value, so the length is -1 and we don't read

1. √ name for object in simpleFunction.rda  Seems to be 1,  not simpleFun.  
      + This is how it is printing when there is only one object.  Fix when combine all the columns.
	    + simpleFun is the name of the element from toc(). 
		+ print.RDAToc is putting a rowname of 1, nothing to do with the 1 in the body of the function.

+ √ References for ENVSXP.

+ √ make [.RDAToc open the file once, read the objects in order of smallest offset and jump to the
  next amount.
    + See [.RDAToc and readVariables()

+ √ When read the entire file in toc mode, then access an object, the second read/pass will add references that are already
  in the references.
  + They don't do any harm, but are unnecessary.
     + the second "pass" still uses the original indices so finds the original  insertions not the new ones.
  + perhaps toc() "locks" the environment as it knows it has done a pass over the entire file
      + addRef() would check for this lock add not add new references


+ √ Registering each symsxp twice. and in pairs, i.e., not interspersed by others.
   + solved - readSYMSXP() was calling readTag() and then registering the result. But readTag() was also registering the result.

+ √ combine the data.frames when printing.

+ √ Implement [[ integer for RDAToc.

+ √ Fix putting formals() on the function in readFunction when not skipping value.


