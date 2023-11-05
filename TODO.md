1. TEST SUITE - create
   + We have example rda files 
   + we can use toc() and lload() for each and then compare the results.
      + when getting the element from the toc(), make certain to unclass to avoid [[.RDAToc
	  + but can also compare the results by getting the object.

1. Check examples
```
rdas = list.files("../inst/sampleRDA", pattern = "\\.rda", full = TRUE)
tmp = structure(lapply(rdas, function(x) try(toc(x))), names = rdas)
err = sapply(tmp, inherits, 'try-error')
table(err)
```
   + Currently errors for 1 of 32
```
[5] "../inst/sampleRDA/trimws_function.rda"  # BCODESXP not implemented.
```

1. 18 SEXP types remaining
  +  BCODESXP =     21    
  + √ S4SXP =        25    
  + check SPECIALSXP =    7	  
  + check  BUILTINSXP =    8	  
  + √ CLOSXP = 	     3	  
  + PROMSXP = 	     5	  
  + DOTSXP = 	    17	  
  + WEAKREFSXP =   23    

  + Not mentioned in serialize.c
     + X ANYSXP = 	    18	    shouldn't see these
     + X NEWSXP =       30    
     + X FREESXP =      31    
     + X FUNSXP =       99    

  + √ EXPRSXP = 	    20	    check
  + √ VECSXP = 	    19	  
  + √ LANGSXP = 	     6	  
  + √ ENVSXP = 	     4	  
  + √ SYMSXP

1. Change in R-devel for OBJSXP and not S4SXP

1. BCODESXP
   + "inst/sampleRDA/trimws_function.rda"
   + "~/OGS/SUForm/ProcessForm/June8.rda"
   
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

+ NA for type - of what readType()?
   + "~/Davis/ComputerUsage/jobs.rda"
   +  now j_jobs.rda
   + type appears as 48.
      + Run this via LLDB and our output of the sexp type, depth, hastag, hasattr.
	     + want this to be stored in memory or written to a file. It is over 100K lines.
   + Read jobs.rda into R via lload(), then assign each of the two elements (j and k) to variables
     and save() each of them. Then toc() each of the rda files.
	    + fails for j - same error.  See j_jobs.rda
		+ okay but very slow for k  (5.6 seconds and k is a 39573 x 15 data.frame with 12 character
          vectors, 1 factor, 1 list (of character vectors) and 1 POSIXlt (not ct) columns)

+ Look at the .RData files
   + ~/OGS/PRCC/GTTP/.RData - seg faults.
   + ¿ A large vector whose length we get as a shorter integer and so is wrong.
   + Seg faulting in R_eatCharVectorElements
   + X Is this a different RDA format? NO
   + The file is 38734 bytes
   + The totalChars at the segfault is 113744 and so is nchars
   + The .RData conains 11 functions which capture the R_GlobalEnv.

   
+ "~/Personal/CV-orig/packageMetaInfo.rda"
     + result from file.info() x 2 (??)

+ "~/Personal/fbLogin.rda"
     + give error for `names(ans)[name] = tag` in readPairList()
     + same error as for S4 file above but not necessarily related.


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


