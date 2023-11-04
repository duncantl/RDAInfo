+ 
```
rdas = list.files("../inst/sampleRDA", pattern = "\\.rda", full = TRUE)
tmp = structure(lapply(rdas, function(x) try(toc(x))), names = rdas)
err = sapply(tmp, inherits, 'try-error')
table(err)
```
   + Currently errors for 2 of 32
```
[4] "../inst/sampleRDA/S4_MyClass.rda"              # attempting to put tag as names() element on empty ans list
[5] "../inst/sampleRDA/trimws_function.rda"         # BCODESXP
```
+ S4 - putting the tag on an empty ans in readPairList `names(ans)[name] = tag`

+ In addDescAttrs(), ignore values that have length 0.
   + See "../inst/sampleRDA/dotsFunction.rda"            

+ In readEnvironment(), wrap the ans$names value in a list() since a data.frame with 1 row.

+ OBJSXP

+ regular list - 
  + add the names
  + the type of each element
  + the length of each element?
  + See inst/sampleRDA/list.rda

+ Do we want the attributes on an expression
  + See inst/sampleRDA/Expression.rda and srcref, wholeref

+ 18 SEXP types remaining
√ EXPRSXP = 	    20	    check

 BCODESXP =     21    
 S4SXP =        25    
 
check SPECIALSXP =    7	  
check  BUILTINSXP =    8	  

 CLOSXP = 	     3	  

 PROMSXP = 	     5	  
 DOTSXP = 	    17	  
 ANYSXP = 	    18	    shouldn't see these
 WEAKREFSXP =   23    
 NEWSXP =       30    
 FREESXP =      31    
 FUNSXP =       99    

√ VECSXP = 	    19	  
√ LANGSXP = 	     6	  
√ ENVSXP = 	     4	  
√ SYMSXP



+ BCODESXP
   + "~/OGS/SUForm/ProcessForm/June8.rda"
   
+ ALTREP_SXP
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
	  
   
   
+ NA for type
   + "~/Davis/ComputerUsage/jobs.rda"
   + type appears as 48.
      + Run this via LLDB and our output of the sexp type, depth, hastag, hasattr.
	     + want this to be stored in memory or written to a file. It is over 100K lines.

+ Look at the .RData files
   + ~/OGS/PRCC/GTTP/.RData - seg faults.

+ Problem Files
  + "~/Personal/CV-orig/packageMetaInfo.rda"
     + result from file.info() x 2
  + √ "~/Personal/fbLogin.rda"


+ √ Find all functions that have a depth parameter and find all calls to those functions that don't include the depth.
   + see depthArg.R
   
   
+ TEST SUITE - create

+ References at the toplevel
   + save(a = obj,  b = obj) - same object.
   + capture concept that restoring b means binding to the value of a.



+ [test] Capture the references.
   + Need to them for √ symsxps, √ environments, PACKAGESXP, PERSISTSXP, NAMESPACESXP, EXTPTRSXP, WEAKREFSXP, ....
   + √ See Expression.rda.  The second call has "20" where a should be.


+ Handling character encoding.
   + The note in serialize.c "  strings without an encoding flag will be converted to the current native  encoding" indicates that the CHARSXP?/STRSXP may contain an encoding instruction.
   + See ReadChar in serialize.c.  The encoding is in the levs flag.

+ Example objects to test.
   + CLOSXP with no default values, attributes, ...
   + calls with missing arguments
   + [leave] name for object in simpleFunction.  Seems to be 1,  not simpleFun.  This is how it is printing when there is only one object.  Fix when combine all the columns.

+ get file name from the connection
   + summary(con)$description
   + add it to the RDAToc object returned by toc() if we start with a connection.
     + already do add file so that would be the connection and we can reopen it
       if we are using it in, e.g., [[ or [

+ Test
   + EXTPTRSXP =    22    ,
      + with attributes - not supposed to put them on the externalptr itself, but legal.
         + `z = toc("inst/sampleRDA/externalpointer_withAttrs.rda")`
         + finishes the file but has 2 entries.


+ FIX:  doesn't work.
```
f = "inst/sampleRDA/class_named_integer_logical_character_uncompress.rda"
info = toc(f)
sapply(info, function(x) x$offset)
sapply(unclass(info), function(x) x$offset) # works
```
  The lapply is using the [[ and is deserializing the individual object!
  + A method for lapply()? for RDA.toc

+ [ok but could be better]  seek() on a gzfile to an offset is not working reliably. 
  + Fine for uncompressed save() files.
  + This may not be possible, or we have to go in smaller steps.
      + https://stackoverflow.com/questions/30834963/seeking-on-a-gz-connection-is-unpredictable
      + explore if this is something we can "fix" in R or is it a characteristic of gunzip.
  + We can read from the start to the offset and discard what we read just to get to the offset.


## Check

+ √ factor() example.
   + see toc("inst/sampleRDA/factor.rda")

## Done (?)

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


