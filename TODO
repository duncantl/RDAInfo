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


+ [ok but could be better]  seek() on a gzfile to an offset is not working reliably. 
  + Fine for uncompressed save() files.
  + This may not be possible, or we have to go in smaller steps.
      + https://stackoverflow.com/questions/30834963/seeking-on-a-gz-connection-is-unpredictable
      + explore if this is something we can "fix" in R or is it a characteristic of gunzip.
  + We can read from the start to the offset and discard what we read just to get to the offset.
+ References for ENVSXP.

+ TEST SUITE - create

+ [test] Capture the references.
   + Need to them for √ symsxps, √ environments, PACKAGESXP, PERSISTSXP, NAMESPACESXP, EXTPTRSXP, WEAKREFSXP, ....
   + √ See Expression.rda.  The second call has "20" where a should be.


+ Handling character encoding.
   + The note in serialize.c "  strings without an encoding flag will be converted to the current native  encoding" indicates that the CHARSXP?/STRSXP may contain an encoding instruction.
   + See ReadChar in serialize.c.  The encoding is in the levs flag.

+ References at the toplevel
   + save(a = obj,  b = obj) - same object.
   + capture concept that restoring b means binding to the value of a.

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


