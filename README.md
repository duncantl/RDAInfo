## Table of Contents of an R .rda File

The idea is to be able to read an R data file created via a call to save() without loading 
the R objects but providing a description of the top-level variables it contains.


This is the start of an R-code implementation of reading an XDR-formatted RDA (rdata) archive.
Since this is already implemented fully and efficiently in C (serialize.c), this version will not be
competitive.
There are however several motivations:
1.see how to implement a version that does not read the  values or create the R objects, but
  provides a summary of the top-level objects:
    + names of variables
	+ length and/or dimensions
	+ types & classes
	+ whether they have names or not

 Perfecting this in R code will make it easier to implement correctly in the C code.

1. If we can make this sufficiently competitive with the C code because, while it is slower in R it
   does less (not allocating memory, not interpreting certain types of bytes), then this can be
   faster for large but simple RDA files. (This basically means vectors and lists of vectors that
   don't contain large character vectors, either as vectors or attributes.)
   
1. A slow R implementation may serve as an interesting example for compiling R code to native
   instructions, e.g., the Rllvm approach I am experimenting with (RLLVCompile, etc.)

1. This would allow us to experiment with a slight enhancement/extension to the RDA format to
   provide 
   + a built-in table of contents/summary of its contents
   + direct access to individual objects in the RDA file by name without having to read or restore the
     preceding objects, but instead jumping directly to their starting position.

2. This package does provide 2 simple routines to deal with XDR int and double values which is
   functionality that doesn't appear to be in any CRAN package.


In other words, this is an R prototype of something we might consider implementing in C, and
specifically, adapting the code in serialize.c (and saveload.c) in the R source.
It is not intended to be a production-level solution that will work best in all cases. 
√ We did ~~may well~~ implement the most significant bottleneck in C, that is reading large
character vectors. This nows makes the R implementation faster than load()'ing all the objects, at
least for cases of a collection of large vectors.

While I originally thought this table-of-contents functionality would be useful about 20 years ago, 
the current motivation grew from something I am writing as a case study in "Hacking the R engine" with the hope of
helping people learn to experiment with and contribute to the R source code.


## Current Status
While we wrote this only today and haven't tested it much, this currently
+ Reads 
   + all vectors, matrices, lists, data.frames
   + attributes

+ Does not YET handle
   + functions
   + language objects generally
   + Environments
   + S4 objects
   + Byte code


##  Performance



## For an RDA file with 4 variables, each of length 1e7 (but separate variables)

Contents of the RDA file are 4 vectors, each of length 1e7
  + numeric
  + numeric
  + numeric
  + factor
268M uncompressed

On a Macbook Pro (32Gb RAM) 

+ load: .775 seconds
+ Table of Contents: .007 seconds 
+ Factor:  110.7
(Medians over 5 runs)


## Character vector rather than factor

If we change the factor to a character vector, the results are not even close.
The original R code in this package read each string in a character vector with three R calls;
+ consume the CHARSXP integer
+ read the number of characters/bytes
+ skip that number of bytes
So we read the entire character vector with
```r
nc = replicate(len, { readInteger(con); nchar = readInteger(con); seek(con, nchar, "current"); nchar})
```
The R interpreter overhead is enormous. We could change the first call to readInteger() to `seek(,
4L)` as we discard the value.  However, this won't change the overall timing sufficiently to make
this approach competitive.

However, switching to a C routine that does the same thing (in `src/eatCharacterVectorEelements.c`)
does.

On a Macbook Pro (32Gb RAM) 

+ load: 3.6 seconds
+ Table of Contents: 1.8 seconds 
+ Speedup factor:  2.0
(Medians over 5 runs, elapsed time)

This doesn't take into account that the `load()` version has used a significant amount of memory
and will have to be garbage collected. (Nor has it summarized the variable types.)


These two variations show the efficiency of factors in representing a large character vector with a small
number of unique values.

