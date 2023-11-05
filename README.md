## Table of Contents of an R .rda File

The purposes of this package are to be able to deal with large R data (rda) files
created via a call to save() **without loading** the R objects but 

+ providing a description of the top-level variables it contains without restoring the objects,
+ allowing deserializing one or a subset of the variables without restoring the others

The following is an example of its use:
```r
rda.file = system.file("sampleRDA/class_named_integer_logical_character_uncompress.rda", package = "RDAInfo")
tc = toc(rda.file)
```
```
file: path/to/class_named_integer_logical_character_uncompress.rda
encoding: UTF-8

      type length   class offset
i   INTSXP      4 Example     32
l   LGLSXP      5    <NA>    195
let STRSXP      7    <NA>    240
```

We can extract an individual variable with, e.g.,
```r
tc[[ "let" ]]
tc$let
```
```
[1] "a" "b" "c" "d" "e" "f" "g"
```

And similarly,  `tc[ c("l", "let") ]` or `tc[ c(2, 3) ]`




##

A more recent example that takes the union of the fields in the description of each top-level object
in the RDA:
```r
f = system.file("sampleRDA", "test.rda", package = "RDAInfo")
info = toc(f)
info
```
```
file: /Users/duncan/Rpackages4/RDAInfo/sampleRDA/test.rda
encoding: UTF-8

      type length      class    names                         name offset numParams                   srcref   dim hasRowNames         colInfo
a  REALSXP     10       <NA>    FALSE a, b, c, d, e, f, g, h, i, j     32        NA                       NA    NA          NA              NA
b  REALSXP     10       <NA>    FALSE                           NA    260        NA                       NA    NA          NA              NA
m   INTSXP      1       <NA>    FALSE                           NA    365        NA                       NA    NA          NA              NA
f   CLOSXP     NA       <NA>       NA                           NA    394         1 1, 5, 1, 33, 5, 33, 1, 1    NA          NA              NA
mx REALSXP     10       <NA>    FALSE                           NA    901        NA                       NA  5, 2          NA              NA
d   VECSXP      2 data.frame a, group                           NA   1046        NA                       NA 10, 2        TRUE REALSXP, INTSXP
```

From this, we see 

+ the class on the data.frame `d`
+ whether it has row names
+ the column names and types for the data.frame
+ the dimensions for the data.frame and the matrix
+ the length of each object, except the function
+ the number of parameters in the function signature
+ the srcref is not very relevant.

We have the offset in the file at which each of the objects starts.

## Marginally improved load()

load with an option to only assign a subset is a minor help in not overwriting existing variables
```r
Load('filename.rda', vars = c('a', 'b'))
```
This restores all the variables in the Rda file into a new environment and then 
assign those in vars to the target `envir`.
This just avoids the caller having to create the new environment and then assigning this subset of
variables. It does allow us to map the variable names to something different, e.g.,
```r
Load('filename.rda', vars = c(x = 'a', y = 'b'))
```
would create variables x and y, copying a and b to those variable names in the target environment.

And of course, if vars is empty, this would be the same as calling `load()`.



## Background, Context & Motivation


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
While we wrote this only today and have now updated it a little, and we haven't tested it much, this currently
+ Reads 
   + all vectors, matrices, lists, data.frames
   + attributes
   + functions
   + language objects generally
   + environments

+ Does not YET handle
   + S4 objects
   + Byte code


##  Performance


### For an RDA file with 4 variables, each of length 1e7 (but separate variables)

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

(Medians over 5 runs, elapsed time.)

This doesn't take into account that the `load()` version has used a significant amount of memory
and will have to be garbage collected. (Nor has it summarized the variable types.)

These two variations show the efficiency of factors in representing a large character vector with a small
number of unique values.


### 

I (programmatically) scanned my drive for all .rda files. The largest of these contains a single
object which is a list  with 785 elements.

Loading the file takes 3.92 seconds. Reading the table of contents takes .054.

This should have been an RDS file. The name did tell me what it contained - SmallSampleTrainDist.

(A good point is: if this is the largest file you have, just load it to see what it contains; you
get more information.  Well, I used to have much larger files!)


## Understanding the RDA XDR Stream

The code in [R/readRDA.R](R/readRDA.R) reads the XDR stream.
It identifies the different SEXP objects and processes their contents.

A [tool](https://github.com/duncantl/LLDB_Scripting) that prints the SEXP type and the flags for the
given SEXP element is useful for seeing the (hierarchical) structure of the stream.
It uses the LLDB debugger to collect the information from the R serialize.c code and display it on
the console (or collect it in a Python object.)



