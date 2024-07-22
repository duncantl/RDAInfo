SEXPMap = c(
 NILSXP = 	     0	  ,
 SYMSXP = 	     1	  ,
 LISTSXP = 	     2	  ,
 CLOSXP = 	     3	  ,
 ENVSXP = 	     4	  ,
 PROMSXP = 	     5	  ,
 LANGSXP = 	     6	  ,
 SPECIALSXP =    7	  ,
 BUILTINSXP =    8	  ,
 CHARSXP = 	     9	  ,
 LGLSXP = 	    10	  ,
 INTSXP = 	    13	  ,
 REALSXP = 	    14	  ,
 CPLXSXP = 	    15	  ,
 STRSXP = 	    16	  ,
 DOTSXP = 	    17	  ,
 ANYSXP = 	    18	  ,
 VECSXP = 	    19	  ,
 EXPRSXP = 	    20	  ,
 BCODESXP =     21    ,
 EXTPTRSXP =    22    ,
 WEAKREFSXP =   23    ,
 RAWSXP =       24    ,
 OBJSXP =       25,   # Same as S4SXP
 S4SXP =        25    ,
 NEWSXP =       30    ,
 FREESXP =      31    ,
 FUNSXP =       99    ,
 
 ###
 ### From serialize.c
 ### 
 REFSXP =             255,


 # NILVALUE, EMPTYENV, BASEENV, GLOBALENV, UNBOUNDVALUE, MISSINGARG, BASENAMESPACE
 # used in SaveSpeciakHook.
 
 NILVALUE_SXP =       254,
 GLOBALENV_SXP =      253,
 UNBOUNDVALUE_SXP =   252,
 MISSINGARG_SXP =     251,
 BASENAMESPACE_SXP =  250,
 NAMESPACESXP =       249,
 PACKAGESXP =         248,
 PERSISTSXP =         247,
 CLASSREFSXP =        246,
 GENERICREFSXP =      245,
 BCREPDEF  =       244,
 BCREPREF  =       243,
 EMPTYENV_SXP = 	  242,
 BASEENV_SXP =  241,

 # for attributes on expressions in the byte code constant pool 
 ATTRLANGSXP  =  240,
 ATTRLISTSXP =  239,

 
 ALTREP_SXP =  238
 
)

sexpType =
function(val)    
  names(SEXPMap)[match(val, SEXPMap)]
