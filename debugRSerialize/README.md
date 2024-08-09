The code here helps us collect information about the objects in an RDA stream.
We use these with LLDB, the native debugger to collect the values of several
variables in the C routine ReadItem in R's serialize.c code.
This is the routine that determines the type of the next object in the stream
and whether it has attributes, a tag/name, is an S4 object.
We collect this across all calls to ReadItem when loading a single RDA file
and when that is complete, we dump the information to a JSON file and read it into
R to augment it and display it as a sequence of rows.

We capture the information in LLDB using its Python API.


# Instructions

We start by running R under a native debugger, e.g.,
```
~/R_SelectiveLoad/build/bin/R -d lldb
```
and starting an R session (so that it loads all it needs before we start tracing the explicit 
load() call.)


Set a breakpoint in ReadItem after the call to UnpackFlags, so that the values of the variables
have been computed:
```
break set -f serialize.c -l 1790
```


Next, we add Python code to be invoked at this breakpoint
```
breakpoint command add -s python 1
```
(If the break point you created previously is not numbered 1, change the 1 in this command to the
corresponding break point number.)

```{python}
global ReadItemInfo
type = frame.FindVariable("type").GetValue()
objf = frame.FindVariable("objf").GetValue()
levs = frame.FindVariable("levs").GetValue()
depth = frame.GetModule().FindFirstGlobalVariable(frame.thread.process.target, "R_ReadItemDepth").GetValue()
ReadItemInfo.append([type, depth, frame.FindVariable("hastag").GetValue(), frame.FindVariable("hasattr").GetValue(), objf, levs])
thread = frame.GetThread()
process = thread.GetProcess()
process.Continue()
DONE
```

Next, we import the utils  module and create an empty list assigned to ReadItemInfo 
```
script import utils
script ReadItemInfo = []
```
If the call to import fails, the utils.py in this directory is not in your Python path. So either
add it via the environment variable PYTHONPATH before starting the debugger or use 
```
script sys.path.insert(0, '/Users/duncan/path/to/RDAXDR/debugRSerialize')
```


Now we return to the R session and load() the RDA file of interest.
```{r}
load("foo.rda")
```

When that is complete, we return to the debugger and issue the command
```
script utils.write(ReadItemInfo)
```

This writes the JSON version of the ReadItemInfo array to '/tmp/foo.json'. (You can change the name
of the file by specifying it as a second argument in the call.)

Then in an R session (perhaps this one running under the debugger, or a separate one)
```{r}
source("utils.R")
readJS()
```
Again, if you use a different file to write the JSON in the call above, provide that file name in
the call to `readJS()`.


Examples of traces are in [Traces/](Traces)


