# Relatively Good Language

---

## Compiling a RGL program

The following command will compile and execute the file "fib.rgl" that is in the folder "examples".

```cmd
..\Relatively-Good-Language> stack build
..\Relatively-Good-Language> stack run "./examples/fib.rgl"
```

## Language Syntax

Statements in RGL are separated by a new line. The language also requires that you put spaces between operators.

### Variables

The language supports two types of variables - integers and booleans.

```
int i = 0
int j = i + 10
bool b = true
bool c = 1 > 2
```

### Operations

The language supports the following arithmetic operations: addition (+), subtraction (-), and multiplication (*). It also support the following boolean operations: smaller than (<), larger than (>), smaller or equal to (<=), larger ot equal to (>=), equality (==), not equal (!=), boolean "or" (||), and boolean "and" (&&).

### If statements

The "if" statements work as in any other language but they use different keywords.
| Original keyword | RGL keyword|
| ----------- | ----------- |
| if | whatIf |
| else if/elif| butWhatIf |
| else | else|

```
int i = 0
whatIf(i == 0){
    print(i)
}
butWhatIf(i == 2){
    print(5)
}
else {
    print(10)
}
```

### While loops

While loops works as expected but require you to be more polite towards the language.

```
int i = 0
pleaseDoWhile(i < 10){
    i = i + 1
    print(i)
}
```

### Concurrency

The language support parallel blocks and sequential blocks inside those parallel blocks. It also support locks and shared variables.
Due to limitations in the shared memory the language supports up to four parallel blocks and up to four locks but for each parallel block used one less lock can be used. The programmer needs to keep track of how many parallel blocks are used so that they can use the appropriate lock.
In the following example the parallel block (allAtOnce) uses a lock so that it can synchronize the threads after they are done. That is why the first available lock is 1 (instead of 0). The user can use locks 2 and 3 but they were not required for the program.

```
shared i = 0
allAtOnce{
    hangingByAThread{
        doNotTouchThis(1)
        i = i + 10
        print(i)
        nowYouCanTouchThis(1)
    }
    hangingByAThread{
        doNotTouchThis(1)
        i = i + 5
        print(i)
        nowYouCanTouchThis(1)
    }
}
print(i)
```

| RGL keyword | Meaning |
| ----------- | ----------- |
| **shared** | Specifies a variables as shared. Multiple treads have access to that variable. |
| **allAtOnce** | Start a parallel block. All statements inside this block will be run in parallel. |
| **hangingByAThread** | Counted as a single statement. Everything inside this block is ran sequntially inside a new thread. *Must* be used in a parallel block. |
| **doNotTouchThis(i)** | Start of a lock "i". If the lock is already in use the thread will halt untill the lock is released. |
| **nowYouCanTouchThis(i)** | Releases lock "i". |

### Print

Printing puts an integer to the I/O. Any expression can be put into the print statement. If the result is an integer it will show it `Sprockell 0 says 55`. If the result is a boolean it will print `Sprockell 0 says 1` for `True` and `Sprockell 0 says 0` for `False`.

### Comments

Two types of comments are supported - single line (%) and multiline (%- -%)

```
% This is a single line comment
%-
 This is a multiline comment
 -%
```

## Additional notes

### First line in the file

Files that do not start with a statement will break (this also includes comments). Examples for such programs are shown below.

```


int i = 0
```

```
% This comment explains the variable but is the first line in the file
int i = 0
```

### Booleans in if statements and while loops

In if statements and while loop the conditions cannot be just a single boolean variable. The variable needs to be compared to the appropriate value.

```
bool b = true
whatIf(b == true){
    % Perform code here
}
```
