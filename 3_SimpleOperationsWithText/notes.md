# Simple Operations with Text

### Chapter Objectives

- Introduce the concept of types, and work through an introductory data-structure called `String`

- Illustrate with examples the syntactic sugar used with strings

- Work with strings and their associated functions in the REPL environment

### First look at types

- types are a categories of values. We find out the type of a value, expression or function in GHCi with `:type`

- In Haskell, `::` between two expressions denotes that the former "has the type of" the latter

- There does not exist a strict "string" type in Haskell. What exists is a _type alias_ for a list of `Char`. 

- the `print` function is not specific to strings, but can be used to print a string to the display

- `putStrLn` and `putStr` are print functions that are specific to string. They both print without quotation marks around the text (unlike `print`) and the former prints with a newline character appended at the end while the latter doesn't.

  

------
### A quick detour into project structure

When we build an executable or run a project in REPL, Stack takes the source files in the `src` folder in the project. `main` is the default action, and is not a function but a series of instructions to execute. It does not mean however that we cannot load other source files in GHCi without a `main` block within our source files when working on other scopes.

​	Because we typically require the `main` block to provide some form of interactivity to the user, it has the type `IO` 



------

