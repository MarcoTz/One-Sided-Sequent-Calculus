# Compiling Programs

In the current implementation, the only way to run a program is to have the `.os` file available in the `Examples` folder in the root of the repostitoy.
To compile a program use `cabal run main Name` where `Name` is the module name of the program. 
While the program is compiled, there will be debug output after each step.
Alternatively, use `cabal run test-suite` to compile all programs in the `Examples` and `Counterexamples` (which should all fail) folders, in which case no debug output will be produced, only the results for all examples.

# The Compilation Pipeline

When a program is run using either the `main` or `test-suite` application, it uses the `inferModule` function in the driver (`Driver/` folder in the source directory) with the provided module name.
This function then goes through the following steps

* load the contents of the corresponding file (if it was found)
* runs the parser (`src/Parser`) on the loaded text
* loads the contents of all imported modules and parses them
* finds the order in which to infer the imports (such that no dependencies are missing) 
    This is currently still work-in-progress, cyclic imports are not detected.
* infers all imported programs in the found order (recursively, the same way as the current program) and adds them to the environment
* desugars the program (`src/Desugar`)
    Currently, the only thing this changes is making sure xtors without arguments are actually xtors and not variables
    For example, the parser will assume `True` is a variable and not a constructor, which is changed during desugaring
* infers all data declarations (`src/TypeInference/InferDecl`) and adds them to the environment
    This amounts to making sure all polarities in declarations are correct.
    For example, all type variables used in xtor definitions need to be the one defined as type argument of the declaration
* infers types of all variable declarations and adds them to the environment.
    This is either type inference (which is not fully implemented yet) or type checking, depending on if a type annotation was provided.
    Type checking only succeeds if all cuts appearing in the term to be checked are either annotated as well, or if each cut contains a variable bound by a Mu-abstaction, xcase or shift.
