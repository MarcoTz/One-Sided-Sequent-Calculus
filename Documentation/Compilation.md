# Compiling Programs
 
As of now, the only way to run a program is using the web interface, found in the web-app directory. 
Using `make build` compiles the purescript code to javascript in the `index.js` file. 
When opening `index.html` in the browser the interface will come up and code can be run by entering it in the given text area.

# The Compilation Pipeline

When a program is run, it uses the `runStr` function in the driver (`one-sided-driver/` folder in the source directory) with the provided module name.
This function then goes through the following steps
 
* runs the parser (`one-sided-parser`) on the loaded text
* infers all imported programs in the found order (recursively, the same way as the current program) and adds them to the environment
* desugars the program (`one-sided-desugar`)
    Currently, the only thing this changes is making sure xtors without arguments are actually xtors and not variables
    For example, the parser will assume `True` is a variable and not a constructor, which is changed during desugaring
* infers all data declarations (`one-sided-inference/src/InferDecl`) and adds them to the environment
    This amounts to making sure all kinds in declarations are correct.
    For example, all type variables used in xtor definitions need to be the one defined as type argument of the declaration
* infers types of all variable declarations and adds them to the environment.
    This is either type inference (which is not implemented yet) or type checking, depending on if a type annotation was provided.
    Type checking only succeeds if all cuts appearing in the term to be checked are either annotated as well, or if each cut contains a variable bound by a Mu-abstaction, xcase or shif
* run the main function if it was provided
    In contrast to other functions, the main function needs to be a command so it can be evaluated.

If the pipeline can be sucessfully executed, the result of evaluating the `main` function is shown in the web app. Otherwise the error message is printed.
