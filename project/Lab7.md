# Lab 7: Extension of improved Arguments and Fields

## The task:
What I will be implementing in this extension is primarily the features escribed on Canvas which were as follows:
* Implement named arguments with potential default values. The default values can be arbitrary Punkt0 expressions.
* Implement a mechanism for providing an arbitrary number of arguments to a function.
* Provide Punkt0 classes with an extra copy() method that allows a quick
cloning of an object while specifying which field to modify (similar to
Scala's copy() method for case classes)

## Changes that will be made:
* Named arguments with default values:
This implementation is related to how a method declaration is defined in the grammar of Punkt0. Thus, I will be beginning by updating the Trees. Scala file. Currently _a method declaration accepts for the arguments to be a formal where we have an identifier and its type. This has to then change for the identifier to be able to have a default value. Thereafter, the following phases such as analyzer and code generation, handling the arguments of a method may need to get updated to take into account the default values.
* Arbitrary arguments to a function:
This will be similar to the previous implementation in terms of what needs to change which is the grammar of method arguments being of type Formal. A formal should eb allowed to be a list of arguments packed together. I am considering implementing this feature first allowing for arbitrary arguments, given that they have the same type. If I got that to work successfully, I will change it to allow arbitrary datatypes.
* Copy method for classes:
I am not 100% sure about this yet, I would need to look into it further in detail. But I would say this would also need a to be introduced from the ast phase(trees.scala) onwards, for the copy method to be attached to a ClassDecl. My initial thought is then to change:
case class ClassDecl (...) ➔ case class ClassDecl (...copy: MethodDecl)
And then of course in the following phases afterwards, wherever the class is being handled, the copy method extension needs to be handled as well.

## Suggestions for possible further improvements
I think there is enough that could be improved in the extension proposed above to be just enough for the project timespan. However, in case that was not the case, I will be attempting the Liberal Syntax suggestion on the Canvas page as well.