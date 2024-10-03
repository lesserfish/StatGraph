# Contributing to the codebase
## Guidelines

All .R files should be located in this directory, not in any subdirectory.

All public methods (those which are marked with a @export tag) should be contained in their own files.

Private auxiliary methods should

* Be added to common.R if they are used by more than one method.
* Be added to the file of the public method which uses it, it no other method invokes it

If the auxiliary method is too large, it can be added to a new file. The name of the new file should be chosen with common sense in mind.
