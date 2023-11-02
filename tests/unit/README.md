Unit tests
==========

Unit tests are grouped into directories according to what I felt like. The
groups doesn't actually do anything (except allow easier running of only some).

Each test file is written as a module declaration, since that make Guile
slightly happier, and should end with a quoted list of modules which are
explicitly tested in that file. Later, the coverage data from that test 
will be limited to the modules specified. This gives *much* better coverage 
data, and ensures that we actually cover all procedures with tests, instead
of them being covered by some test "higher up" in the chain.
