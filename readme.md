Ion Parse
=========

Ion parse is build on top of gobble, a parser system that I have been creating.

The aim is to create code that looks like a grammar even though it is standard rust code.

It is close, but not quite as close as I'd like.  The biggest advantage of this is that there is very little magic to follow as you read/build it up.

This parser works instruction at a time. So unlike many parsers that would begin a block when a for loop is started, this one decides to just return a ForLoop  statement at the beginning of the loop.  It is therefor up to the reader to look for the next end block and handle all of the cases between.  Doing this should make the repl easier to implement.

The primary target of the parser is a Statement that completes with either a line break or a semicolon. 

To test out the parser run the bin ```ion_live```, and try out different expressions, the REPL behaviour should just be to show you what it has resolved those expressions to.
