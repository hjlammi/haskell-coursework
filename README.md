## haskell-coursework

# Running the program
In the root folder compile the program with the command:
`ghc --make Main.hs`
Then run the program with the command:
`./Main test.txt`

Type question for the program to answer. The program can be exited by typing 'quit'.

# Unit tests
The program was developed following test-driven development style. There are unit tests in three files which can be run with the following commands:
`runhaskell test/MainSpec.hs`
`runhaskell test/ParserSpec.hs`
`runhaskell test/PersonSpec.hs`
