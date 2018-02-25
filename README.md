# PLT
Course Project in COMS4115 Spring 2018


Xingjian Wu (xw2534)
Binwei Xu (BX2157)
Yi Zhang (yz3206)
Guanming Qiao (gq2135)
Chunlin Zhu (cz2487)


1.Syntax In-coming:
things related to regex and array implementation


2.How to compile and execute the compiler:
First edit the test.of file for your test, "of" standing for OpenFile file.
Then run:
    ocamlbuild -clean toplevel.native
    ocamlbuild toplevel.native
    ./toplevel.native test.of
The last execution should print a "it passes" if it passes. 
Error states will cause the parser to backtrack and die. 


3.How to run the test script:
"python script.py" (python3) will build the compiler, test against the 10 test cases and then measure whether it's the 
expected result. After running this, you should see the prompt "The result matches what we expected" for all 10 test cases with the output of each test cases, which means the 5 positive tests all pass and the 5 negative tests all fail. 






***************The following is for our own record******************************** 

To debug parser, refer to the state where the parser began to backtrack from parser.output file, which contains all states in the parser. 
To disable printing of state transitions when parsing, run 
    export OCAMLRUNPARAM='' in your terminal. 
To reenable it, run 
    export OCAMLRUNPARAM='p'
To generate new .output file for parser, use ocamlyacc -v parser.mly

