# PLT
Course Project in COMS4115 Spring 2018


Xingjian Wu (xw2534)
Binwei Xu (BX2157)
Yi Zhang (yz3206)
Guanming Qiao (gq2135)
Chunlin Zhu (cz2487)

The "test_hello_world.of" contains the OpenFile hello world testing program.

Steps to run test for OpenFile hello world program:
        1. make clean
        2. make all
        3. /openFile.native test_hello_world.of > test.ir
        4. llc-3.7 test.ir
        5. gcc -o test test.ir.s
        6. /test

For the fourth step, "llc-3.7" should be replaced by whatever version of llc installed in the testing computer.

We wrote a python script (python2) to automate the steps above. Again, if the version of llc is not 3.7, then 
in the script, line 13 should be updated to the version actually installed in the testing computer.

By running "python toplevel.py", the output of the testing program will be on the bottom, which should equal to
"Hello World"





