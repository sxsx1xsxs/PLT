import sys
import os

if __name__ == "__main__":
	print ("building the compiler \n")
	os.system("make clean")
	os.system("make all")

	print "producing ir using our compiler \n"
	os.system("./openFile.native test_hello_world.of > test.ir")

	print "generating assembly code using llc"
	os.system("llc-3.7 test.ir")

	print "generating executable using gcc \n"
	os.system("gcc -o test test.ir.s")

	print "executing openFile test program, the following is the output of the program: \n"
	os.system("./test")
