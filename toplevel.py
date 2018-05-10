import sys
import os


def compile_exe(test_file):
    print ("producing ir using our compiler \n")
    os.system("./openFile.native "+test_file +" > test.ir")

    print ("generating assembly code using llc")
    os.system("llc-3.7 test.ir")

    print ("generating executable using gcc \n")
    os.system("gcc -o test test.ir.s fopen.o fwrite.o")

    print ("executing openFile test program, the following is the output of the program: \n")
    os.system("./test")

if __name__ == "__main__":
    print ("building the compiler \n")
    os.system("make clean")
    os.system("make all")

    if len(sys.argv) > 1:
        test_file = sys.argv[1]
        if test_file == "tests" or test_file == "tests/":
            for f in os.listdir(test_file):
                compile_exe("tests/"+f)
        else:
            compile_exe(test_file)
