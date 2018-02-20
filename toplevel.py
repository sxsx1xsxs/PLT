import sys
import os

if __name__ == "__main__":
    os.system("ocamlbuild -clean")
    os.system("ocamlbuild toplevel.native")

    for i in range (1,6,1):
    	print ("\nPositive test " + str(i)+":")
    	os.system("./toplevel.native tests/test_pos_"+str(i)+".of")
    	print ("\nNegative test " + str(i)+":")
    	os.system("./toplevel.native tests/test_neg_"+str(i)+".of")