import sys
import os
import subprocess

if __name__ == "__main__":
    os.system("ocamlbuild -clean")
    os.system("ocamlbuild toplevel.native")

    for i in range (1,6,1):
    	print ("\nPositive test " + str(i)+":")
    	output1 = os.popen("./toplevel.native tests/test_pos_"+str(i)+".of").read()
    	if "it passes" in output1:
    		print (output1.strip())
    		print ("The result matches what we expected")
    	else:
    		print ("Wrong result")

    	print ("\nNegative test " + str(i)+":")
    	output2 = os.popen("./toplevel.native tests/test_neg_"+str(i)+".of").read()
    	if "it passes" not in output2:
    		print ("The result matches what we expected")
    	else:
    		print (output2.strip())
    		print ("Wrong result")