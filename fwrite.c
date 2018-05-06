#include<stdio.h>
#include<stdlib.h>

char *writefile( char* fname, char* fcontent, int length) {
    FILE *fp=fopen(fname, "w");
    fprintf(fp,"%s\n", fcontent);
    fclose(fp);
    return fcontent;
 }
