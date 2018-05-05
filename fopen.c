#include<stdio.h>

char *myFile( char * str, int length) {
	char text[length];
	FILE *fp = fopen(str, "r");
	int i=0;
	while(feof(fp)){
     text[i++] = fgetc(fp);
	}
	text[i]='\0';
	return text;
}

