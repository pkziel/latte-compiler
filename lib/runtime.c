#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printInt(int x) {
   printf("%d\n", x);
}

void printString(char* s) {
   printf("%s\n", s);
}

void error() {
   printf("runtime error\n");
   exit(1);
}

char* readString() {
    char * line = NULL;
    size_t size, last;
    getline(&line, &size, stdin);
    if (line[strlen(line) - 1] == '\n') 
        line[strlen(line) - 1] = '\0';
    return line;
}

int readInt() {
    int x;
    scanf("%d", &x);
    readString(); // read the rest of the line without this test doesn't work
    return x;
}

char* concat(char* s1, char* s2) {
   char* new_ = malloc(strlen(s1) + strlen(s2) + 1);
   return strcat(strcpy(new_, s1), s2);
}

// int main () {
//     int x = readInt();
//     char* s1 = readString();
//     char* s2 = readString();
//     printInt(x);
//     printString(concat(s1, s2));
//     error();
// }