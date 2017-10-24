#define _CRT_SECURE_NO_WARNINGS
#include<stdio.h>
#include<string.h>
int address = 0;
int t = 0;
char c;
int a; int b;
FILE* fp = fopen("in.txt", "r");
FILE* f = fopen("out.txt", "w");
int proc_E();int proc_T();int proc_F();
int add(int a, int b);
int mult(int a, int b);
int main()
{	
	int result = 0;
	c = fgetc(fp);
	proc_E();
	fclose(fp);
	fclose(f);
	return 0;
}
int proc_T() {
	int result = proc_F();
	while (c == '*') {
		c = fgetc(fp);
		b = proc_F();
		result = mult(result,b);
	}
	return result;
}
int proc_F() {
	int result;
	if (c == '(') {
		c = fgetc(fp);
		result = proc_E();
		if (c == ')') {
		}
	}
	else {
		printf("Allocate  %c  at : @%d\n", c, address);
		result = address;
		address = address + 4;
	}
	c = fgetc(fp);
	return result;
}
int proc_E() {
	int result = proc_T();
	while (c == '+') {
		c = fgetc(fp);
		a = proc_T();
		result = add(result, a);
	}
	return result;
}
int add(int a, int b) {
	printf("Allocate  temp%d at : @%d\n", t, address);
	fprintf(f,"ADD \t@%d\t@%d\t@%d\n", a, b, address);
	int result = address;
	address = address + 4;
	t = t + 1;
	return result;
}
int mult(int a, int b) {
	printf("Allocate  temp%d at : @%d\n", t, address);
	fprintf(f,"MULT\t@%d\t@%d\t@%d\n", a, b, address);
	int result = address;
	address = address + 4;
	t = t + 1;
	return result;
}