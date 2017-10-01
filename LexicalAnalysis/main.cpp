#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<ctype.h>
int main(){
	FILE* fp=fopen("in.txt","r");
	char c;//存放当前读进的字符
	char C[2];
	enum MemSymbol
	{
		BEGINSY, ENDSY, IFSY, THENSY, ELSESY, IDSY, INTSY, PLUSSY, MINUSSY,
		STARSY, DIVISY, LPARSY, RPARSY, COMMASY, SEMISY, COLONSY, ASSIGNSY, EQUSY
	} symbol;//保存当前识别单词的类型
	char reserver[] = "begin end if then else         ";//保留字
	char token[20];//存放单词的字符串
	int num = 0;//存放当前读入的整型数值
	memset(token, 0, sizeof(token));//cleantoken()
	memset(C, 0, sizeof(C));//cleanC
	c = fgetc(fp);
	C[0] = c;
	while (!feof(fp)) {
		if (isblank(c) || iscntrl(c)) {
			//if (c == '\n') printf("\n");
			memset(token, 0, sizeof(token));//cleantoken()
			memset(C, 0, sizeof(C));//cleanC
			c = fgetc(fp);
			C[0] = c;
			continue;
		}
		//识别是否是标识符
		if (isalpha(c)) {
			while (isalpha(c) || isdigit(c)) {
				strcat_s(token, C);
				c = fgetc(fp);
				memset(C, 0, sizeof(C));//cleanC
				C[0] = c;
			}
			fseek(fp, -1, SEEK_CUR);//指针后退一个字符
			if (strcmp(token, "begin") == 0) {
				printf("BEGINSY ");
			}
			else if (strcmp(token, "end") == 0) {
				printf("ENDSY ");
			}
			else if (strcmp(token, "if") == 0) {
				printf("IFSY ");
			}
			else if (strcmp(token, "then") == 0) {
				printf("THENSY ");
			}
			else if (strcmp(token, "else") == 0) {
				printf("ELSESY ");
			}
			else {
				printf("IDSY(%s) ", token);
			}
		}
		//识别是否是整数
		else if (isdigit(c)) {
			while (isdigit(c)) {
				strcat_s(token, C);
				c = fgetc(fp);
				memset(C, 0, sizeof(C));//cleanC
				C[0] = c;
			}
			fseek(fp, -1, SEEK_CUR);//指针后退一个字符
			for (int i = 0; i < strlen(token); i++) {
				num = num * 10 + ((int)(token[i]) - (int)('0'));
			}
			memset(C, 0, sizeof(C));//cleanC
			memset(token, 0, sizeof(token));//cleantoken()
			printf("INTSY(%d) ", num);
			num = 0;//重置num值
		}
		//识别是否是冒号，或者赋值符号
		else if (c == ':') {
			c = fgetc(fp);
			if (c == '=') {
				printf("ASSIGNSY ");
			}
			else {
				fseek(fp, -1, SEEK_CUR);//指针后退一个字符
				printf("COLONSY ");
			}			
		}
		//识别是否是其他分隔符
		else if (c == '+') { printf("PLUSSY "); }
		else if (c == '=') { printf("EQUSSY "); }
		else if (c == '-') { printf("MINUSSY ");}
		else if (c == '*') { printf("STARSY "); }
		else if (c == '(') { printf("LPARSY "); }
		else if (c == ')') { printf("RPARSY "); }
		else if (c == ',') { printf("COMMASY ");}
		else if (c == ';') { printf("SEMISY "); }
		else if (c == '/') {
			c = fgetc(fp); 
			if (c == '*') {
				//说明是注释
				do {
					do { c = fgetc(fp); } while (c != '*');
					do { c = fgetc(fp); if (c == '/') goto finish; } while (c == '*');
				} while (c != '*');
			}
			else {
				fseek(fp, -1, SEEK_CUR);//指针后退一个字符
				printf("DIVISY ");
			}
		}
		//出现其他非法字符
		else {
			printf("\n------ find illegal symbol! ------\n");
		}

		if ((feof(fp))) {
			break;
		}
		else {
		finish:
			memset(token, 0, sizeof(token));//cleantoken()
			memset(C, 0, sizeof(C));//cleanC
			c = fgetc(fp);
			C[0] = c;
		}
	}
	fclose(fp);
}