#include<stdio.h>
#include<string.h>

int main(){
  char a[5];
  FILE * ptr = fopen("word5","r");
  FILE * ptr1 = fopen("word5new","w");
  fscanf(ptr,"%s",a);
    while(ptr!=NULL&&!feof(ptr)){
    if(strlen(a)==5){
      fprintf(ptr1,"%s\n",a);
    }
    fscanf(ptr,"%s",a);
  }
  fclose(ptr);
  fclose(ptr1);
  return 0;
}
