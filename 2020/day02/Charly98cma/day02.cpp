#include <stdio.h>

#define PWD_LEN 80

int main() {
  FILE * myFile;
  int i, min, max, counterLetters;
  int counter1st, counter2nd;
  char letter, pwd[PWD_LEN];

  if (!(myFile = fopen("input.txt", "r"))) {
    printf("File missing!");
    return 1;
  }

  counter1st = 0;
  counter2nd = 0;

  /* line -> 1-3 a: abcde */
  while(fscanf(myFile, "%d-%d %c: %s", &min, &max, &letter, pwd) != EOF) {
    for(i=0, counterLetters=0; i < PWD_LEN && counterLetters<=max; i++) {
      if (pwd[i]=='\0') break;
      if (pwd[i]==letter) counterLetters++;
    }
    if (counterLetters >= min && counterLetters <= max)  counter1st++;
    if ((pwd[min-1] == letter) ^ (pwd[max-1] == letter)) counter2nd++;
  }
  fclose(myFile);

  printf("1st STAR RESULT = %d\n", counter1st);
  printf("2st STAR RESULT = %d\n", counter2nd);

  return 0;
}
