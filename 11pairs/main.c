#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

typedef uint64_t SNAKEVAL;

extern SNAKEVAL our_code_starts_here() asm("our_code_starts_here");

const uint64_t BOOL_TAG   = 0x0000000000000001;
const SNAKEVAL BOOL_TRUE  = 0xFFFFFFFFFFFFFFFF; // These must be the same values
const SNAKEVAL BOOL_FALSE = 0x7FFFFFFFFFFFFFFF; // as chosen in compile.ml

SNAKEVAL print(SNAKEVAL val) {
  if ((val & BOOL_TAG) == 0) { // val is even ==> number
    printf("%ld", ((int64_t)(val)) / 2); // shift bits right to remove tag
  } else if (val == BOOL_TRUE) {
    printf("true");
  } else if (val == BOOL_FALSE) {
    printf("false");
  } else {
    printf("Unknown value: %#018lx", val); // print unknown val in hex
  }
  return val;
}

void our_error(uint64_t err, SNAKEVAL val) {
  if (err == 1) {
    printf ("Expected an integer, got ");
    print (val);
    printf("\n");
  }
  exit(err);
}

int main(int argc, char** argv) {
  uint64_t *heap = calloc(1024, sizeof(uint64_t));

  SNAKEVAL result = our_code_starts_here(heap);
  print(result);
  printf("\n");
  free(heap);
  return 0;
}
