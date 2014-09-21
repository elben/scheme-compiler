#include <stdio.h>

/* define all scheme constants */

#define bool_f    0x2F
#define bool_t    0x6F

// Scheme fixnums.
//
// A word is a fixnum if the last two bits are 0s. The mask, tag and shift are
// used together to check if a word is a fixnum, and to shift appropriately to
// get the true value.
//
#define fx_mask   0x03 // fix num mask TODO explain
#define fx_tag    0x00 // fix num mask TODO explain
#define fx_shift  2    // fix num tag TODO explain

/* all scheme values are of type ptrs */
typedef unsigned int ptr;

int scheme_entry();

static void print_ptr(ptr x) {
  if ((x & fx_mask) == fx_tag) {
    // A fixnum.
    printf("%d", ((int)x) >> fx_shift);
  } else if (x == bool_f) {
    printf("#f");
  } else {
    printf("#<unknown 0x%08x>", x);
  }
  printf("\n");
}

int main(int argc, char** argv) {
  /* print_ptr(scheme_entry()); */
  printf("%d\n", scheme_entry());
  return 0;
}
