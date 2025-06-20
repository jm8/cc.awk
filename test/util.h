#include <stdio.h>
#include <stdlib.h>

#define ASSERT_EQ(a, b)                                                        \
  do {                                                                         \
    __auto_type aa = (a);                                                      \
    __auto_type bb = (b);                                                      \
    if (aa != bb) {                                                            \
      printf("%s = %d <> %s = %d\n", #a, aa, #b, bb);                          \
      exit(1);                                                                 \
    }                                                                          \
  } while (0)
;
