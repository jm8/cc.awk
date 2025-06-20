#include "../util.h"

int func();

int main() { ASSERT_EQ(func(), (128 / 2 / 4) + (6 - 3 % 3) * 7); }
