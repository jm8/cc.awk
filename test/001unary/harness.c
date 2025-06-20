#include "../util.h"

int func();

int main() { ASSERT_EQ(func(), -~(!0)); }
