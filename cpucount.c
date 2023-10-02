/**
   Basic script for checking number of available CPU cores.

   Used by tests to spawn an appropriate number of threads.
 */

#include <unistd.h>
#include <stdio.h>

int main() {
	printf("%i\n", sysconf(_SC_NPROCESSORS_ONLN));
}
