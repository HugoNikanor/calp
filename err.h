#ifndef ERR_H
#define ERR_H

#include <stdio.h>
#define RED "\x1B[0;31m"
#define RESET "\x1b[m"
#define ERR(msg) fprintf(stderr, RED "ERR" RESET " (%s:%i) %s\n", __FILE__, __LINE__, #msg)
#define ERR_F(fmt, ...) fprintf(stderr, RED "ERR" RESET " (%s:%i) " fmt "\n", \
		__FILE__, __LINE__, ##__VA_ARGS__)

#endif /* ERR_H */
