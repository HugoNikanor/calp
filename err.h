#ifndef ERR_H
#define ERR_H

#include <stdio.h>

#define _RESET "\x1b[m"
#define _BLACK  "\x1B[0;30m"
#define _RED    "\x1B[0;31m"
#define _GREEN  "\x1B[0;32m"
#define _YELLOW "\x1B[0;33m"
#define _BLUE   "\x1B[0;34m"
#define _PURPLE "\x1B[0;35m"
#define _CYAN   "\x1B[0;36m"
#define _WHITE  "\x1B[0;37m"

#define ERR(msg) fprintf(stderr, _RED "ERR" _RESET " (%s:%i) %s\n", __FILE__, __LINE__, #msg)
#define ERR_F(fmt, ...) fprintf(stderr, _RED "ERR" _RESET " (%s:%i) " fmt "\n", \
		__FILE__, __LINE__, ##__VA_ARGS__)

/* Parse error */
#define ERR_P(ctx, fmt, ...) fprintf(stderr, _RED "PARSE" _RESET " (%s:%i) %i:%i " fmt "\n", \
		__FILE__, __LINE__, (ctx)->pline, (ctx)->pcolumn, ##__VA_ARGS__)

#define INFO(msg) fprintf(stderr, _BLUE "INFO" _RESET " (%s:%i) %s\n", __FILE__, __LINE__, #msg)
#define INFO_F(fmt, ...) fprintf(stderr, _BLUE "INFO" _RESET " (%s:%i) " fmt "\n", \
		__FILE__, __LINE__, ##__VA_ARGS__)

#if 0
#define LINE(len) do { \
	printf(_GREEN); \
	FOR(int, i, len) printf("_"); \
	printf("\n"); \
} while (0)

#define PRINT(T, v) do { \
	char buf[0x1000]; \
	FMT(T)(v, buf); \
	INFO_F("%s", buf); \
} while (0)
#endif

#endif /* ERR_H */
