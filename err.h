#ifndef ERR_H
#define ERR_H

#include <stdio.h>
#define ERR(s) fprintf(stderr, "ERR (%s:%i): %s\n", __FILE__, __LINE__, s)

#endif /* ERR_H */
