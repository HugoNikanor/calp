#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

typedef enum {
	key, value
} context;

typedef struct  {
	char *key, *value;
} kvpair;

/*
 * Max length of a line.
 * TODO update this to allow longer lines, in case someone doesn't
 * follow the standard.
 */
#define SEGSIZE 75

int ii = 0;

#define MARK do { printf("%i -- %i\n", __LINE__, ii++); } while (0)

#define ERR(x) do { \
	fprintf(stderr, "ERR %i: %s", __LINE__, (x)); \
} while(0)


int main (int argc, char* argv[argc]) {
	FILE* F = fopen(argv[1], "r");

	int segments = 1;
	char* str = malloc(segments * SEGSIZE);
	int i = 0;

	context ctx = key;
	kvpair kvs[100];
	int ki = 0;

	char c;
	while ( (c = fgetc(F)) != EOF) {
		/*
		 * A carrige return means that the current line is at an
		 * end. The following character should always be \n.
		 * However, if the first character on the next line is a
		 * whitespace then the two lines should be concatenated.
		 */
		if (c == '\r') {

			char s[2];
			s[0] = fgetc(F);
			s[1] = fgetc(F);

			if (s[0] != '\n') { ERR("expected newline after CR"); }
			else if (s[1] == ' ' || s[1] == '\t') {
				MARK;
				// TODO check return value
				// TODO segments is always incremented here, meaning
				// that segment grows larger for every multi line
				// encountered.
				str = realloc(str, ++segments * SEGSIZE);
				if (str == NULL) { /* TODO signal error */
					exit (1);
				}
				continue;
			} else {
				MARK;
				if (ungetc(s[1], F) != s[1]) { /* TODO signal error */
					exit (2);
				}

				/* At TRUE end of line */
				kvs[ki].value = malloc(i + 1);
				memcpy(kvs[ki].value, str, i);
				kvs[ki].value[i] = 0;
				ki++;
				i = 0;
				ctx = key;
				continue;
			}
		} else if (ctx == key && c == ':') {
			kvs[ki].key = malloc(i + 1);
			memcpy(kvs[ki].key, str, i);
			kvs[ki].key[i] = 0;
			printf("key := %s\n", kvs[ki].key);
			i = 0;
			ctx = value;
			continue;
		}

		str[i] = c;
		++i;
	}
	if (errno != 0) {
		printf("Error parsing, errno = %i\n", errno);
	}
	puts("File parsed");
	free(str);

	/*
	 * Just print and free all collected data
	 */
	for (int i = 0; i < ki; i++) {
		printf("[%s] := [%s]\n", kvs[i].key, kvs[i].value);
		free(kvs[i].key);
		free(kvs[i].value);
	}

}
