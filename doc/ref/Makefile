TEXI_FILES := $(shell find . -type f -name \*.texi)
INFOFLAGS :=

all: calp.info

calp.info: $(TEXI_FILES)
	makeinfo $(INFOFLAGS) calp.texi
