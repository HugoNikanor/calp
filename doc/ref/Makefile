TEXI_FILES := $(wildcard *.texi)
INFOFLAGS :=

all: calp.info

calp.info: $(TEXI_FILES)
	makeinfo $(INFOFLAGS) calp.texi
