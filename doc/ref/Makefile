.PHONY: all install

TEXI_FILES := $(shell find . -type f -name \*.texi)
INFOFLAGS := --no-split

all: calp.info

calp.info: $(TEXI_FILES)
	$(MAKEINFO) -o $@ $(INFOFLAGS) calp.texi

install: all
	install -m644 -D -t $(DESTDIR)/usr/share/info/ calp.info
