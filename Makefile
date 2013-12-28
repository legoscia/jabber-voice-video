VERSION:=$(shell sed -ne 's/^ *"\([0-9.]\{1,\}\)"$$/\1/p' jabber-voice-video-pkg.el)
ifeq "" "$(VERSION)"
$(error Cannot determine version)
endif
NAME=jabber-voice-video-$(VERSION)
PACKAGE=$(NAME).tar
HTDOCS=index.html.template adapter.js main.js

$(PACKAGE): $(wildcard *.el) $(addprefix htdocs/,$(HTDOCS)) $(NAME)
	tar -cf $@ $(NAME)/*.el $(addprefix $(NAME)/htdocs/,$(HTDOCS))
	@echo Created $@. Now install it with M-x package-install-file.

$(NAME):
	ln -sf . $(NAME)

.INTERMEDIATE: $(NAME)

clean:
	rm -f $(PACKAGE)

.PHONY: clean
