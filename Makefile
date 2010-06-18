###-----------------------------------------------------------------------------
### APPLICATION LAYOUT
###-----------------------------------------------------------------------------
APPNAME = oserl
ERLS = $(patsubst src/%.erl,%.erl,$(wildcard src/*.erl))
MODS = $(subst $(space),$(comma)$(space),$(ERLS:.erl=))
DOCS = $(patsubst doc/%.ndoc,%.ndoc,$(wildcard doc/*.ndoc))
MANS = $(DOCS:.ndoc=.3)
HTMS = $(DOCS:.ndoc=.html)
TEXS = $(DOCS:.ndoc=.tex)
PDFS = $(DOCS:.ndoc=.pdf)

### Dependecy Search Paths
VPATH = src:include:ebin:doc

.PHONY: compile clean clobber doc
.SUFFIXES: .erl .hrl .beam .app.src .app .rel .ndoc

###-----------------------------------------------------------------------------
### TARGETS
###-----------------------------------------------------------------------------
compile:
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

clobber: clean
	@$(RM) -R doc/html
	@$(RM) -R doc/man
	@$(RM) -R doc/pdf

dialyze:
	./rebar analyze

doc: man html pdf

man: $(MANS)
	@$(MV) doc/man/$(APPNAME)_overview.3 doc/man/$(APPNAME).1

html: $(HTMS)
	@$(MV) doc/html/$(APPNAME)_overview.html doc/html/index.html

pdf: $(PDFS)

## Rules
%.3: %.ndoc
	@$(CD) doc; $(SED) "s|%MODULES%|`echo $(MODS)`|g" ../$^ | \
	$(SED) "s|%VSN%|$(VSN)|g" | $(SED) "s|%APPLICATION%|$(APPNAME)|g" | \
	$(ERLDOC) -i - -t man --no-toc --erl -o man/$@; $(CD) ..

%.html: %.ndoc
	@$(CD) doc; $(SED) "s|%MODULES%|`echo $(MODS)`|g" ../$^ | \
	$(SED) "s|%VSN%|$(VSN)|g" | $(SED) "s|%APPLICATION%|$(APPNAME)|g" | \
	$(ERLDOC) -i - -t xhtml --erl -o html/$@; $(CD) ..

%.pdf: %.ndoc
	@$(CD) doc; $(SED) "s|%MODULES%|`echo $(MODS)`|g" ../$^ | \
	$(SED) "s|%VSN%|$(VSN)|g" | $(SED) "s|%APPLICATION%|$(APPNAME)|g" | \
	$(ERLDOC) -i - -t pdf --no-toc --erl -o pdf/$@; $(CD) ..
