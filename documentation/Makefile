NAME=eclector

SOURCES=$(NAME).texi $(wildcard chap-*.texi)

GENERATED=version.texi

FIGURES=$(wildcard *.png)

.PHONY: all clean

all : $(NAME).info $(NAME).pdf $(NAME).html

#%.png: %.plantuml
#	java -jar .../plantuml.jar -tpng $<

version.texi: ../version.sexp
	sbcl --noinform --disable-debugger --no-sysinit --no-userinit                           \
	     --eval '(require :asdf)'                                                           \
	     --eval '(format t "@macro sysversion~%~A~%@end macro" (uiop:read-file-form "$<"))' \
	     --quit                                                                             \
	  > $@

$(NAME).info: $(SOURCES) $(GENERATED) $(FIGURES)
	makeinfo --output $@ $(NAME).texi

$(NAME).pdf: $(SOURCES) $(GENERATED) $(FIGURES)
	texi2pdf $(NAME).texi

$(NAME).html: $(SOURCES) $(GENERATED) $(FIGURES) $(wildcard *.css)
	makeinfo --html --no-split \
	         --css-include=style-common.css --css-include=style-single.css \
	         $(NAME).texi

clean:
	rm -f *.cp *.cps *.fn *.fns \
	      *.aux *.toc *.log \
	      $(NAME).info $(NAME).pdf $(NAME).html \
	      $(GENERATED)
