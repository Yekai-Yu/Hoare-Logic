#MPVERSION=int_if_then_else
#STUDENTSRC=$(MPVERSION).ml  # Shouldn't we start with the json file?

OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
GMAKE=make
RM=rm
CP=cp
LN=ln
MV=mv
TAR=tar
GZIP=gzip
MKDIR=mkdir


GRADER_NAME=grader

#MODULE_STUDENT=student
#MODULE_SOLUTION=solution
MODULE_COMMON=hlcommon
OBJLANG=hl


OBJECTS=$(MODULE_COMMON).cmo $(OBJLANG)parse.cmo $(OBJLANG)lex.cmo hlcheck.cmo $(OBJLANG)processing.cmo

all: $(GRADER_NAME)

STUDENT_CLEAN=$(OBJECTS) *.cm? $(OBJLANG)parse.ml $(OBJLANG)parse.mli $(OBJLANG)lex.ml conflicts.txt $(GRADER_NAME)

$(GRADER_NAME): $(OBJECTS)
	$(OCAMLC) -o $(GRADER_NAME) $(OBJECTS) 

$(OBJLANG)parse.cmo: $(MODULE_COMMON).cmo $(OBJLANG)parse.mly
	$(OCAMLYACC) $(OBJLANG)parse.mly
	$(OCAMLC) -c $(OBJLANG)parse.mli
	$(OCAMLC) -c $(OBJLANG)parse.ml

$(OBJLANG)lex.cmo: $(OBJLANG)parse.cmo $(OBJLANG)lex.mll 
	$(OCAMLLEX) $(OBJLANG)lex.mll
	$(OCAMLC) -c $(OBJLANG)lex.ml

$(OBJLANG)processing.cmo: $(MODULE_COMMON).cmo $(OBJLANG)parse.cmo $(OBJLANG)lex.cmo $(OBJLANG)processing.ml
	$(OCAMLC) -c $(OBJLANG)processing.ml
	
hlcheck.cmo: hlcheck.ml $(MODULE_COMMON).cmo
	$(OCAMLC) -c hlcheck.ml

$(MODULE_COMMON).cmo: $(MODULE_COMMON).ml
	$(OCAMLC) -c $(MODULE_COMMON).ml

clean:
	rm -fr $(STUDENT_CLEAN)

dist-clean: clean

