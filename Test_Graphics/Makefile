OBJS = testGraphics.cmo

CONF=-I +threads

LIBS=$(WITHGRAPHICS) $(WITHUNIX) $(WITHTHREADS)

# Default setting of the WITH* variables. Should be changed if your
# local libraries are not found by the compiler.
WITHGRAPHICS =graphics.cma -cclib -lgraphics -cclib -L/usr/X11R6/lib -cclib -lX11

WITHTHREADS =threads.cma -cclib -lthreads

WITHUNIX =unix.cma -cclib -lunix

testGraphics : $(OBJS)
	ocamlc $(CONF) -o testGraphics $(LIBS) $(OBJS)

%.cmo : %.ml
	ocamlc $(CONF) -c $<

.PHONY : clean
clean :
	rm -f testGraphics \
	*.cmo *.cmi *.out *.diff