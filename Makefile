CC=clang++
CPPFLAGS:=$(shell llvm-config --cxxflags)
LDFLAGS:=$(shell llvm-config --ldflags --system-libs --libs core native mcjit)

csharptollvm: lex.yy.o parser.tab.o ast.o
	$(CC) -o $@ $^ $(LDFLAGS) -rdynamic
parser.tab.o: parser.tab.cpp parser.tab.hpp ast.hpp
	$(CC) $(CPPFLAGS) -c -o $@ $<
parser.tab.cpp parser.tab.hpp: parser.ypp
	bison -d -v $<
lex.yy.o: lex.yy.c parser.tab.hpp ast.hpp
	$(CC) $(CPPFLAGS) -Wno-deprecated-register -Wno-sign-compare -c -o $@ $<
lex.yy.c: lexer.lex
	flex $<
ast.o: ast.cpp ast.hpp
	$(CC) $(CPPFLAGS) -c -o $@ $<

.PHONY: clean

clean:
	rm -rf *~ *tab* lex.yy.* *.o csharptollvm *.output a.out
