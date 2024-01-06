parser.out: lex.yy.o y.tab.o translator.o target_translator.o
	g++ lex.yy.o y.tab.o translator.o target_translator.o -lfl -o parser.out

target_translator.o: target_translator.cxx translator.h
	g++ -c target_translator.cxx

translator.o: translator.cxx translator.h
	g++ -c translator.cxx

lex.yy.o: lex.yy.c
	g++ -c lex.yy.c

y.tab.o: y.tab.c
	g++ -c y.tab.c

lex.yy.c: lexfile.l y.tab.h translator.h
	flex lexfile.l

y.tab.c y.tab.h: yaccfile.y
	bison -dty --report=all yaccfile.y

libass2.a: custom_print_read.o
	ar -rcs libass2.a custom_print_read.o

custom_print_read.o: custom_print_read.c myl.h
	gcc -c custom_print_read.c

clean:
	rm libass2.a custom_print_read.o parser.out translator.o target_translator.o lex.yy.* y.tab.* y.output

test: parser.out libass2.a
	@echo "Generating assembly files\n"
	cat ttest | ./parser.out test > quads.out
	@echo "Compiling assembly files\n"
	gcc test.s -L. -lass2 -no-pie -o test.o
	@echo "Binaries generated. Run using ./test<num>\n\n"
	@echo "Running test:\n"
	@./test
