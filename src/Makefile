CFLAGS		= -O3 -Wall -Wstrict-prototypes
#CFLAGS		= -O3 -Wall -Wextra -Wstrict-prototypes
LIBS		= -lm
CC		= gcc
RM		= rm

#SRC		=

PROGS		= acme
BINDIR		= /usr/local/bin
USERBIN		= $(HOME)/bin
OBJS		= acme.o alu.o cliargs.o cpu.o dynabuf.o encoding.o flow.o global.o input.o macro.o mnemo.o output.o platform.o pseudoopcodes.o section.o symbol.o tree.o typesystem.o

all: $(PROGS)

acme: $(OBJS)
	$(CC) $(CFLAGS) -o acme $(OBJS) $(LIBS)
	strip acme


acme.o: config.h platform.h alu.h cpu.h dynabuf.h encoding.h flow.h global.h input.h macro.h mnemo.h output.h pseudoopcodes.h section.h symbol.h version.h acme.h acme.c

alu.o: config.h platform.h cpu.h dynabuf.h encoding.h global.h input.h section.h symbol.h tree.h alu.h alu.c

cliargs.o: cliargs.h cliargs.c

cpu.o: config.h dynabuf.h global.h mnemo.h tree.h cpu.h cpu.c

dynabuf.o: config.h global.h input.h dynabuf.h dynabuf.c

encoding.o: encoding.h encoding.c

flow.o: config.h alu.h dynabuf.h global.h input.h mnemo.h symbol.h tree.h flow.h flow.c

global.o: config.h platform.h acme.h cpu.h dynabuf.h encoding.h input.h macro.h pseudoopcodes.h section.h symbol.h global.h global.c

input.o: config.h alu.h dynabuf.h global.h section.h symbol.h tree.h input.h input.c

macro.o: config.h alu.h dynabuf.h global.h input.h section.h symbol.h tree.h macro.h macro.c

mnemo.o: config.h alu.h cpu.h dynabuf.h global.h input.h output.h tree.h mnemo.h mnemo.c

output.o: config.h cpu.h global.h output.h output.c

platform.o: config.h platform.h platform.c

pseudoopcodes.o: alu.h flow.h global.h input.h macro.h output.h symbol.h pseudoopcodes.h pseudoopcodes.c

section.o: config.h dynabuf.h global.h symbol.h tree.h section.h section.c

symbol.o: config.h alu.h cpu.h dynabuf.h global.h input.h section.h tree.h symbol.h symbol.c

tree.o: config.h dynabuf.h global.h symbol.h tree.h tree.c

typesystem.o: config.h global.h typesystem.h typesystem.c

clean:
	-$(RM) -f *.o $(PROGS) *~ core


install: all
	install -d $(BINDIR)
	install $(PROGS) $(BINDIR)

userinstall: all
	install -d $(USERBIN)
	install $(PROGS) $(USERBIN)

# DO NOT DELETE
