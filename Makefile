#######################################################################
# Generic make script for compiling erlang code                       #
# The environment variable $ERLHOME has to be set to where erlang/OTP #
# is installed                                                        #
# Compiles the code into a ebin dir. relative to the source dir.      #
####################################################################### 
#Compiles the code into a ebin dir. relative to the source dir. 
include vsn.mk

ERLC 	:= erlc
GEN 	:= beam

EFLAGS := +debug_info

INCLUDE := include
EBIN 	:= ebin
SRC     := $(wildcard src/*.erl)
HEADERS := $(wildcard $(INCLUDE)/*.hrl)	
CODE  	:= $(patsubst src/%.erl, ebin/%.beam, $(SRC))
DOTSRC  := $(wildcard src/*.app.src)
DOTAPP  := $(patsubst src/%.app.src, ebin/%.app, $(DOTSRC))


.PHONY: clean all

$(EBIN)/%.beam: src/%.erl
	$(ERLC) -I$(INCLUDE)  -W -b beam -o $(EBIN) $(EFLAGS) $(WAIT) $<

all: $(CODE) $(DOTAPP)

$(DOTAPP): $(DOTSRC)
	@sed 's/%VSN%/$(VSN)/g' <$(DOTSRC) >$(DOTAPP)



clean:
	rm -f $(EBIN)/* 
