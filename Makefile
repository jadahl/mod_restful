EJABBERD_PATH=../ejabberd/
EJABBERD_SRC_PATH=$(EJABBERD_PATH)src

BEH_BEAMS=ebin/gen_restful_api.beam

BEAMS=ebin/mod_restful.beam \
      ebin/mod_restful_admin.beam \
      ebin/mod_restful_register.beam \
      ebin/mod_restful_mochinum.beam \
      ebin/mod_restful_mochijson2.beam

all: $(BEH_BEAMS) $(BEAMS)

ebin/%.beam: src/%.erl
	@mkdir -p ebin
	erlc -pa ./ebin -I ./include -I $(EJABBERD_SRC_PATH) -pa $(EJABBERD_SRC_PATH) -o ./ebin $<

install: all
	cp ebin/*.beam $(EJABBERD_SRC_PATH)

clean:
	rm -f ebin/*.beam
