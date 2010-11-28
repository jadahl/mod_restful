EJABBERD_PATH=../ejabberd/
EJABBERD_SRC_PATH=$(EJABBERD_PATH)src

BEH_BEAMS=ebin/gen_restful_api.beam

BEAMS=ebin/mod_restful.beam \
      ebin/mod_restful_admin.beam \
      ebin/mod_restful_register.beam \
      ebin/mod_restful_mochinum.beam \
      ebin/mod_restful_mochijson2.beam

TEST_BEAMS=test_ebin/tests.beam \
	   test_ebin/mod_restful_tests.beam

all: $(BEH_BEAMS) $(BEAMS)

ebin/%.beam: src/%.erl
	@mkdir -p ebin
	erlc -pa ./ebin -I ./include -I $(EJABBERD_SRC_PATH) -pa $(EJABBERD_SRC_PATH) -o ./ebin $<

test_ebin/%.beam: tests/%.erl
	@mkdir -p test_ebin
	erlc -pa ./ebin -pa ./test_ebin -I ./include -I $(EJABBERD_SRC_PATH) -pa $(EJABBERD_SRC_PATH) -o ./test_ebin $<

install: all
	cp ebin/*.beam $(EJABBERD_SRC_PATH)

test: $(TEST_BEAMS)
	@erl \
	    -noshell \
	    -sname mod_restful_test \
	    -pa ./ebin -pa ./test_ebin -pa ./include \
	    -pa $(EJABBERD_SRC_PATH) \
	    -eval "tests:all_test()" \
		-s init stop


clean:
	rm -f ebin/*.beam test_ebin/*.beam
