PROJECT = mekao
EUNIT_DIR = test
DIALYZER_OPTS = -Werror_handling -Wrace_conditions \
	-Wunmatched_returns -Wunderspecs

include erlang.mk
