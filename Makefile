PROJECT = mekao
EUNIT_OPTS = verbose
DIALYZER_OPTS = -Werror_handling -Wrace_conditions \
	-Wunmatched_returns -Wunderspecs

include erlang.mk
