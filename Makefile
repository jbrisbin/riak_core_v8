PACKAGE=riak_core_v8
DIST_DIR=dist
EBIN_DIR=ebin
INCLUDE_DIRS=include
DEPS_DIR=deps
DEPS ?= erlv8 basho_stats lager mochiweb poolboy protobuffs riak_core riak_sysmon webmachine
DEPS_EZ=$(foreach DEP, $(DEPS), $(DEPS_DIR)/$(DEP).ez)

all: compile

clean:
	rm -rf $(DIST_DIR)
	rm -rf $(EBIN_DIR)

distclean: clean
	rm -rf $(DEPS_DIR)

package: compile $(DEPS_EZ)
	rm -f $(DIST_DIR)/$(PACKAGE).ez
	mkdir -p $(DIST_DIR)/$(PACKAGE)
	cp -r $(EBIN_DIR) $(DIST_DIR)/$(PACKAGE)
	$(foreach EXTRA_DIR, $(INCLUDE_DIRS), cp -r $(EXTRA_DIR) $(DIST_DIR)/$(PACKAGE);)
	(cd $(DIST_DIR); zip -r $(PACKAGE).ez $(PACKAGE))

$(DEPS_DIR):
	./rebar get-deps

$(DEPS_EZ): 
	cd $(DEPS_DIR); $(foreach DEP, $(DEPS), zip -r $(DEP).ez $(DEP);)

compile: $(DEPS_DIR)
	./rebar compile