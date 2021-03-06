MODULES := $(wildcard modules/*)

$(MODULES):
	$(MAKE) -C $@ $(MAKECMDGOALS)

lint-root:
	clj-kondo --lint src test deps.edn

lint: $(MODULES) lint-root

test-root:
	clojure -M:test --profile :ci

test: $(MODULES) test-root

test-coverage: $(MODULES)

clean-root:
	rm -rf .clj-kondo/.cache .cpcache target

clean: $(MODULES) clean-root

uberjar:
	clojure -X:depstar uberjar :jar target/blaze-standalone.jar

outdated:
	clojure -M:outdated

deps-tree:
	clojure -Stree

.PHONY: $(MODULES) lint-root lint test-root test test-coverage clean-root clean uberjar outdated deps-tree
