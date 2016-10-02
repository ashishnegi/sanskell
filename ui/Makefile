../assets/index.html: Api.elm
	echo "in ../assets/index.html : doing elm make Main.elm"
	elm make src/Main.elm
	mv index.html ../assets/

Api.elm: GenerateElm.hs ../src/Sanskell/Api.hs
	echo "in Api.elm : stack exec -- runhaskell ..."
	(cd ../ ; stack exec -- runhaskell -isrc ui/GenerateElm.hs)

# run-tests: tests.js
# 	echo "running tests"
# 	node tests.js

# tests.js: Main.elm Api.elm Tests.elm
# 	echo "making tests"
# 	elm make Tests.elm --output tests.js
