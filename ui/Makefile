#../assets/index.html: Api.elm
../assets/index.html: clean-html
	elm make src/Main.elm --output=index.html
	mv *.html ../assets/

Api.elm: GenerateElm.hs ../src/Sanskell/Api.hs
	(cd ../ ; stack exec -- runhaskell -isrc ui/GenerateElm.hs)

clean-html:
	rm -f index.html
# run-tests: tests.js
# 	echo "running tests"
# 	node tests.js

# tests.js: Main.elm Api.elm Tests.elm
# 	echo "making tests"
# 	elm make Tests.elm --output tests.js
