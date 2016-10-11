# Sanskell
A crawler for sanskrit website that generates `word-cloud` for the pages that are crawled.

## setup
You will need to install `stack` and `elm` if you already do not have them.

```
brew install haskell-stack
brew install elm
```

Then just run `make` in `sanskell` directory.

start server with :

`stack exec sanskell-exe`

## Todo
1. Have better position of words : most-weighted at the center
   compress index.js;
   add checksum to index.js and put in index.html.
   avgWeight gives NaN :P
2. Have timeout for "crawling thread" in case it never returns..
   Add logging/exception handling in server : sometimes seems to not work.
3. Have job start/finish time in JobResult.
4. For fair policy --
   * do not crawl same url for 10 mins, if job failed.
   * do not crawl same url for 1 hour, if job succeeded.

* try to write tests for IO things.
* break down IO things into more pure functions and test them.
* have really more tests : property based as well; with -K1K test suite size.
* Elm-export types for Sum types : https://github.com/krisajenkins/elm-export/
issues/6 OR use elm-bridge
