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
1. Have timeout for "crawling thread" in case it never returns..
   Add logging/exception handling in server : sometimes seems to not work.
2. Have job start/finish time in JobResult.
3. For fair policy --
   * do not crawl same url for 10 mins, if job failed.
   * do not crawl same url for 1 hour, if job succeeded.
4. Compress index.js; -- may be donot need it after gzip in server..
   add checksum to index.js and put in index.html. -- need asset pipelining / webpack. -- low priority
   some cache control of 1 minute would be good.

*. Add api endpoint for reading all .response files : heroku dyno will lose all files; so no use; s3 is good option.
* try to write tests for IO things.
* break down IO things into more pure functions and test them.
* have really more tests : property based as well; with -K1K test suite size.
* Elm-export types for Sum types : https://github.com/krisajenkins/elm-export/
issues/6 OR use elm-bridge
