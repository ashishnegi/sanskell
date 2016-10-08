## Sanskell


# Todo

* Have better position of words : most-weighted at the center
* Add logging/exception handling in server : sometimes seems to not work.
* remove hardcoding of port/config : reader monad
* Elm-export types for Sum types : https://github.com/krisajenkins/elm-export/
issues/6 OR use elm-bridge

1. Elm UI
2. Have timeout for "crawling thread" in case it never returns..
3. Have job start/finish time in JobResult.
4. For fair policy --
   * do not crawl same url for 10 mins, if job failed.
   * do not crawl same url for 1 hour, if job succeeded.
