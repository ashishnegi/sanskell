## Sanskell


# Todo
*. write tests for wordCount : remove whitespaces and empty strings.
*. poll status every 1 second.
*. remove hardcoding of port : reader monad
*. Elm-export types for Sum types : https://github.com/krisajenkins/elm-export/issues/6 OR use elm-bridge
1. Elm UI
2. Have timeout for "crawling thread" in case it never returns..
3. Have job start/finish time in JobResult.
4. For fair policy --
   a) do not crawl same url for 10 mins, if job failed.
   b) do not crawl same url for 1 hour, if job succeeded.
