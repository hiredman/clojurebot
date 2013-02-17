# clojurebot-eval

clojurebot's sandbox as a rest service

## Usage

```
lein ring uberjar

java -Xmx100m -jar jetty\-runner\-7.4.2.v20110526.jar --port 3235
clojurebot-eval-0.1.0-SNAPSHOT-standalone.war
```

then add :evaluator "http://localhost:3235/eval" to your clojurebot config


## License

Copyright © 2013 Kevin Downey

Distributed under the Eclipse Public License, the same as Clojure.
