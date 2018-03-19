# Make a Clojure project also run in ClojureScript

Full story [here](https://facundoolano.wordpress.com/2016/10/16/from-clojure-to-clojurescript/).

## Getting started

1. [Differences from Clojure](https://www.clojurescript.org/about/differences)
  1. `:require-macros`
2. [ClojureScript Quick Start](http://clojurescript.org/guides/quick-start)
  1. Using ClojureScript on a Web Page
  2. Running ClojureScript on Node.js
3. [Reader Conditionals](http://clojure.org/guides/reader_conditionals)
  1. `.clj` files for Clojure-only logic (and macros!).
  2. `.cljs` files for ClojureScript-only logic.
  3. `.cljc` files for shared logic (and macros!).
  4. `#?` reader conditionals syntax.

```clojure
(defn str->int [s]
  #?(:clj  (java.lang.Integer/parseInt s)
     :cljs (js/parseInt s)))
```
```clojure
(ns example.dialogs
  #?(:clj (:require [advenjure.dialogs :refer [dialog]])
     :cljs (:require-macros [advenjure.dialogs :refer [dialog]])))
```

## Basic JavaScript interop

1. Dot notation same as in Clojure.
2. `js-obj` to create plain key/value objects.
3. `clj->js` convert clj structures to js structures.
4. `aget`/`aset` to access object properties.

```clojure
(def plain-object (js-obj "a" 1 "b" true "c" nil))

(def nested-object (clj->js :a 1 :b [1 2 3] :c #{"d" true :e nil}))

(aset js/localStorage "key" "value"))
(println (aget js/localStorage "key")))
```

Translates (more or less) to:

```js
namespace.plain_object = {a: 1, b: true, c: null};

namespace.nested_object = {a: 1, b: [1, 2, 3], c: ["d", true, "e", null]};

window.localStorage.key = "value";
console.log(window.localStorage.key);
```

## Trickier interop: core.async
```clojure
(ns advenjure.game)

(loop [state game-state]
  (let [input (get-input state)
        new-state (process-input state input)]
    (if-not (finished? new-state)
      (recur new-state)
      (exit))))
```

```clojure
(ns advenjure.ui.input)

(def input-chan (chan))

(defn process-command
  "Callback passed to jQuery terminal upon initialization"
  [command]
  (go (>! input-chan command)))

(defn get-input
  "Wait for input to be written in the input channel"
  [state]
  (go (<! input-chan)))
```

```clojure
(ns advenjure.game)

(go-loop [state game-state]
  (let [input (<! (get-input state))
        new-state (process-input state input)]
    (if-not (finished? new-state)
      (recur new-state)
      (exit))))
```

```clojure
(ns advenjure.async)

(defmacro <!?
  "If value is a channel (implements ReadPort protocol), take the value from it
  (<!), otherwise return as is. Works with nested channels, I wish there wasn't any.
  "
  [value]
  `(if-cljs
    (loop [result# ~value]
      (if (satisfies? cljs.core.async.impl.protocols/ReadPort result#)
        (recur (cljs.core.async/<! result#))
        result#))
    ~value))
```

## lein-cljsbuild

```clojure
:plugins [[lein-cljsbuild "1.1.4"]]
:cljsbuild
  {:builds
   {:main {:source-paths ["src"]
           :compiler {:output-to "main.js"
                      :main example.core
                      :optimizations :simple
                      :pretty-print false
                      :optimize-constants true
                      :static-fns true}}

    :dev {:source-paths ["src"]
          :compiler {:output-to "dev.js"
                     :main example.core
                     :optimizations :none
                     :source-map true
                     :pretty-print true}}}}
```

Compiles with `lein cljsbuild once main`

## lein-figwheel (thanks to @nicoberger!)

```clojure
:dev {:source-paths ["src"]
      :figwheel {:before-jsload "advenjure.ui.input/figwheel-cleanup"}

      :compiler {:output-to "resources/public/js/main.js"
                 :output-dir "resources/public/js/out"
                 :main example.core
                 :parallel-build true
                 :asset-path "js/out"
                 :optimizations :none
                 :source-map true
                 :pretty-print true}}}}
```

Runs with `lein figwheel`

## Bundling foreign libs

How to use third-party js without having the user manually include it in the HTML?

1. [ClojureScript dependencies](http://clojurescript.org/reference/dependencies)
  1. Need externs so Google Closure doesn't rename external symbols (e.g. "JQuery").
2. [CLJSJS Project](http://cljsjs.github.io/)
3. `:foreign-libs` and `src/deps.cljs`

```clojure
{:foreign-libs
 [{:file "jquery/jquery-3.1.1.js"
 :file-min "jquery/jquery-3.1.1.min.js"
 :provides ["jquery"]}
 {:file "jquery.terminal/jquery.terminal-0.11.10.js"
 :file-min "jquery.terminal/jquery.terminal-0.11.10.min.js"
 :requires ["jquery"]
 :provides ["jquery.terminal"]}
 {:file "jquery.terminal/jquery.mousewheel.js"
 :file-min "jquery.terminal/jquery.mousewheel.min.js"
 :requires ["jquery"]
 :provides ["jquery.mousewheel"]}
 {:file "xregexp/xregexp-all.js"
 :file-min "xregexp/xregexp-all.min.js"
 :provides ["xregexp"]}]
 :externs ["jquery/externs.js" "jquery.terminal/externs.js" "xregexp/externs.js"]}
```
