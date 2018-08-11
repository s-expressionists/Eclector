# Eclector: A portable and extensible Common Lisp Reader

## Introduction ##

The `eclector` system provides a portable implementation of a reader
following the Common Lisp specification.

**``eclector`` is in an early phase of its development. Its ASDF
system structure, package structure, exported symbols and protocols
may all change at any time without prior notice.**

## Tutorial ##

### Basics ###

In the simplest case, the eclector reader can be used like any Common
Lisp reader:

``` lisp
(with-input-from-string (stream "(1 2 3)")
  (eclector.reader:read stream))
```

### Custom Parse Results ###

Using features provided in the `eclector.parse-result` package,
the reader can produce parse results controlled by the client,
optionally including source tracking and representation of skipped
input (due to e.g. comments and reader conditionals):

```lisp
(defclass my-client (eclector.parse-result:parse-result-client)
  ())

(defmethod eclector.parse-result:make-expression-result
    ((client my-client) (result t) (children t) (source t))
  (list :result result :source source :children children))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client my-client) (stream t) (reason t) (source t))
  (list :reason reason :source source))

(let ((eclector.reader:*client* (make-instance 'my-client)))
  (with-input-from-string (stream "(1 #|comment|# \"string\")")
    (eclector.parse-result:read stream)))
```

### Concrete Syntax Trees ###

The `eclector.concrete-syntax-tree` system provides a variant of the
`eclector` reader that produces instances of the concrete syntax tree
classes provided by the [concrete syntax tree library]:

``` lisp
(with-input-from-string (stream "(1 2 3)")
  (eclector.concrete-syntax-tree:cst-read stream))
```

[concrete syntax tree library]: https://github.com/robert-strandh/Concrete-Syntax-Tree
