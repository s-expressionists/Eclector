## Reading an object in a different package

Based on extension in SBCL, the syntax `PACKAGE::OBJECT` can be used
to read an object in a package other than the current one.

```lisp
(let ((eclector.reader:*client* (make-instance 'eclector.syntax-extensions.object-in-package:object-in-package-syntax-mixin)))
  (read-from-string "cl-user::(foo bar 1)"))
```

```lisp
(let ((eclector.reader:*client* (make-instance 'eclector.syntax-extensions.object-in-package:object-in-package-syntax-mixin)))
  (read-from-string "cl-user::#(foo)"))
```

```lisp
(let ((eclector.reader:*client* (make-instance 'eclector.syntax-extensions.object-in-package:object-in-package-syntax-mixin)))
  (read-from-string "cl-user::;foo
bar"))
```
