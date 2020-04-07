(cl:in-package #:eclector.reader)

;;;; Recovery strategy descriptions

(macrolet ((define-description (strategy description)
             `(defmethod recovery-description ((strategy (eql ',strategy))
                                               (language acclimation:english))
                ,description)))
  (define-description use-partial-symbol          "Return a symbol named by the already read characters.")
  (define-description replace-invalid-character   "Replace the invalid character with a valid one.")
  (define-description treat-as-escaped            "Treat the character as if it had been escaped.")

  (define-description replace-invalid-digit       "Use a suitable digit in place of the invalid digit.")

  (define-description ignore-quasiquote           "Read the following form as if it were not quasiquoted.")
  (define-description ignore-unquote              "Read the following form as if it were not unquoted.")
  (define-description ignore-missing-delimiter    "Ignore the missing delimiter.")
  (define-description use-partial-string          "Return a string of the already read characters.")
  (define-description inject-nil                  "Use NIL in place of the missing object.")
  (define-description ignore-object               "Ignore the object.")
  (define-description use-partial-list            "Return a list of the already read elements.")
  (define-description ignore-trailing-right-paren "Ignore the trailing right parenthesis.")

  (define-description use-empty-vector            "Return an empty vector.")
  (define-description use-partial-vector          "Return a vector of the already read elements.")
  (define-description ignore-excess-elements      "Use the already read elements and ignore the excess elements."))

;;;; Contexts and condition reporters

(macrolet
    ((define-reporter (((condition-var condition-specializer) stream-var)
                       &body body)
       `(defmethod acclimation:report-condition
            ((,condition-var ,condition-specializer)
             ,stream-var
             (language acclimation:english))
          ,@body))
     (define-context (context name)
       `(defmethod context-name ((context  (eql ',context))
                                 (language acclimation:english))
          ,name)))

;;; Type error

  (define-reporter ((condition read-object-type-error) stream)
    (format stream "The read object ~s is not of the required type ~s."
            (type-error-datum condition)
            (type-error-expected-type condition)))

;;; Conditions related to symbols

  (define-reporter ((condition package-does-not-exist) stream)
    (format stream "Package named ~s does not exist."
            (desired-package-name condition)))

;;; Conditions related to symbols

  (flet ((package-name* (package)
           ;; PACKAGE may be a `cl:package' but could also be a
           ;; client-defined representation of a package.
           (typecase package
             (package (package-name package))
             (t package))))

    (define-reporter ((condition symbol-does-not-exist) stream)
      (format stream "Symbol named ~s not found in the ~a package."
              (desired-symbol-name condition)
              (package-name* (desired-symbol-package condition))))

    (define-reporter ((condition symbol-is-not-external) stream)
      (format stream "Symbol named ~s is not external in the ~a package."
              (desired-symbol-name condition)
              (package-name* (desired-symbol-package condition)))))

  (define-reporter ((condition invalid-constituent-character) stream)
    (format stream "The ~/eclector.base::describe-character-english/ must ~
                    not occur in a symbol as it is an invalid ~
                    constituent."
            (aref (token condition) 0)))

  (define-reporter ((condition unterminated-single-escape-in-symbol) stream)
    (format stream "While reading symbol, expected character after ~
                    the ~/eclector.base::describe-character-english/ ~
                    when input ended."
            (escape-char condition)))

  (define-reporter ((condition unterminated-multiple-escape-in-symbol) stream)
    (format stream "While reading symbol, expected the ~
                    ~/eclector.base::describe-character-english/ when ~
                    input ended."
            (delimiter condition)))

  (define-reporter ((condition symbol-name-must-not-be-only-package-markers) stream)
    (format stream "Symbol name without any escapes must not consist ~
                    solely of package markers (: characters)."))

  (define-reporter ((condition symbol-name-must-not-end-with-package-marker) stream)
    (format stream "Symbol name must not end with a package ~
                    marker (the : character)."))

  (define-reporter ((condition two-package-markers-must-be-adjacent) stream)
    (format stream "If a symbol token contains two package markers, ~
                    they must be adjacent as in package::symbol."))

  (define-reporter ((condition two-package-markers-must-not-be-first) stream)
    (format stream "A symbol token must not start with two package ~
                    markers as in ::name."))

  (define-reporter ((condition symbol-can-have-at-most-two-package-markers) stream)
    (format stream "A symbol token must not contain more than two ~
                    package markers as in package:::symbol or ~
                    package::first:rest."))

  (define-reporter ((condition uninterned-symbol-must-not-contain-package-marker) stream)
    (format stream "A symbol token following #: must not contain a ~
                    package marker."))

;;; General reader macro conditions

  (define-reporter ((condition sharpsign-invalid) stream)
    (format stream "The ~/eclector.base::describe-character-english/ is not ~
                    a valid sub-character for the # dispatch macro."
            (character-found condition)))

  (define-reporter ((condition numeric-parameter-supplied-but-ignored) stream)
    (format stream "Dispatch reader macro ~a was supplied with a ~
                    numeric parameter it does not accept."
            (macro-name condition)))

  (define-reporter ((condition numeric-parameter-not-supplied-but-required) stream)
    (format stream "Dispatch reader macro ~a requires a numeric ~
                    parameter, but none was supplied."
            (macro-name condition)))

;;; Conditions related to quotation

  (define-context sharpsign-single-quote "the function reader macro")

  (define-reporter ((condition end-of-input-after-quote) stream)
    (format stream "While reading quote, expected quoted material when ~
                    input ended."))

  (define-reporter ((condition object-must-follow-quote) stream)
    (format stream "An object must follow quote."))

;;; Conditions related to strings

  (define-reporter ((condition unterminated-string) stream)
    ;; Use the DELIMITER slot instead of a fixed character since the
    ;; reader macro may have been installed on non-default character.
    (format stream "While reading string, expected the ~
                    ~/eclector.base::describe-character-english/ when input ~
                    ended."
            (delimiter condition)))

  (define-reporter ((condition unterminated-single-escape-in-string) stream)
    (format stream "While reading string, expected character after ~
                    the escape ~
                    ~/eclector.base::describe-character-english/ ~
                    when input ended."
            (escape-char condition)))

;;; Conditions related to quasiquotation

  (define-reporter ((condition backquote-in-invalid-context) stream)
    (format stream "Backquote is illegal in ~A."
            (context-name (context condition) language)))

  (define-reporter ((condition object-must-follow-backquote) stream)
    (format stream "An object must follow backquote."))

  (define-reporter ((condition end-of-input-after-backquote) stream)
    (format stream "While reading backquote, expected an object when ~
                    input ended."))

  (define-reporter ((condition unquote-not-inside-backquote) stream)
    (format stream "~:[Unquote~;Splicing unquote~] not inside backquote."
            (splicing-p condition)))

  (define-reporter ((condition unquote-in-invalid-context) stream)
    (format stream "~:[Unquote~;Splicing unquote~] is illegal in ~A."
            (splicing-p condition)
            (context-name (context condition) language)))

  (define-reporter ((condition end-of-input-after-unquote) stream)
    (format stream "While reading ~:[~;splicing ~]unquote, expected an ~
                    object when input ended."
            (splicing-p condition)))

  (define-reporter ((condition object-must-follow-unquote) stream)
    (format stream "An object must follow a~:[~; splicing~] unquote."
            (splicing-p condition)))

  (define-reporter ((condition unquote-splicing-in-dotted-list) stream)
    (format stream "Splicing unquote at end of list (like a . ,@b)."))

  (define-reporter ((condition unquote-splicing-at-top) stream)
    (format stream "Splicing unquote as backquote form (like `,@foo)."))

;;; Conditions related to lists

  (define-reporter ((condition unterminated-list) stream)
    ;; Use the DELIMITER slot instead of a fixed character since the
    ;; reader macro may have been installed on a non-default
    ;; character.
    (format stream "While reading list, expected the ~
                    ~/eclector.base::describe-character-english/ when input ~
                    ended."
            (delimiter condition)))

  (define-reporter ((condition too-many-dots) stream)
      (format stream "A token consisting solely of multiple dots is ~
                    illegal."))

  (define-reporter ((condition invalid-context-for-consing-dot) stream)
    (format stream "A consing dot appeared in an illegal position."))

  (define-reporter ((condition end-of-input-after-consing-dot) stream)
    (format stream "While reading consing dot, expected an object when ~
                    input ended."))

  (define-reporter ((condition object-must-follow-consing-dot) stream)
    (format stream "An object must follow a consing dot."))

  (define-reporter ((condition multiple-objects-following-consing-dot) stream)
    (format stream "Only a single object can follow a consing dot."))

  (define-reporter ((condition invalid-context-for-right-parenthesis) stream)
    (format stream "Unmatched close parenthesis."))

;;; Conditions related SHARPSIGN-SINGLE-QUOTE

  (define-reporter ((condition end-of-input-after-sharpsign-single-quote) stream)
    (format stream "While reading function literal, expected a ~
                    function name when input ended."))

  (define-reporter ((condition object-must-follow-sharpsign-single-quote) stream)
    (format stream "An object must follow #'."))

;;; Conditions related to read-time evaluation

  (define-reporter ((condition end-of-input-after-sharpsign-dot) stream)
    (format stream "While reading an expression to be evaluated at ~
                    read-time, expected a form when input ended."))

  (define-reporter ((condition object-must-follow-sharpsign-dot) stream)
    (format stream "A form must follow #.."))

  (define-reporter ((condition read-time-evaluation-inhibited) stream)
    (format stream "Cannot evaluate expression at read-time because ~s ~
                    is false."
            '*read-eval*))

  (define-reporter ((condition read-time-evaluation-error) stream)
    (let ((expression (expression condition))
          (original-condition (original-condition condition)))
      (format stream "Read-time evaluation of expression ~s signaled ~
                      ~s: ~a"
              expression (type-of original-condition) original-condition)))

;;; Conditions related to characters

  (define-reporter ((condition end-of-input-after-backslash) stream)
    (format stream "While reading character name, expected a name when ~
                    input ended."))

  (define-reporter ((condition unterminated-single-escape-in-character-name) stream)
    (format stream "While reading character name, expected character ~
                    after the ~/eclector.base::describe-character-english/ ~
                    when input ended."
              (escape-char condition)))

  (define-reporter ((condition unterminated-multiple-escape-in-character-name) stream)
    (format stream "While reading character name, expected ~
                    the ~/eclector.base::describe-character-english/ when ~
                    input ended."
              (delimiter condition)))

  (define-reporter ((condition unknown-character-name) stream)
    (format stream "Unrecognized character name: ~s" (name condition)))

  (define-context sharpsign-c "the complex reader macro")

;;; Conditions related to rational numbers

  (define-reporter ((condition end-of-input-before-digit) stream)
    (format stream "While reading number, expected digit in base ~d ~
                    when input ended."
            (base condition)))

  (define-reporter ((condition digit-expected) stream)
    (format stream "The ~/eclector.base::describe-character-english/ is ~
                    not a digit in base ~D."
            (character-found condition) (base condition)))

  (define-reporter ((condition zero-denominator) stream)
    (format stream "The denominator of a rational number literal is 0."))

  (define-reporter ((condition invalid-radix) stream)
    (format stream "~d is too ~:[big~;small~] to be a radix."
            (radix condition) (< (radix condition) 2)))

  (define-reporter ((condition invalid-default-float-format) stream)
    (format stream "~a is not a valid ~a."
            (float-format condition) 'cl:*read-default-float-format*))

;;; Conditions related to block comments

  (define-reporter ((condition unterminated-block-comment) stream)
    ;; Use the DELIMITER slot instead of a fixed character since the
    ;; reader macro may have been installed on non-default (sub-)
    ;; character.
    (let ((delimiter-1 (delimiter condition))
          (delimiter-2 #\#))
      (format stream "While reading block comment, expected ~
                      \"~A~A\" (that is the ~
                      ~/eclector.base::describe-character-english/ ~
                      followed by the ~
                      ~/eclector.base::describe-character-english/) when ~
                      input ended."
              (string delimiter-1) (string delimiter-2)
              delimiter-1 delimiter-2)))

;;; Conditions related to arrays

  (define-context sharpsign-a "the general array reader macro")

  (define-reporter ((condition unterminated-vector) stream)
    ;; Use the DELIMITER slot instead of a fixed character since the
    ;; reader macro may have been installed on a non-default
    ;; character.
    (format stream "While reading vector, expected ~:c when input ~
                    ended."
            (delimiter condition)))

  (define-reporter ((condition too-many-elements) stream)
    (format stream "~a was specified to have length ~d, but ~d ~
                    element~:P ~:*~[were~;was~:;were~] found."
            (array-type condition)
            (expected-number condition)
            (number-found condition)))

  (define-reporter ((condition no-elements-found) stream)
    (format stream "~a was specified to have length ~d, but no ~
                    elements were found."
            (array-type condition) (expected-number condition)))

  (define-reporter ((condition incorrect-initialization-length) stream)
    (format stream "~a was specified to have length ~d along the ~:R ~
                    axis, but provided initial-contents don't ~
                    match:~%~a"
            (array-type condition)
            (expected-length condition)
            (1+ (axis condition))
            (datum condition)))

;;; Sharpsign S conditions

  (define-context sharpsign-s-type       "the structure type name in the structure literal reader macro")
  (define-context sharpsign-s-slot-name  "a structure slot name in the structure literal reader macro")
  (define-context sharpsign-s-slot-value "a structure slot value in the structure literal reader macro")

  (define-reporter ((condition non-list-following-sharpsign-s) stream)
    (format stream "A proper list must immediately follow #S."))

  (define-reporter ((condition no-structure-type-name-found) stream)
    (format stream "A symbol naming a structure type must be the first ~
                    element of the list following #S."))

  (define-reporter ((condition structure-type-name-is-not-a-symbol) stream)
    (format stream "~S should designate a structure type but is not a ~
                    symbol."
            (type-error-datum condition)))

  (define-reporter ((condition slot-name-is-not-a-symbol) stream)
    (format stream "~S should designate a structure slot but is not ~
                    a symbol."
            (type-error-datum condition)))

  (define-reporter ((condition no-slot-value-found) stream)
    (format stream "A slot value form must follow the slot name ~S."
            (slot-name condition)))

;;; Conditions related to pathnames

  (define-context sharpsign-p "the pathname reader macro")

;;; Conditions related to feature expressions

  (define-context :sharpsign-plus  "the #+ conditionalization reader macro")
  (define-context :sharpsign-minus "the #- conditionalization reader macro")

  (define-reporter ((condition feature-expression-type-error) stream)
    (format stream "Feature expression is not of type ~a:~%~a"
            (type-error-expected-type condition) (type-error-datum condition)))

  (define-reporter ((condition single-feature-expected) stream)
    (format stream "Bad feature expression- found multiple features ~
                    when only one was expected:~%~a"
            (features condition)))

;;; SHARPSIGN-{EQUALS,SHARPSIGN} conditions

  (define-reporter ((condition sharpsign-equals-label-defined-more-than-once) stream)
    (format stream "Label ~d defined more than once."
            (label condition)))

  (define-reporter ((condition sharpsign-equals-only-refers-to-self) stream)
    (format stream "Label ~d is defined as a reference to itself."
            (label condition)))

  (define-reporter ((condition sharpsign-sharpsign-undefined-label) stream)
    (format stream "Reference to undefined label #~d#." (label condition)))

  ) ; MACROLET DEFINE-REPORTER
