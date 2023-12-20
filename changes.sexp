(:changes
 (:release "0.10" nil
  (:item
   (:paragraph
    "The" "deprecated" "generic" "functions"
    (:symbol "eclector.parse-result:source-position")
    "and" (:symbol "eclector.parse-result:make-source-range") "have" "been"
    "removed" "."))

  (:item
   (:paragraph
    "The" "new" "reader" (:symbol "eclector.base:range-length") "can" "be"
    "applied" "to" "conditions" "of" "type"
    (:symbol "eclector.base:stream-position-condition") "(" "which"
    "includes" "almost" "all" "conditions" "related" "to" "syntax" "errors"
    ")" "to" "determine" "the" "length" "of" "the" "sub-sequence" "of" "the"
    "input" "to" "which" "the" "condition" "in" "question" "pertains" "."))

  (:item
   (:paragraph
    "Minor" "incompatible" "change")
   (:paragraph
    "The" "part" "of" "the" "labeled" "objects" "protocol" "that" "allows"
    "clients" "to" "construct" "parse" "results" "which" "represent"
    "labeled" "objects" "has" "been" "changed" "in" "an" "incompatible" "way"
    "." "The" "change" "allows" "parse" "results" "which" "represent"
    "labeled" "objects" "to" "have" "child" "parse" "results" "but" "requires"
    "that" "clients" "construct" "parse" "results" "which" "represent"
    "labeled" "objects" "differently" ":" "instead" "of" "eql-specializing"
    "the" (:tt "result") "parameters" "of" "methods" "on"
    (:symbol "eclector.parse-result:make-expression-result") "to"
    (:symbol "eclector.parse-result:**definition**") "and"
    (:symbol "eclector.parse-result:**reference**") "and" "receiving" "the"
    "labeled" "object" "in" "the" (:tt "children") "parameters" ","
    "the" (:tt "result") "parameters" "now" "have" "to" "be" "specialized"
    "to" "the" "classes" (:symbol "eclector.parse-result:definition") "and"
    (:symbol "eclector.parse-result:reference") "respectively" "." "The"
    "object" "passed" "as" "the" (:tt "result") "argument" "now" "contains"
    "the" "labeled" "object" "so" "that" "the" (:tt "children") "parameter"
    "can" "receive" "child" "parse" "results" ".")
   (:paragraph
    "This" "change" "is" "considered" "minor" "since" "the" "old" "mechanism"
    "described" "above" "was" "not" "documented" "." "For" "now" "," "the"
    "new" "mechanism" "also" "remains" "undocumented" "so" "that" "the"
    "design" "can" "be" "validated" "through" "experimentation" "before" "it"
    "is" "finalized" ".")))

 (:release "0.9" "2023-03-19"
  (:item
   (:paragraph
    "The" "deprecated" "function"
    (:symbol "eclector.concrete-syntax-tree:cst-read") "has" "been" "removed"
    "."))
  (:item
   (:paragraph
    (:symbol "eclector.reader:find-character") "receives" "characters"
    "names" "with" "unmodified" "case" "and" "is" "also" "called" "in" "the"
    (:tt "#\\<single character>") "case" "so" "that" "clients" "have" "more"
    "control" "over" "character" "lookup" "."))
  (:item
   (:paragraph
    "The" "new" "generic" "function"
    (:symbol "eclector.base:position-offset") "allows" "interested" "clients"
    "to" "refine" "the" "source" "positions" "of" "errors" "obtained" "by"
    "calling" (:symbol "eclector.base:stream-position") "."))
  (:item
   (:paragraph
    "Some" "condition" "and" "restart" "reports" "have" "been" "improved" "."))
  (:item
   (:paragraph
    "A" "discussion" "of" "the" "relation" "between" "circular" "objects"
    "and" "custom" "reader" "macros" "has" "been" "added" "to" "the"
    "manual"
    (:when "manual"
      "(" (:ref :section "Circular objects and custom reader macros") ")")
    "."))
  (:item
   (:paragraph
    "Problems" "in" "the" (:symbol "eclector.reader:fixup") "method" "for"
    "hash" "tables" "have" "been" "fixed" ":" "keys" "were" "not" "checked"
    "for" "circular" "structure" "and" "circular" "structures" "in" "values"
    "were" "not" "fixed" "up" "in" "some" "cases" "."))
  (:item
   (:paragraph
    "Eclector" "provides" "a" "new" "protocol" "for" "handling" "labeled"
    "objects" "," "that" "is" "the" "objects" "defined" "and" "referenced"
    "by" "the" (:tt "#=") "and" (:tt "##") "reader" "macros" "respectively"
    (:when "manual"
      "(" (:ref :section "Labeled objects and references") ")")
    "."))
  (:item
   (:paragraph
    "Eclector" "now" "avoids" "unnecessary" "fixup" "processing" "in"
    "object" "graphs" "with" "complicated" "definitions" "and" "references"
    ".")
   (:paragraph
    "Before" "this" "change" "," "cases" "like")
   (:code :common-lisp
    "#1=(1 #1# #2=(2 #2# ... #100=(100 #100#)))")
   (:paragraph "or")
   (:code :common-lisp
    "#1=(1 #2=(2 ... #2#) ... #1#)")
   (:paragraph
    "led" "to" "unnecessary" "and/or" "repeated" "traversals" "during"
    "fixup" "processing" "."))
  (:item
   (:paragraph
    "Fixup" "processing" "is" "now" "performed" "in" "parse" "result" "objects" ".")
   (:paragraph
    "Before" "this" "change" "," "something" "like")
   (:code :common-lisp
    "(eclector.concrete-syntax-tree:read-from-string \"#1=(#1#)\")")
   (:paragraph
    "produced" "a" "CST" "object" "," "say" (:tt "cst") "," "which" "failed"
    "to" "satisfy")
   (:code :common-lisp
    "(eq (cst:first cst)       cst)
(eq (cst:raw (first cst)) (cst:raw cst))")
   (:paragraph
    "The" "properties" "now" "hold" "."))
  (:item
   (:paragraph
    "Clients" "can" "use" "the" "new" "mixin" "classes"
    (:symbol "eclector.concrete-syntax-tree:definition-csts-mixin") "and"
    (:symbol "eclector.concrete-syntax-tree:reference-csts-mixin") "to"
    "represent" "labeled" "object" "definitions" "and" "references" "as"
    "instances" "of"
    (:symbol "eclector.concrete-syntax-tree:definition-cst")
    "and" (:symbol "eclector.concrete-syntax-tree:reference-cst")
    "respectively" "."))
  (:item
   (:paragraph
    "The" "stream" "position" "in" "conditions" "signaled" "by"
    (:symbol "eclector.reader::sharpsign-colon") "is" "now" "always"
    "present" "."))
  (:item
   (:paragraph
    "When" "Eclector" "is" "used" "to" "produce" "parse" "results" "," "it"
    "no" "longer" "confuses" "end-of-input" "with" "having" "read"
    (:symbol "nil") "when" (:tt "nil") "is" "used" "as" "the"
    (:tt "eof-value") "(" (:tt "nil") "makes" "sense" "as" "an"
    (:tt "eof-value") "in" "that" "case" "since" (:tt "nil") "is"
    "generally" "not" "a" "possible" "parse" "result" ")" "."))
  (:item
   (:paragraph
    "A" "detailed" "description" "of" "the" "constraints" "on" "return"
    "values" "of" "the" "generic" "functions" "in" "the" "Reader" "behavior"
    "protocol" "has" "been" "added" "to" "the" "manual"
    (:when "manual"
      "(" (:ref :section "Reader behavior protocol") ")")
    "."))
  (:item
   (:paragraph
    "The" (:tt "eclector-concrete-syntax-tree") "system" "now" "works"
    "with" "and" "requires" "version" "0.2" "of" "the"
    (:tt "concrete-syntax-tree") "system" "."))
  (:item
   (:paragraph
    "Eclector" "provides" "a" "new" "protocol" "for" "querying" "and"
    "binding" "behavior-changing" "aspects" "of" "the" "current" "state" "of"
    "the" "reader" "such" "as" "the" "current" "package" "," "the" "current"
    "readtable" "and" "the" "current" "read" "base"
    (:when "manual"
      "(" (:ref :section "Reader state protocol") ")")
    ".")
   (:paragraph
    "Clients" "can" "use" "this" "protocol" "to" "control" "the" "reader"
    "state" "in" "other" "ways" "than" "binding" "the" "Common" "Lisp"
    "variables" "," "for" "example" "by" "storing" "the" "values" "of"
    "reader" "state" "aspects" "in" "context" "objects" ".")
   (:paragraph
    "Furthermore" "," "implementations" "which" "use" "Eclector" "as" "the"
    "Common" "Lisp" "reader" "can" "use" "this" "protocol" "to" "tie" "the"
    (:symbol "cl:*readtable*") "aspect" "to" "the" (:symbol "cl:*readtable*")
    "variable" "instead" "of" "the" (:symbol "eclector.reader:*readtable*")
    "variable" ".")
   (:paragraph
    "The" "new" "protocol" "subsumes" "the" "purpose" "of" "the" "generic"
    "function" (:symbol "eclector.reader:call-with-current-package") "which"
    "is" "deprecated" "as" "of" "this" "Eclector" "version" "."))
  (:item
   (:paragraph
    "Eclector" "now" "provides" "and" "uses" "by" "default" "a" "relaxed"
    "version" "of" "the" (:symbol "eclector.reader::sharpsign-s") "reader"
    "macro" "function" "which" "requires" "the" "input" "following"
    (:tt "#S") "to" "be" "read" "as" "a" "list" "but" "not" "necessarily"
    "be" "literally" "written" "as" (:tt "(TYPE INITARG₁ VALUE₁ …)") ".")
   (:paragraph
    "A" "detailed" "discussion" "of" "the" "topic" "has" "been" "added" "to"
    "the" "manual"
    (:when "manual"
      "(" (:ref :section "Interpretation of Sharpsign C and Sharpsign S") ")")
    ".")))

 (:release "0.8" "2021-08-24"
  (:item
   (:paragraph
    "The" "default" (:symbol "eclector.reader:read-token") "method" "and"
    "the" "functions" (:symbol "eclector.reader::sharpsign-colon") "and"
    (:symbol "eclector.reader::sharpsign-backslash") "are" "now" "more"
    "efficient" "as" "well" "as" "less" "redundant" "in" "terms" "of"
    "repeated" "code" "."))
  (:item
   (:paragraph
    "The" "feature" (:symbol ":eclector-define-cl-variables") "now" "controls"
    "whether" "the" "file" (:tt "code/reader/variables.lisp") "is" "loaded"
    "and" "thus" "whether" "the" "variables"
    (:symbol "eclector.reader:*package*") ","
    (:symbol "eclector.reader:*read-eval*") "," "etc" "." "are" "defined" ".")))

 (:release "0.7" "2021-05-16"
  (:item
   (:paragraph
    "The" "incorrectly" "committed" "generic" "function"
    (:symbol "eclector.reader:check-symbol-token") "has" "been" "fixed" "."))
  (:item
   (:paragraph
    "Empty" "escape" "ranges" "like" (:tt "||") "are" "no" "longer"
    "interpreted" "as" "potential" "numbers" "."))
  (:item
   (:paragraph
    "The" "default" (:symbol "eclector.reader:interpret-symbol") "method"
    "now" "signals" "specific" "conditions" "and" "offers" "restarts" "for"
    "recovering" "from" "situations" "related" "to" "non-existent" "packages"
    "and" "symbols" "as" "well" "as" "non-exported" "symbols" ".")
   (:paragraph
    "The" "default" "error" "recovery" "strategy" "for" "invalid" "symbols"
    "now" "constructs" "an" "uninterned" "symbol" "of" "the" "given" "name"
    "instead" "of" "using" (:symbol "nil") "."))
  (:item
   (:paragraph
    "The" "\"consing dot\"" "is" "no" "longer" "accepted" "in"
    "sub-expressions" "of" (:symbol "eclector.reader::left-parenthesis") ".")
   (:paragraph
    "At" "the" "same" "time" "," "it" "is" "now" "possible" "to" "recover"
    "from" "encountering" "the" "\"consing dot\"" "in" "invalid" "positions"
    "."))
  (:item
   (:paragraph
    "The" "default" (:symbol "eclector.reader:interpret-token") "method" "has"
    "been" "optimized" "substantially" "."))
  (:item
   (:paragraph
    "The" (:symbol "eclector.reader:*client*") "variable" "and" "the"
    "source" "location" "protocol" "have" "been" "moved" "to" "a" "new"
    (:tt "base") "module" "and" "package" (:tt "eclector.base") "which"
    "the" (:tt "reader") "module" "and" "the" (:tt "eclector.reader")
    "package" "can" "use" "." "This" "structure" "allows" "code" "in" "the"
    (:tt "reader") "module" "to" "work" "with" "source" "locations" ".")
   (:paragraph
    "The" "name" (:symbol "eclector.base:*client*") "remains" "exported" "as"
    (:symbol "eclector.reader:*client*") ".")
   (:paragraph
    "The" "old" "names" (:symbol "eclector.parse-result:source-position")
    "and" (:symbol "eclector.parse-result:make-source-range") "still" "exist"
    "but" "are" "now" "deprecated" "and" "will" "be" "removed" "in" "a"
    "future" "release" "."))
  (:item
   (:paragraph
    "Conditions" "signaled" "by" "code" "in" "the" (:tt "reader") "module"
    "now" "include" "source" "positions" "which" "are" "obtained" "by"
    "calling" (:symbol "eclector.base:source-position") ".")))

 (:release "0.6" "2020-11-29"
  (:item
   (:paragraph
    "Bogus" (:tt "nil") "parse" "results" "are" "no" "longer" "generated"
    "by" (:symbol "eclector.parse-result:make-skipped-input-result") "calls"
    "when" (:symbol "cl:*read-suppress*") "is" "true" "."))
  (:item
   (:paragraph
    "The" "new" "generic" "functions"
    (:symbol "eclector.reader:read-maybe-nothing") "and"
    (:symbol "eclector.reader:call-as-top-level-read") "give" "clients"
    "additional" "entry" "points" "to" "the" "reader" "as" "well" "as"
    "customization" "possibilities" "." "With" "these" "functions" "," "the"
    "chain" "of" "functions" "calls" "for" "a" (:symbol "read") "call"
    "looks" "like" "this:")
   (:code nil "eclector.reader:read
  eclector.reader:call-as-top-level-read
    eclector.reader:read-common
      eclector.reader:read-maybe-nothing
        ...
          eclector.reader:read-char
          eclector.reader:peek-char")
   (:paragraph
    "Diagrams" "which" "illustrate" "the" "relations" "between" "the" "new"
    "and" "existing" "functions" "have" "been" "added" "to" "the" "manual"
    (:when "manual"
      "(" (:ref :figure "read-call-sequence-ordinary") ","
      (:ref :figure "read-call-sequence-customization") ","
      (:ref :figure "read-call-sequence-parse-result") ")")
    "."))
  (:item
   (:paragraph
    "The" "function" (:symbol "eclector.reader::read-rational") "now"
    "better" "respects" "the" "value" "of" (:symbol "*read-suppress*") "."))
  (:item
   (:paragraph
    "Fix" "return" "value" "of"
    (:symbol "eclector.readtable:set-syntax-from-char") "," "fix"
    (:symbol "(setf eclector.readtable:syntax-from-char)") "to" "also" "copy"
    "the" "macro" "character" "information" "."))
  (:item
   (:paragraph
    "The" "semicolon" "reader" "macro" "now" "consumes" "the" "terminating"
    "newline" "character" "."))
  (:item
   (:paragraph
    "Eclector" "now" "provides" "the" "generic" "function"
    (:symbol "eclector.reader:wrap-in-function") "."))
  (:item
   (:paragraph
    "Reset" (:symbol "eclector.reader::*list-reader*") "around" "recursive"
    "read" "in" (:symbol "eclector.reader::sharpsign-dot") "."))
  (:item
   (:paragraph
    "Implement" "and" "default" "to" "relaxed" "syntax" "for"
    (:symbol "eclector.reader::sharpsign-c") "." "The" "strict" "version" "is"
    "still" "available" "as" (:symbol "eclector.reader:strict-sharpsign-c")
    "and" "can" "be" "installed" "into" "a" "custom" "readtable" ".")
   (:paragraph
    "A" "detailed" "discussion" "of" "the" "topic" "has" "been" "added" "to"
    "the" "manual"
    (:when "manual"
      "(" (:ref :section "Interpretation of Sharpsign C and Sharpsign S") ")")
    "."))
  (:item
   (:paragraph
    "Eclector" "can" "now" "recover" "from" "reading" "invalid" "inputs"
    "like" (:tt "..") "and" (:tt "...") "."))
  (:item
   (:paragraph
    "Implement" "and" "default" "to" "relaxed" "syntax" "for"
    (:symbol "eclector.reader::sharpsign-single-quote") "." "The" "strict"
    "version" "is" "still" "available" "as"
    (:symbol "eclector.reader:strict-sharpsign-single-quote") "and" "can"
    "be" "installed" "into" "a" "custom" "readtable" ".")
   (:paragraph
    "A" "detailed" "discussion" "of" "the" "topic" "has" "been" "added" "to"
    "the" "manual"
    (:when "manual"
      "(" (:ref :section "Interpretation of Backquote and Sharpsign Single Quote") ")")
    "."))
  (:item
   (:paragraph
    "Eclector" "now" "provides" "the" "generic" "function"
    (:symbol "eclector.reader:check-symbol-token") "."))
  (:item
   (:paragraph
    "Input" "of" "the" "form" (:tt "PACKAGE::||") "is" "now" "correctly"
    "read" "as" "a" "symbol" "."))
  (:item
   (:paragraph
    "Eclector" "can" "now" "recover" "from" "reading" "the" "invalid"
    "input" (:tt ":") ".")))

 (:release "0.5" "2020-06-09"
  (:item
   (:paragraph
    "The" "generic" "function"
    (:symbol "eclector.reader:call-with-current-package") "has" "been"
    "added" "."))
  (:item
   (:paragraph
    "The" "previously" "missing" "functions"
    (:symbol "eclector.parse-result:read-preserving-whitespace") "and"
    (:symbol "eclector.parse-result:read-from-string") "have" "been" "added"
    "."))
  (:item
   (:paragraph
    "The" "previously" "missing" "functions"
    (:symbol "eclector.concrete-syntax-tree:read-preserving-whitespace")
    "and" (:symbol "eclector.concrete-syntax-tree:read-from-string") "have"
    "been" "added" "."))
  (:item
   (:paragraph
    "The" "function" (:symbol "eclector.concrete-syntax-tree:cst-read") "has"
    "been" "renamed" "to" (:symbol "eclector.concrete-syntax-tree:read") "."
    (:symbol "eclector.concrete-syntax-tree:cst-read") "still" "exists" "but"
    "is" "deprecated" "and" "will" "be" "removed" "in" "a" "future" "version"
    "."))
  (:item
   (:paragraph
    "Quasiquote" "and" "unquote" "are" "now" "opt-out" "instead" "of" "opt-in"
    "." "This" "allows" "quasiquotation" "in" "custom" "reader" "macros" "by"
    "default" "." "The" "new" "macro"
    (:symbol "eclector.reader::with-forbidden-quasiquotation") "is" "used" "by"
    "Eclector" "(" "and" "can" "be" "used" "in" "custom" "reader" "macros" ")"
    "to" "control" "this" "behavior" "."))
  (:item
   (:paragraph
    "A" "method" "on" (:symbol "eclector.readtable:readtablep") "for" "the"
    "simple" "readtable" "implementation" "has" "been" "added" "."))
  (:item
   (:paragraph
    "The" "condition" "type" (:symbol "eclector.base:end-of-file") "is" "now"
    "a" "subtype" "of" (:symbol "cl:stream-error") "but" "not" "of"
    (:symbol "cl:reader-error") "."))
  (:item
   (:paragraph
    "An" "error" "is" "now" "always" "signaled" "independently" "of" "the"
    "the" "value" "of" "the" (:tt "eof-error") "parameter" "when" "the" "end"
    "of" "input" "is" "encountered" "a" "after" "single" "escape" "or"
    "within" "a" "multiple" "escape" "." "The" "new" "error" "conditions"
    (:symbol "eclector.reader:unterminated-single-escape") "and"
    (:symbol "eclector.reader:unterminated-multiple-escape") "are" "signaled"
    "in" "such" "situations" "."))
  (:item
   (:paragraph
    "The" "set" "invalid" "sub-characters" "for" (:tt "#") "now" "conforms"
    "to" "the" "specification" "."))
  (:item
   (:paragraph
    "The" "value" "of" (:symbol "cl:*read-base*") "is" "now" "used"
    "correctly" "when" "distinguishing" "numbers" "and" "symbols" "."))
  (:item
   (:paragraph
    "When" "a" "number" "with" "a" "denominator" "of" "zero" "is" "read"
    "the" "new" "condition" (:symbol "eclector.reader:zero-denominator") "is"
    "signaled" "."))
  (:item
   (:paragraph
    "The" "function" (:symbol "eclector.reader:read-delimited-list") "has"
    "been" "added" "."))
  (:item
   (:paragraph
    "The" "reader" "macro" "function" (:symbol "eclector.reader::sharpsign-s")
    "now" "accepts" "string" "designators" "as" "slot" "names" "."))
  (:item
   (:paragraph
    "The" "reader" "macro" "functions"
    (:symbol "eclector.reader::sharpsign-equals") "and"
    (:symbol "eclector.reader::sharpsign-sharpsign") "respect" "the" "value"
    "of" (:symbol "cl:*read-suppress*") "."))
  (:item
   (:paragraph
    "The" "default" "methods" "on" "the" "generic" "function"
    (:symbol "eclector.reader:fixup") "now" "works" "correctly" "for"
    (:symbol "standard-object") "instances" "with" "unbound" "slots" "."))
  (:item
   (:paragraph
    "The" "reader" "macro" "function"
    (:symbol "eclector.reader::left-parenthesis") "now" "always" "reads"
    "until" (:tt "#\\)") "," "not" "some" "\"opposite\"" "character" "."))
  (:item
   (:paragraph
    (:symbol "eclector.reader:*skip-reason*") "is" "now" "set" "correctly"
    "when" "a" "line" "comment" "at" "the" "end" "of" "input" "is" "read"
    "."))
  (:item
   (:paragraph
    "In" "almost" "all" "situations" "in" "which" "Eclector" "signals" "a"
    "syntax" "error" "," "a" "restart" "named"
    (:symbol "eclector.reader:recover") "is" "now" "established" "which" ","
    "when" "invoked" "performs" "some" "action" "which" "allows" "the"
    "remainder" "of" "the" "expression" "to" "be" "read" "." "The"
    "convenience" "function" (:symbol "eclector.reader:recover") "can" "be"
    "used" "to" "invoke" "the" "restart" ".")))

 (:release "0.4" "2019-05-11"
  (:item
   (:paragraph
    "The" "reader" "macro" "function"
    (:symbol "eclector.reader::sharpsign-plus-minus") "now" "sets"
    (:symbol "eclector.reader:*skip-reason*") "so" "that" "parse" "results"
    "can" "be" "created" "with" "an" "accurate" "\"reason\"" "value" "."))
  (:item
   (:paragraph
    "Constituent" "traits" "are" "now" "represented" "and" "used" "properly"
    "."))
  (:item
   (:paragraph
    "The" "lambda" "lists" "of" "the" "functions"
    (:symbol "eclector.reader:read-char") "and"
    (:symbol "eclector.reader:peek-char") "have" "been" "fixed" "."))
  (:item
   (:paragraph
    "The" "function" (:symbol "eclector.reader::read-rational") "now"
    "respects" (:symbol "cl:*read-suppress*") "and" "handles" "inputs" "of"
    "the" "form" (:tt "1 2") "correctly" "."))
  (:item
   (:paragraph
    "The" "reader" "macro" "function" (:symbol "eclector.reader::sharpsign-r")
    "now" "handles" (:symbol "cl:*read-suppress*") "better" "."))
  (:item
   (:paragraph
    "The" "default" "method" "on" "the" "generic" "function"
    (:symbol "eclector.reader:interpret-token") "now" "distinguishes"
    "positive" "and" "negative" "float" "zeros" "and" "uses" "radix" "10"
    "instead" "of" "the" "value" "of" (:symbol "cl:*read-base*") "for"
    "float" "digits" "."))
  (:item
   (:paragraph
    "The" "input" (:tt ".||") "is" "now" "interpreted" "as" "a" "symbol"
    "instead" "of" "the" "\"consing dot\"" "."))
  (:item
   (:paragraph
    "Long" "lists" "are" "now" "read" "into" "concrete" "syntax" "tree"
    "results" "without" "relying" "on" "unbounded" "recursion" "."))
  (:item
   (:paragraph
    "Syntax" "errors" "in" "the" "initial" "contents" "part" "of" (:tt "#A")
    "expressions" "now" "signal" "appropriate" "errors" "."))
  (:item
   (:paragraph
    "Source" "ranges" "of" "parse" "results" "no" "longer" "include"
    "whitespace" "which" "followed" "the" "corresponding" "expression" "in"
    "the" "input" "."))
  (:item
   (:paragraph
    "The" "lambda" "list" "of" "the" "function"
    (:symbol "eclector.parse-result:read") "is" "now" "accurate" ".")))

 (:release "0.3" "2018-11-28"
  (:item
   (:paragraph
    "The" "function" (:symbol "eclector.reader:peek-char") "has" "been"
    "added" "." "The" "new" "function" "is" "like" (:symbol "cl:peek-char")
    "but" "signals" "Eclector" "conditions" "and" "uses" "the" "Eclector"
    "readtable" "."))
  (:item
   (:paragraph
    "The" "function" (:symbol "eclector.reader:read-from-string") "has"
    "been" "added" "." "The" "new" "function" "is" "like"
    (:symbol "cl:read-from-string") "but" "uses" "Eclector's" "reader"
    "implementation" "."))
  (:item
   (:paragraph
    "The" "reader" "macro" "function" (:symbol "eclector.reader::sharpsign-s")
    "and" "the" "generic" "function"
    (:symbol "eclector.reader:make-structure-instance") "have" "been" "added"
    "." "Eclector" "does" "not" "define" "any" "methods" "on" "the" "latter"
    "generic" "function" "since" "there" "is" "no" "portable" "way" "of"
    "creating" "a" "structure" "instance" "when" "only" "the" "symbol"
    "naming" "the" "structure" "is" "known" "."))
  (:item
   (:paragraph
    "The" "generic" "function" (:symbol "eclector.reader:interpret-symbol")
    "is" "now" "called" "when" "the" "reader" "creates" "uninterned" "symbols"
    "."))
  (:item
   (:paragraph
    "The" "generic" "function" (:symbol "eclector.reader:fixup") "now"
    "accepts" "a" "client" "object" "as" "the" "the" "argument" "."))
  (:item
   (:paragraph
    "In" "the" "generic" "functions"
    (:symbol "eclector.reader:wrap-in-quasiquote") ","
    (:symbol "eclector.reader:wrap-in-unquote") "and"
    (:symbol "eclector.reader:wrap-in-unquote-splicing") "," "the" "client"
    "parameter" "is" "now" "the" "first" "parameter" "."))
  (:item
   (:paragraph
    "The" "generic" "function" (:symbol "eclector.reader:wrap-in-quote")
    "has" "been" "added" ".")))

 (:release "0.2" "2018-08-13"
  (:item
   (:paragraph
    "The" (:tt "concrete-syntax-tree") "module" "has" "been" "generalized"
    "into" "a" (:tt "parse-result") "module" "which" "provides" "a" "protocol"
    "for" "constructing" "arbitrary" "custom" "parse" "results" "." "The"
    (:tt "concrete-syntax-tree") "module" "is" "now" "based" "on" "this"
    "new" "module" "but" "can" "be" "used" "as" "before" "by" "clients" "."))
  (:item
   (:paragraph
    "The" "default" "value" "of" "the" (:tt "eof-error-p") "parameter" "of"
    "the" (:symbol "eclector.reader:read-char") "function" "is" "now" "true"
    ".")))

 (:release "0.1" "2018-08-10"
  (:item
   (:paragraph
    "Eclector" "was" "created" "by" "extracting" "the" "reader" "module"
    "from" "the" "SICL" "repository" "."))
  (:item
   (:paragraph
    "The" "initial" "release" "includes" "many" "improvements" "over" "the"
    "original" "SICL" "reader" "," "particularly" "in" "the" "areas" "of"
    "customizability" "," "error" "reporting" "and" "constructing" "parse"
    "results" "."))))
