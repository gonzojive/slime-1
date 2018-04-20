;;; swank-c-p-c.lisp -- ILISP style Compound Prefix Completion
;;
;; Author: Luke Gorrie  <luke@synap.se>
;;         Edi Weitz  <edi@agharta.de>
;;         Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;;         Tobias C. Rittweiler <tcr@freebits.de>
;;         and others
;;
;; License: Public Domain
;;


(in-package :swank)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (swank-require :swank-util))

(defslimefun completions (string default-package-name)
  "Return a list of completions for a symbol designator STRING.

The result is the list (COMPLETION-SET COMPLETED-PREFIX), where
COMPLETION-SET is the list of all matching completions, and
COMPLETED-PREFIX is the best (partial) completion of the input
string.

Simple compound matching is supported on a per-hyphen basis:

  (completions \"m-v-\" \"COMMON-LISP\")
    ==> ((\"multiple-value-bind\" \"multiple-value-call\"
          \"multiple-value-list\" \"multiple-value-prog1\"
          \"multiple-value-setq\" \"multiple-values-limit\")
         \"multiple-value\")

\(For more advanced compound matching, see FUZZY-COMPLETIONS.)

If STRING is package qualified the result list will also be
qualified.  If string is non-qualified the result strings are
also not qualified and are considered relative to
DEFAULT-PACKAGE-NAME.

The way symbols are matched depends on the symbol designator's
format. The cases are as follows:
  FOO      - Symbols with matching prefix and accessible in the buffer package.
  PKG:FOO  - Symbols with matching prefix and external in package PKG.
  PKG::FOO - Symbols with matching prefix and accessible in package PKG.
"
  (multiple-value-bind (name package-name package internal-p)
      (parse-completion-arguments string default-package-name)
    (let* ((symbol-set  (symbol-completion-set
                         name package-name package internal-p
                         (default-compound-prefix-matcher :symbol)))
           (package-set (package-completion-set
                         name package-name package internal-p
                         (default-compound-prefix-matcher :package)))
           (combined-set (combined-completion-set
                          name package-name package internal-p
                          (default-compound-prefix-matcher :package)
                          (default-compound-prefix-matcher :symbol)))
           (completion-set
            (if (not combined-set)
                (format-completion-set (nconc symbol-set package-set)
                                       internal-p package-name)
                (sort combined-set #'string<))))
      (when completion-set
        (list completion-set (longest-compound-prefix completion-set))))))

(defun default-compound-prefix-matcher (prefix-type)
  "Returns the compound prefix matcher for the given type of prefix.

PREFIX-TYPE is one of :package, :symbol.

Returns a matching function that takes a `prefix' and a
`target' string and which returns T if `prefix' is a
compound-prefix of `target', and otherwise NIL."
  (check-type prefix-type (member :package :symbol))
  (ecase prefix-type
    (:package (make-compound-prefix-matcher '(#\. #\- #\/)))
    (:symbol (make-compound-prefix-matcher #\-))))

;;;;; Find completion set

(defun symbol-completion-set (name package-name package internal-p matchp)
  "Return the set of completion-candidates as strings."
  (format t "(symbol-completion-set ~S ~S ~S ~S x)~%"
          name package-name package internal-p)
  (when package
    (mapcar
     (completion-output-symbol-converter name)
     (mapcar #'symbol-name
             (find-matching-symbols name
                                    package
                                    (and (not internal-p)
                                         package-name)
                                    matchp)))))

(defun combined-completion-set (name package-name package internal-p
                                package-matchp symbol-matchp)
  "Return the set of completion-candidates as strings.

This function handles the case when PACKAGE-NAME is incomplete (i.e. package is
null) and NAME may or may not be incomplete.

The strings returned include the package prefix and symbol."
  (when (and name package-name (not package))
    (let* ((completed-package-prefixes
            (let ((single-colon-prefixes ;; example: ("cl-user:" "cl:")
                   (package-completion-set package-name
                                           nil
                                           package
                                           internal-p
                                           package-matchp)))
              (if (not internal-p)
                  single-colon-prefixes
                  (mapcar (lambda (package-prefix)
                            (concatenate 'string package-prefix ":"))
                          single-colon-prefixes))))
           (completed-package-names
            (mapcar 'strip-package-delimeter completed-package-prefixes))
           (completed-packages
            (mapcar 'guess-package completed-package-names))
           (completions-for-each-package-completion
            (mapcar (lambda (prefix completed-package-name completed-package)
                      (when completed-package
                        (mapcar
                         (lambda (completed-sym)
                           (concatenate 'string prefix completed-sym))
                         (symbol-completion-set name
                                                completed-package-name
                                                completed-package
                                                internal-p
                                                symbol-matchp))))
                    completed-package-prefixes
                    completed-package-names
                    completed-packages)))
      (apply #'concatenate
             'list
             completions-for-each-package-completion))))


(defun ends-in-colon (package-prefix)
  (check-type package-prefix string)
  (char= #\: (elt package-prefix (- (length package-prefix) 1))))

(defun strip-package-delimeter (package-prefix)
  (check-type package-prefix string)
  (assert (ends-in-colon package-prefix))
  (flet ((maybe-strip (x)
           (let ((len (length x)))
             (if (char= #\: (elt x (- len 1)))
                 (subseq x 0 (- len 1))
                 x))))
    (maybe-strip (maybe-strip package-prefix))))

(defun package-completion-set (name package-name package internal-p matchp)
  (declare (ignore package internal-p))
  (let ((packages (and (not package-name)
                       (find-matching-packages name matchp))))
    (values
     (mapcar (completion-output-package-converter name) packages)
     packages)))

(defun find-matching-symbols (string package external test)
  "Return a list of symbols in PACKAGE matching STRING.
TEST is called with two strings.  If EXTERNAL is true, only external
symbols are returned."
  (let ((completions '())
        (converter (completion-output-symbol-converter string)))
    (flet ((symbol-matches-p (symbol)
             (and (or (not external)
                      (symbol-external-p symbol package))
                  (funcall test string
                           (funcall converter (symbol-name symbol))))))
      (do-symbols* (symbol package)
        (when (symbol-matches-p symbol)
          (push symbol completions))))
    completions))

(defun find-matching-symbols-in-list (string list test)
  "Return a list of symbols in LIST matching STRING.
TEST is called with two strings."
  (let ((completions '())
        (converter (completion-output-symbol-converter string)))
    (flet ((symbol-matches-p (symbol)
             (funcall test string
                      (funcall converter (symbol-name symbol)))))
      (dolist (symbol list)
        (when (symbol-matches-p symbol)
          (push symbol completions))))
    (remove-duplicates completions)))

(defun find-matching-packages (name matcher)
  "Return a list of packages matching NAME with MATCHER.
MATCHER is a two-argument predicate.

Each element of the returned list is a string of the form 'pkg-name:'
where 'pkg-name' is a valid package name."
  (let ((converter (completion-output-package-converter name)))
    (remove-if-not (lambda (x)
                     (funcall matcher name (funcall converter x)))
                   (mapcar (lambda (pkgname)
                             (concatenate 'string pkgname ":"))
                           (loop for package in (list-all-packages)
                              nconcing (package-names package))))))

(defun find-matching-package-objects (name matcher)
  "Return a list of package objects matching NAME with MATCHER.
MATCHER is a two-argument predicate.

Each element of the returned list is a package object, rather than a suffixed
packaged string as returned by FIND-MATCHING-PACKAGES."
  (let* ((suffixed-package-names (find-matching-packages name matcher))
         (package-names (mapcar
                         (lambda (name)
                           (assert (char= #\: (elt name (- (length name) 1))))
                           (subseq name 0 (- (length name) 1)))
                         suffixed-package-names)))
    ;; TODO: Respect find-locally-nicknamed-package?
    (mapcar 'guess-package package-names)))


;; PARSE-COMPLETION-ARGUMENTS return table:
;;
;;  user behaviour |  NAME  | PACKAGE-NAME | PACKAGE
;; ----------------+--------+--------------+-----------------------------------
;; asdf     [tab]  | "asdf" |     NIL      | #<PACKAGE "DEFAULT-PACKAGE-NAME">
;;                 |        |              |      or *BUFFER-PACKAGE*
;; asdf:    [tab]  |   ""   |    "asdf"    | #<PACKAGE "ASDF">
;;                 |        |              |
;; asdf:foo [tab]  | "foo"  |    "asdf"    | #<PACKAGE "ASDF">
;;                 |        |              |
;; as:fo    [tab]  |  "fo"  |     "as"     | NIL
;;                 |        |              |
;; :        [tab]  |   ""   |      ""      | #<PACKAGE "KEYWORD">
;;                 |        |              |
;; :foo     [tab]  | "foo"  |      ""      | #<PACKAGE "KEYWORD">
;;
(defun parse-completion-arguments (string default-package-name)
  "Parse STRING as a symbol designator.
Return these values:
 SYMBOL-NAME
 PACKAGE-NAME, or nil if the designator does not include an explicit package.
 PACKAGE, generally the package to complete in. (However, if PACKAGE-NAME is
          NIL, return the respective package of DEFAULT-PACKAGE-NAME instead;
          if PACKAGE is non-NIL but a package cannot be found under that name,
          return NIL.)
 INTERNAL-P, if the symbol is qualified with `::'."
  (multiple-value-bind (name package-name internal-p)
      (tokenize-symbol string)
    (flet ((default-package ()
             (or (guess-package default-package-name) *buffer-package*)))
      (let ((package (cond
                       ((not package-name)
                        (default-package))
                       ((equal package-name "")
                        (guess-package (symbol-name :keyword)))
                       ((find-locally-nicknamed-package
                         package-name (default-package)))
                       (t
                        (guess-package package-name)))))
        (values name package-name package internal-p)))))

(defun completion-output-case-converter (input &optional with-escaping-p)
  "Return a function to convert strings for the completion output.
INPUT is used to guess the preferred case."
  (ecase (readtable-case *readtable*)
    (:upcase (cond ((or with-escaping-p
                        (and (plusp (length input))
                             (not (some #'lower-case-p input))))
                    #'identity)
                   (t #'string-downcase)))
    (:invert (lambda (output)
               (multiple-value-bind (lower upper) (determine-case output)
                 (cond ((and lower upper) output)
                       (lower (string-upcase output))
                       (upper (string-downcase output))
                       (t output)))))
    (:downcase (cond ((or with-escaping-p
                          (and (zerop (length input))
                               (not (some #'upper-case-p input))))
                      #'identity)
                     (t #'string-upcase)))
    (:preserve #'identity)))

(defun completion-output-package-converter (input)
  "Return a function to convert strings for the completion output.
INPUT is used to guess the preferred case."
  (completion-output-case-converter input))

(defun completion-output-symbol-converter (input)
  "Return a function to convert strings for the completion output.
INPUT is used to guess the preferred case. Escape symbols when needed."
  (let ((case-converter (completion-output-case-converter input))
        (case-converter-with-escaping (completion-output-case-converter input t)))
    (lambda (str)
      (if (or (multiple-value-bind (lowercase uppercase)
                  (determine-case str)
                ;; In these readtable cases, symbols with letters from
                ;; the wrong case need escaping
                (case (readtable-case *readtable*)
                  (:upcase   lowercase)
                  (:downcase uppercase)
                  (t         nil)))
              (some (lambda (el)
                      (or (member el '(#\: #\Space #\Newline #\Tab))
                          (multiple-value-bind (macrofun nonterminating)
                              (get-macro-character el)
                            (and macrofun
                                 (not nonterminating)))))
                    str))
          (concatenate 'string "|" (funcall case-converter-with-escaping str) "|")
          (funcall case-converter str)))))


(defun determine-case (string)
  "Return two booleans LOWER and UPPER indicating whether STRING
contains lower or upper case characters."
  (values (some #'lower-case-p string)
          (some #'upper-case-p string)))


;;;;; Compound-prefix matching

(defun make-compound-prefix-matcher (delimiter &key (test #'char=))
  "Returns a matching function that takes a `prefix' and a
`target' string and which returns T if `prefix' is a
compound-prefix of `target', and otherwise NIL.

Viewing each of `prefix' and `target' as a series of substrings
delimited by DELIMITER, if each substring of `prefix' is a prefix
of the corresponding substring in `target' then we call `prefix'
a compound-prefix of `target'.

DELIMITER may be a character, or a list of characters."
  (let ((delimiters (etypecase delimiter
                      (character (list delimiter))
                      (cons      (assert (every #'characterp delimiter))
                                 delimiter))))
    (lambda (prefix target)
      (declare (type simple-string prefix target))
      (loop with tpos = 0
            for ch across prefix
            always (and (< tpos (length target))
                        (let ((delimiter (car (member ch delimiters :test test))))
                          (if delimiter
                              (setf tpos (position delimiter target :start tpos))
                              (funcall test ch (aref target tpos)))))
            do (incf tpos)))))


;;;;; Extending the input string by completion

(defun longest-compound-prefix (completions &optional (delimiter #\-))
  "Return the longest compound _prefix_ for all COMPLETIONS."
  (flet ((tokenizer (string) (tokenize-completion string delimiter)))
    (untokenize-completion
     (loop for token-list in (transpose-lists (mapcar #'tokenizer completions))
           if (notevery #'string= token-list (rest token-list))
           ;; Note that we possibly collect the "" here as well, so that
           ;; UNTOKENIZE-COMPLETION will append a delimiter for us.
             collect (longest-common-prefix token-list)
             and do (loop-finish)
           else collect (first token-list))
     delimiter)))

(defun tokenize-completion (string delimiter)
  "Return all substrings of STRING delimited by DELIMITER."
  (loop with end
        for start = 0 then (1+ end)
        until (> start (length string))
        do (setq end (or (position delimiter string :start start) (length string)))
        collect (subseq string start end)))

(defun untokenize-completion (tokens &optional (delimiter #\-))
  (format nil (format nil "~~{~~A~~^~a~~}" delimiter) tokens))

(defun transpose-lists (lists)
  "Turn a list-of-lists on its side.
If the rows are of unequal length, truncate uniformly to the shortest.

For example:
\(transpose-lists '((ONE TWO THREE) (1 2)))
  => ((ONE 1) (TWO 2))"
  (cond ((null lists) '())
        ((some #'null lists) '())
        (t (cons (mapcar #'car lists)
                 (transpose-lists (mapcar #'cdr lists))))))


;;;; Completion for character names

(defslimefun completions-for-character (prefix)
  (let* ((matcher (make-compound-prefix-matcher #\_ :test #'char-equal))
         (completion-set (character-completion-set prefix matcher))
         (completions (sort completion-set #'string<)))
    (list completions (longest-compound-prefix completions #\_))))

(provide :swank-c-p-c)
