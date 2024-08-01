#lang racket

(require racket/file)

(define output-file empty)
(define tokens-list empty)
(define tabs "")

(define (parse-file input-path output-path)
  (let ([input-content (file->string input-path)])
    (displayln (format "Debug: Read ~a lines from input file" (length (string-split input-content "\n"))))
    (set! tokens-list (regexp-split #px"\n" input-content))
    (set! tokens-list (cddr tokens-list))
    (displayln (format "Debug: tokens-list contains ~a tokens after processing" (length tokens-list)))
    (call-with-output-file output-path
      #:exists 'replace
      (lambda (out)
        (set! output-file out)
        (parse-class)))))

(define (parse-class)
  (displayln "Debug: Entering parse-class")
  (write-opening-tag "class")
  (write-token) ; class
  (write-token) ; class name
;  (write-token) ; {
  (displayln "Debug: Before parse-class-var-decs")
  (parse-class-var-decs)
  (displayln "Debug: Before parse-subroutine-decs")
  (parse-subroutine-decs)
  (when (not (null? tokens-list))
    (write-token)) ; }
  (write-closing-tag "class")
  (displayln "Debug: Exiting parse-class"))

(define (parse-class-var-decs)
  (let loop ()
    (when (and (not (null? tokens-list)) (class-var-dec?))
      (parse-class-var-dec)
      (loop))))

(define (parse-class-var-dec)
  (write-opening-tag "classVarDec")
  (write-token) ; static/field
  (write-token) ; type
  (write-token) ; var name
  (let loop ()
            (when (comma?)
                (write-token) ; ","
                (write-token) ; ID
                (loop)             
            )
        )
  (write-token) ; ;
  (write-closing-tag "classVarDec"))

(define (parse-subroutine-decs)
  (displayln (format "Debug: Entering parse-subroutine-decs with ~a tokens left" (length tokens-list)))
  (let loop ()
    (when (and (not (null? tokens-list)) (subroutine-dec?))
      (parse-subroutine-dec)
      (loop))))

(define (parse-subroutine-dec)
  (write-opening-tag "subroutineDec")
  (write-token) ; constructor/function/method
  (write-token) ; return type
  (write-token) ; name
  (write-token) ; (
  (parse-parameter-list)
  (write-token) ; )
  (parse-subroutine-body)
  (write-closing-tag "subroutineDec"))

(define (parse-parameter-list)
  (write-opening-tag "parameterList")
  (unless (is-next? ")")
    (write-token) ; type
    (write-token) ; var name
      (let loop ()
            (when (comma?)
                (write-token) ; ","
                (write-token) ; type
                (write-token) ; var name
                (loop)             
            )
        ))
  (write-closing-tag "parameterList"))

(define (parse-subroutine-body)
  (write-opening-tag "subroutineBody")
  (write-token) ; {
  (parse-var-decs)
  (parse-statements)
  (write-token) ; }
  (write-closing-tag "subroutineBody"))

(define (parse-var-decs)
  (let loop ()
    (when (var-dec?)
      (parse-var-dec)
      (loop))))

(define (parse-var-dec)
  (write-opening-tag "varDec")
  (write-token) ; var
  (write-token) ; type
  (write-token) ; var name
  (let loop ()
            (when (comma?)
                (write-token) ; ","
                (write-token) ; identifier  
                (loop)             
            )
        )
  (write-token) ; ;
  (write-closing-tag "varDec"))

(define (parse-statements)
  (write-opening-tag "statements")
  (let loop ()
    (cond
      [(let-statement?) (parse-let-statement) (loop)]
      [(if-statement?) (parse-if-statement) (loop)]
      [(while-statement?) (parse-while-statement) (loop)]
      [(do-statement?) (parse-do-statement) (loop)]
      [(return-statement?) (parse-return-statement) (loop)]
      [else (void)]))
  (write-closing-tag "statements"))

(define (parse-let-statement)
  (write-opening-tag "letStatement")
  (write-token) ; let
  (write-token) ; var name
  (when (is-next? "[")
    (write-token) ; [
    (parse-expression)
    (write-token)) ; ]
  (write-token) ; =
  (parse-expression)
  (write-token) ; ;
  (write-closing-tag "letStatement"))

(define (parse-if-statement)
  (write-opening-tag "ifStatement")
  (write-token) ; if
  (write-token) ; (
  (parse-expression)
  (write-token) ; )
  (write-token) ; {
  (parse-statements)
  (write-token) ; }
  (when (is-next? "else")
    (write-token) ; else
    (write-token) ; {
    (parse-statements)
    (write-token)) ; }
  (write-closing-tag "ifStatement"))

(define (parse-while-statement)
  (write-opening-tag "whileStatement")
  (write-token) ; while
  (write-token) ; (
  (parse-expression)
  (write-token) ; )
  (write-token) ; {
  (parse-statements)
  (write-token) ; }
  (write-closing-tag "whileStatement"))

(define (parse-do-statement)
  (write-opening-tag "doStatement")
  (write-token) ; do
  (parse-subroutine-call)
  (write-token) ; ;
  (write-closing-tag "doStatement"))

(define (parse-return-statement)
  (write-opening-tag "returnStatement")
  (write-token) ; return
  (unless (is-next? ";")
    (parse-expression))
  (write-token) ; ;
  (write-closing-tag "returnStatement"))

(define (parse-expression)
  (write-opening-tag "expression")
  (parse-term)
  (let loop ()
    (when (op?)
                (write-token) ; operation
                (parse-term)  
                (loop)             
            ))
  (write-closing-tag "expression"))

(define (parse-term)
  (write-opening-tag "term")
  (cond
    [(int-const?) (write-token)]
    [(string-const?) (write-token)]
    [(keyword-const?) (write-token)]
    [(is-next? "(")
     (write-token) ; (
     (parse-expression)
     (write-token)] ; )
    [(or (is-next? "-") (is-next? "~"))
     (write-token)
     (parse-term)]
    [(is-second? "[")
     (write-token) ; var name
     (write-token) ; [
     (parse-expression)
     (write-token)] ; ]
    [(or (is-second? "(") (is-second? "."))
     (parse-subroutine-call)]
    [else (write-token)]) ; var name
  (write-closing-tag "term"))

(define (parse-subroutine-call)
  (write-token) ; class/var name
  (if (is-next? "(")
      (begin
        (write-token) ; (
        (parse-expression-list)
        (write-token)) ; )
      (begin
        (write-token) ; .
        (write-token) ; subroutine name
        (write-token) ; (
        (parse-expression-list)
        (write-token))) ; )
  )

(define (parse-expression-list)
  (write-opening-tag "expressionList")
  (unless (is-next? ")")
    (parse-expression)
  (let loop ()
            (when (comma?)
                (write-token) ; ","
                (parse-expression)
                (loop)             
            )
        ))
  (write-closing-tag "expressionList")
  )

(define (write-opening-tag tag-name)
  (fprintf output-file "~a<~a>\n" tabs tag-name)
  (set! tabs (string-append tabs "  ")))

(define (write-closing-tag tag-name)
  (set! tabs (substring tabs 2))
  (fprintf output-file "~a</~a>\n" tabs tag-name))

(define (write-token)
      (define token (first tokens-list))
    (set! tokens-list (remove (first tokens-list) tokens-list))
    (displayln (string-append tabs token) output-file))

(define (is-next? data)
  (and (not (null? tokens-list))
       (string-contains? (car tokens-list) data)))

(define (is-second? data)
  (and (> (length tokens-list) 1)
       (string-contains? (cadr tokens-list) data)))

(define (class-var-dec?)
  (define result (or (string-contains? (car tokens-list) "static")
                     (string-contains? (car tokens-list) "field")))
  (displayln (format "Debug: class-var-dec? returned ~a for token ~a" result (car tokens-list)))
  result)

(define (subroutine-dec?)
  (define result (or (string-contains? (car tokens-list) "constructor")
                     (string-contains? (car tokens-list) "function")
                     (string-contains? (car tokens-list) "method")))
  (displayln (format "Debug: subroutine-dec? returned ~a for token ~a" result (car tokens-list)))
  result)

(define (var-dec?)
  (string-contains? (car tokens-list) "var"))

(define (let-statement?)
  (string-contains? (car tokens-list) "let"))

(define (if-statement?)
  (string-contains? (car tokens-list) "if"))

(define (while-statement?)
  (string-contains? (car tokens-list) "while"))

(define (do-statement?)
  (string-contains? (car tokens-list) "do"))

(define (return-statement?)
  (string-contains? (car tokens-list) "return"))

(define (comma?)
  (string-contains? (car tokens-list) ","))

(define (op?)
  (define result (or (string-contains? (car tokens-list) "+")
                     (string-contains? (car tokens-list) "//")
                     (string-contains? (car tokens-list) "=")
                     (string-contains? (car tokens-list) "-")
                     (string-contains? (car tokens-list) "&amp;")
                     (string-contains? (car tokens-list) "|")
                     (string-contains? (car tokens-list) "&lt")
                     (string-contains? (car tokens-list) "&gt")))
   (displayln (format "Debug: op? returned ~a for token ~a" result (car tokens-list)))
  result)

(define (int-const?)
  (string-contains? (car tokens-list) "integerConstant"))

(define (string-const?)
  (string-contains? (car tokens-list) "stringConstant"))

(define (keyword-const?)
  (and (string-contains? (car tokens-list) "keyword")
       (or (string-contains? (car tokens-list) "true")
           (string-contains? (car tokens-list) "false")
           (string-contains? (car tokens-list) "null")
           (string-contains? (car tokens-list) "this"))))
;
(define (rename file-name)
  (let* ([parts (regexp-match #rx"^(.*)\\.xml$" file-name)]
         [base-name (second parts)])
    (string-append base-name "To.xml")))

(define (parse-directory dir-path)
  (for ([file (directory-list dir-path)])
    (when (regexp-match? #rx"\\.xml$" (path->string file))
      (let* ([input-path (build-path dir-path file)]
             [output-file-name (rename (path->string file))]
             [output-path (build-path dir-path output-file-name)])
        (parse-file input-path output-path)))))


; נקודת הכניסה הראשית
(define (main dir-path)
  (parse-directory dir-path))

; הפעלת התוכנית
(main "C:\\Ekronot\\Ekronot4")