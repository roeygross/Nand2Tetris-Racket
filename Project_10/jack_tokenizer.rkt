#lang racket/base
(require racket/list)
(require racket/path)
(require racket/file)
(require parser-tools/lex)
(require parser-tools/lex-sre)
(require "tokens.rkt")
(require "lexer.rkt")

(define (tokenize-file filename)
  (let* ((input (open-input-file filename))
         (tokens '()))
    (displayln (string-append "Tokenizing file: " (path->string filename)))
    (let loop ()
      (let ((token (Nand2TetrisLexer input)))
        (cond
          ((void? token) (loop))  ; ignore void tokens and continue
          ((eq? (token-name token) 'EOF)
           (begin
             (displayln "Reached EOF token")
             (close-input-port input)
             (reverse tokens)))  ; return tokens in correct order
          (else
           (begin
             (displayln (string-append "Found token: " (format "~a" token)))
             (set! tokens (cons token tokens))
             (loop))))))
    (displayln (string-append "Total tokens found: " (number->string (length tokens))))
    (reverse tokens)))

(define (jack-to-xml dir-path jack-file)
  (let* ([full-path (build-path dir-path jack-file)]
         [file-base-name (path->string (path-replace-extension jack-file #""))]
         [xml-file-path (build-path dir-path (string-append file-base-name "E.xml"))]
         [tokens (tokenize-file full-path)]
         [output (open-output-file xml-file-path #:exists 'replace)])
    (displayln (string-append "Processing file: " (path->string full-path)))
;    (fprintf output "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    (fprintf output "<tokens>\n")
    (if (null? tokens)
        (displayln "Warning: No tokens found in the file.")
        (begin
          (displayln (string-append "Writing " (number->string (length tokens)) " tokens to XML"))
          (for-each (lambda (token)
                      (displayln (string-append "Writing token: " (format "~a" token)))
                      (let* ([token-name (token-name token)]
                             [token-value (token-value token)]
                             [token-type (get-token-type token-name)])
                        (fprintf output "<~a> ~a </~a>\n"
                                 token-type
                                 (cond
                                   [(equal? token-value "<") "&lt;"]
                                   [(equal? token-value ">") "&gt;"]
                                   [(equal? token-value "&") "&amp;"]
                                   [else token-value])
                                 token-type)))
                    tokens)))
    (fprintf output "</tokens>\n")
    (close-output-port output)
    (displayln (string-append "Created file: " (path->string xml-file-path)))))

(define (get-token-type token-name)
  (cond
    [(symbol-token? token-name) "symbol"]
    [(keyword-token? token-name) "keyword"]
    [(eq? token-name 'INTEGER) "integerConstant"]
    [(eq? token-name 'STRING) "stringConstant"]
    [(eq? token-name 'ID) "identifier"]
    [else "unknown"]))

(define (symbol-token? token-name)
  (member (symbol->string token-name)
          '("JSYM-RROUND" "JSYM-LROUND" "JSYM-LCURLY" "JSYM-RCURLY" "JSYM-LREC" "JSYM-RREC"
            "JSYM-DOT" "JSYM-COMMA" "JSYM-DOTCOM" "JSYM-PLUS" "JSYM-MINUS" "JSYM-STAR"
            "JSYM-SLASH" "JSYM-ANDC" "JSYM-ORC" "JSYM-BIGER" "JSYM-SMALLER" "JSYM-EQULAS" "JSYM-NOTC")))

(define (keyword-token? token-name)
  (member (symbol->string token-name)
          '("CLASS" "CONSTRUCTOR" "FUNCTION" "METHOD" "FIELD" "STATIC" "INT" "VAR" "CHAR"
            "BOOLEAN" "VOID" "TRUE" "FALSE" "NULL" "THIS" "LET" "DO" "IF" "ELSE" "WHILE" "RETURN")))

(define (main args)
  (if (null? args)
      (displayln "Please provide a directory path.")
      (let ([dir-path (car args)])
        (for-each
         (lambda (file)
           (when (regexp-match? #rx"\\.jack$" (path->string file))
             (jack-to-xml dir-path file)))
         (directory-list dir-path)))))

(main (vector->list (current-command-line-arguments)))