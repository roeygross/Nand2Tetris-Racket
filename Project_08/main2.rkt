
#lang racket/base
;importing
(require racket/string)
(require racket/list)
(require "commands2.rkt")

;create a hash table mapping string to function (from "command.rkt")  
(define commands (hash "push"   hack-push  "pop"        hack-pop  
                    "add"    hack-add    "sub"       hack-sub 
                    "or"     hack-or     "neg"       hack-neg 
                    "not"     hack-not    "and"      hack-and
                    "eq"      hack-eq     "gt"       hack-gt
                    "lt"      hack-lt     "label"    hack-label 
                    "goto"    hack-goto   "if-goto"  hack-if-goto 
                    "function" hack-function "call"  hack-call  
                    "return"   hack-return ))

;function to process a VM file and convert it to an ASM file
(define (ProcessVMToASM  inputFile outputFile)
  (define (analyze fileLine) ;apply each line - <command> <arg>
    (let* ([splitted (string-split fileLine " ")] [command (car splitted)])
      (apply (hash-ref commands command) (take-right splitted (- (length splitted) 1))))) ;apply translation from commands.rkt
  (define (looped fileo) ;proccessing (parsing) each line in the file
    (let ([line (read-line fileo)])
      (cond
        [(eof-object? line)  (void) ] 
        [(regexp-match? #rx"^((//| ).*)?$" (string-trim line)) (looped fileo)]   ;not a command (note etc)
        [else 
        (display (analyze (string-trim  (regexp-replace #rx"//.*" line "" ))) outputFile)   ;run analyzer on each command line
        (display "\n" outputFile )  
          (looped fileo)
        ]))) 

  (looped inputFile))
(let* ( ;""main" - opens file, and runs the translator on all the vm files in it"
  [namespaceFolder (vector*-ref (current-command-line-arguments) 0)]
  [namespace (last (string-split namespaceFolder "\\"))]
  ; map of the command name to the import from commands.rkt
  [outputFileName (string-append namespaceFolder "/" namespace ".asm")]
  [outputFile (open-output-file outputFileName #:exists 'replace #:replace-permissions? #t)]
  ; Check for Sys.vm existence aka bootstrap
  [hasSysVM (not (file-exists? (string-append namespaceFolder "/Sys.vm")))])
  ; Write bootstrap only if Sys.vm doesn't exist
  (when (not hasSysVM)
    (display (string-append hack-bootstrap "\n") outputFile))

  ;writing the bootstrap in the head of the file
  ;lets anlayze each file and add it to the file ... 

  (for ([name (map path->string (directory-list namespaceFolder))] #:when (string-suffix? name ".vm")) ; for each .vm file
  (let ([inputFile (open-input-file (string-append namespaceFolder "/" name ))])  ;open <input>.vm
    (hack-set-namespace (string-trim name ".vm" #:right? #t #:left? #f)) ;set namespace to path without vm
    (ProcessVMToASM inputFile outputFile) ;translate the command

  )
  ))
  