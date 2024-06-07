
#lang racket/base
(require racket/string)
(require racket/list)
(require "commands.rkt")

(define commands (hash "push"   hack-push  "pop"        hack-pop ;create a hash table mapping string to function (from "command.rkt")  
                    "add"    hack-add    "sub"       hack-sub 
                    "or"     hack-or     "neg"       hack-neg 
                    "not"     hack-not    "and"      hack-and
                    "eq"      hack-eq     "gt"       hack-gt
                    "lt"      hack-lt     
                  ))

(define (ProcessVMToASM  inputFile outputFile);function to process a VM file and convert it to an ASM file
  (define (analyze fileLine) ;apply each line - <command> <arg>
    (let* ([splitted (string-split fileLine " ")] [command (car splitted)])
      (apply (hash-ref commands command) (take-right splitted (- (length splitted) 1))))) ;apply translation from commands.rkt
  (define (looped fileo) ;proccessing (parsing) each line in the file
    (let ([line (read-line fileo)])
      (cond
        [(eof-object? line)  (void) ] 
        [(regexp-match? #rx"^((//| ).*)?$" (string-trim line)) (looped fileo)] ;not a command (note etc)
        [else 
        (display (analyze (string-trim  (regexp-replace #rx"//.*" line "" ))) outputFile)  ;run analyzer on each command line
        (display "\n" outputFile )  
          (looped fileo)
        ]))) 

  (looped inputFile))
(let* ( ;""main" - open file, and runs the compiler on it"
  [input_file (vector*-ref (current-command-line-arguments) 0)]; get the input fileName from first sysarg
  [output_file (string-append (string-trim input_file ".vm" #:right? #t #:left? #f) ".asm")] ;output is "input - .vm + .asm"
  [outputFile (open-output-file output_file #:exists 'replace #:replace-permissions? #t)]);create and open output file
  ;writing the base of the file
  
  (let ([inputFile (open-input-file input_file)]);open input for reading
    
    (hack-set-namespace (string-trim (last (string-split input_file "\\") ) ".vm" #:right? #t #:left? #f)) ;set namespace to path without vm
    (ProcessVMToASM inputFile outputFile) ;run the translator over the compiler
  )
  )