; compiler


; This compiler gets tokens and performs syntax parsing on them
; processes Jack files and generates corresponding VM files

#lang racket/base
;imports
 (require racket/string)
 (require "jack_tokenizer.rkt")
 (require "lexer.rkt")

; Define file extensions
(define JACK-FILE-END ".jack")
(define VM-FILE-END   ".vm")

 ; Global variables 
; Get the input folder path from command-line arguments

 (define argFolder (vector*-ref (current-command-line-arguments) 0)) ;path 
; Iterate through all .jack files in the input folder
 (for ([name (map path->string (directory-list argFolder))] #:when (string-suffix? name JACK-FILE-END))  
    (letrec (
        ; Generate output file name by replacing .jack with .vm
        [fileName (string-append (string-trim name JACK-FILE-END #:left? #f #:right? #t #:repeat? #f)
                                 VM-FILE-END)]
        [output-file (open-output-file (string-append argFolder "/" fileName)
                                       #:exists 'replace
                                       #:replace-permissions? #t)]
       ; Function to get non-void tokens from the lexer
       [get_non_val_token (lambda () (let
                                    ([tokenN (Nand2TetrisLexer input-file)])
                                    (cond 
                                        [(void? tokenN) (get_non_val_token)]
                                        [else tokenN]
                                    )
       ))]
         ; Open input .jack file
       [input-file (open-input-file (string-append argFolder "/" name))]
       ; Parse the input file using the VMjackParser
       [parser-output (VMjackParser get_non_val_token)]
    
    )
    ; Write the parser output to the VM file
    (display parser-output output-file) 
     ; Print the name of the processed file
    (writeln fileName )
     )
 )
 