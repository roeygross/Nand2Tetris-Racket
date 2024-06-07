
#lang racket/base
;importing
(require racket/string)
(require racket/list)

; exporting functions to make them available outside this module
(provide hack-add)
(provide hack-sub )
(provide hack-neg )
(provide hack-or  )
(provide hack-not )
(provide hack-and )
(provide hack-eq  )
(provide hack-gt  )
(provide hack-lt  )
(provide hack-push)
(provide hack-pop)
(provide hack-label)
(provide hack-goto)
(provide hack-if-goto)
(provide hack-function)
(provide hack-call)
(provide hack-return)
(provide hack-bootstrap)
(provide hack-set-namespace)


;global vars 
(define comp-count 0)
(define hack-namespace "") ; namespace for the current file
(define (update-comp-count) (set! comp-count (+ comp-count 1)) )
; function to set the namespace for the current file
(define (hack-set-namespace name) (set! hack-namespace name))



; format strings for command templates
(define FILL_COMMAND_FORNAT "~a")
(define LOAD_FORMAT "@~a")
(define DLOAD_FORNAT "@~a.~a")
(define Label_Format "LABEL_~a.~a")
(define Function_Format "FUNCTION_~a")
(define Return_Point_Format "ReturnPoint_~a_~a")
(define IF_Succeed_Format "IF_TRUE_~a")
(define IF_FAIL_Format "IF_FALSE_~a") 
(define (get_label name) (format Label_Format hack-namespace name))
(define (get_return_point name) (update-comp-count) (get_label (format Return_Point_Format name comp-count) )  )
(define (get_return_point_name name) (format Return_Point_Format name comp-count) )
(define (get_load_format formatX ) (format LOAD_FORMAT formatX))
(define (get_label_format formatX ) (format "(~a)" formatX ))

; template for binary operator template
(define hack-binary-operator-str (string-join (list "@SP" "M=M-1" "A=M" "D=M" "@SP" "A=M-1""M=M~aD") "\n"))
(define (hack-add) (format hack-binary-operator-str "+"))
(define (hack-sub) (format hack-binary-operator-str "-"))
(define (hack-or)  (format hack-binary-operator-str "|"))
(define (hack-and) (format hack-binary-operator-str "&"))

; template for unary operator template
(define hack-unary-operator-str (string-join (list "@SP" "A=M-1""M=~aM") "\n"))
(define (hack-neg) (format hack-unary-operator-str "-"))
(define (hack-not) (format hack-unary-operator-str "!"))

; template for for pushing a value onto the stack
(define hack-push-str (string-join (list FILL_COMMAND_FORNAT "@SP" "A=M" "M=D" "@SP" "M=M+1" ) "\n"))
(define (hack-push segment arg) 
(define segments-push (hash 
        "argument"  (lambda (val) (format (string-join  (list LOAD_FORMAT "D=A" "@ARG" "A=M" "A=A+D" "D=M" ) "\n")  val ))
        "local"     (lambda (val) (format (string-join  (list LOAD_FORMAT "D=A" "@LCL" "A=M" "A=A+D" "D=M" ) "\n")  val ))
        "this"      (lambda (val) (format (string-join  (list LOAD_FORMAT "D=A" "@THIS" "A=M" "A=A+D" "D=M") "\n")  val ))
        "that"      (lambda (val) (format (string-join  (list LOAD_FORMAT "D=A" "@THAT" "A=M" "A=A+D" "D=M") "\n")  val )) 
        "temp"      (lambda (val) (format (string-join  (list LOAD_FORMAT "D=M"                            ) "\n")  (+ (string->number val) 5) ) )
        "constant"  (lambda (val) (format (string-join  (list LOAD_FORMAT "D=A"                            ) "\n") val))
        "static"    (lambda (val) (format (string-join  (list DLOAD_FORNAT "D=M"                         ) "\n") hack-namespace val ))
        "pointer"   (lambda (val) (format (string-join  (list LOAD_FORMAT "D=M"                            ) "\n")  (cond [(string=? "0" val) "THIS"] 
                                                                                                                  [(string=? "1" val) "THAT"] 
                                                                                                             [else (error 'method-a "failed because ~a" "pointer pop value is not valid")])))))
(format hack-push-str ((hash-ref segments-push segment) arg)))


; template for popping a value from the stack
(define hack-pop-str (string-join (list "@SP" "A=M-1" "D=M" FILL_COMMAND_FORNAT "M=D" "@SP" "M=M-1") "\n"))
(define (hack-pop segment arg) 
(define (A-add-command val )
(string-join (map (lambda (_) "A=A+1") (range (string->number val)) ) "\n"))
(define segments-pop (hash 
        "argument" (lambda (val)  (string-trim (format (string-join (list "@ARG" "A=M" FILL_COMMAND_FORNAT) "\n") (A-add-command val))))
        "local"    (lambda (val)  (string-trim (format (string-join (list "@LCL" "A=M" FILL_COMMAND_FORNAT) "\n")  (A-add-command val))))
        "this"     (lambda (val)  (string-trim (format (string-join (list "@THIS" "A=M" FILL_COMMAND_FORNAT) "\n" )  (A-add-command val))))
        "that"     (lambda (val)  (string-trim (format (string-join (list "@THAT" "A=M" FILL_COMMAND_FORNAT) "\n") (A-add-command val))))
        "temp"     (lambda (val)  (format                                 LOAD_FORMAT  (+ (string->number val ) 5 )))
        "static"   (lambda (val)  (format                                 DLOAD_FORNAT hack-namespace val) ) 
        "pointer"   (lambda (val) (format                                 LOAD_FORMAT  (cond [(string=? "0" val) "THIS"] 
                                                                                        [(string=? "1" val) "THAT"] 
                                                                                        [else (error 'method-a "failed because ~a" "pointer pop value is not valid")])))))
        (format hack-pop-str ((hash-ref segments-pop segment) arg)))

; template for comparison operations
(define hack-comp-str (string-join (list "@SP" "A=M-1" "D=M" "A=A-1" "D=D-M" (get_load_format IF_Succeed_Format) "D;~a" "D=0" (get_load_format IF_FAIL_Format) "0;JMP" (get_label_format IF_Succeed_Format) "D=-1" (get_label_format IF_FAIL_Format) "@SP" "A=M-1" "A=A-1" "M=D" "@SP" "M=M-1") "\n"))
(define (hack-eq)  (update-comp-count) (format hack-comp-str comp-count "JEQ"  comp-count comp-count comp-count))
(define (hack-gt)  (update-comp-count) (format hack-comp-str comp-count "JLT"  comp-count comp-count comp-count))
(define (hack-lt)  (update-comp-count) (format hack-comp-str comp-count "JGT"  comp-count comp-count comp-count))

;goto
(define hack-goto-str (string-join (list (get_load_format Label_Format) "0;JMP") "\n"))
(define (hack-goto labelName) (format hack-goto-str hack-namespace labelName))

;label
(define hack-label-str (get_label_format Label_Format))
(define (hack-label labelName) (format hack-label-str hack-namespace labelName))

;if goto
(define hack-if-goto-str (string-join (list "@SP" "M=M-1" "A=M" "D=M" (get_load_format Label_Format) "D;JNE" ) "\n"))
(define (hack-if-goto labelName)  (format hack-if-goto-str  hack-namespace labelName))

;call to function
(define hack-call-str (string-join (list FILL_COMMAND_FORNAT FILL_COMMAND_FORNAT FILL_COMMAND_FORNAT FILL_COMMAND_FORNAT FILL_COMMAND_FORNAT LOAD_FORMAT "D=A" "@SP" "D=M-D" "@ARG" "M=D" "@SP" "D=M" "@LCL" "M=D" LOAD_FORMAT "0;JMP" FILL_COMMAND_FORNAT) "\n" ))
(define (hack-call funcName nArgs) (update-comp-count) (format hack-call-str
                                        (hack-push "constant"  (get_return_point funcName ))
                                        (format hack-push-str (string-join (list "@LCL" "D=M") "\n" ) )
                                        (format hack-push-str (string-join (list "@ARG" "D=M") "\n" ))
                                        (format hack-push-str (string-join (list "@THIS" "D=M") "\n" )) 
                                        (format hack-push-str (string-join (list "@THAT" "D=M") "\n"))
                                        (number->string (+ (string->number nArgs) 5 )) 
                                        (format Function_Format funcName )
                                        (hack-label (get_return_point_name funcName))))

;"create function"
(define hack-function-str (string-join (list FILL_COMMAND_FORNAT FILL_COMMAND_FORNAT) "\n" ) )
(define (hack-function funcName nArgs) (format hack-function-str 
        (get_label_format (format Function_Format funcName )) 
        (string-join (map (lambda (_) (hack-push "constant" "0")) (range (string->number nArgs)) ) "\n")
        
)) 

;"return from function"
(define hack-return-str (string-join (list "@LCL" "D=M" "@5" "A=D-A" "D=M" "@13" "M=D" FILL_COMMAND_FORNAT "@ARG" "D=M" "@SP" "M=D+1" FILL_COMMAND_FORNAT FILL_COMMAND_FORNAT FILL_COMMAND_FORNAT FILL_COMMAND_FORNAT "@13" "A=M" "0;JMP" ) "\n") )
(define (hack-return) 
(define (mini_recovery_segment segment) (string-join (list "@LCL" "M=M-1" "A=M" "D=M" (format LOAD_FORMAT segment) "M=D") "\n"))
(format hack-return-str 
        (hack-pop "argument" "0")
        (mini_recovery_segment "THAT") 
        (mini_recovery_segment "THIS") 
        (mini_recovery_segment "ARG") 
        (mini_recovery_segment "LCL")
))

; bootstrap -initialize the first running function
(define hack-bootstrap-str (string-join (list "@256" "D=A" "@SP" "M=D" FILL_COMMAND_FORNAT) "\n"))
(define hack-bootstrap (format hack-bootstrap-str  (hack-call "Sys.init" "0")) ) 
