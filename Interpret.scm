#lang r5rs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some functions for manipulating lambda expressions.
;;
;; (make-abs <var> <expr>) creates the encoding for an abstraction
;; (var-of <abs>) return the variable of an abstraction
;; (body-of <abs>) return the body of an abstraction
;;
;; (make-app <rator> <rand>) creates the encoding for an application
;; (rator-of <app>) return the function of an application
;; (rand-of <app>) return the argument of an application
;;
;; (abs? <expr>) predicate to identify abstractions
;; (app? <expr>) predicate to identify applications
;; (var? <expr>) predicate to identify variables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-abs (lambda (var body) (list 'fun var body)))
(define make-app (lambda (rator rand) (list rator rand)))

(define abs? (lambda (expr) 
  (and (list? expr) (= (length expr) 3) (eqv? 'fun (car expr)))))
(define app? (lambda (expr)
  (and (list? expr) (= (length expr) 2))))
(define var? symbol?)

(define var-of cadr)
(define body-of caddr)

(define rator-of car)
(define rand-of cadr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Substitution 
;; - Implements Dynamic Binding
;;
;; [e/x]y = y  (if x != y)
;; [e/x]x = e
;; [e/x](e1 e2) = ([e/x]e1 [e/x]e2)
;; [e/x](fun y e1) (fun y [e/x]e1)  (if x != y) 
;; [e/x](fun x e1) = (fun x e1)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define substd (lambda (e x expr)   
    (cond 
      ((abs? expr)
         (if (eqv? x (var-of expr))
             expr        ;; [e/x](fun x e1) = (fun x e1)
             (make-abs   ;; [e/x](fun y e1) = (fun y [e/x]e1)
                (var-of expr) (substd e x (body-of expr))
             )
         )
      )
      ((app? expr)      ;; [e/x](e1 e2) = ([e/x]e1 [e/x]e2)
         (make-app (substd e x (rator-of expr))
                   (substd e x (rand-of expr))
         )
      )
      ((var? expr)
         (if (eqv? x expr)
             e       ;; [e/x]x = e
             expr    ;; [e/x]y = y
         )
      )
      (else expr)    ;; Error! Just return the expr
    )
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (free-var? x expr) returns true iff x is a free variable in expr
;;  FV[x] = x
;;  FV[(fun x e)] = FV[e]\x
;;  FV[(e1 e2)] = FV[e1] union FV[e2]
;; This is a helper function for static substitution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define free-var? (lambda (x expr)
    (cond
      ((var? expr) 
       (if (eqv? x expr)
           #t   ;; expr=x
           #f   ;; expr not= x
       ))
      ((abs? expr)
       (if (eqv? x (var-of expr))
           #f   ;; x is the variable in functio
           (free-var? x (body-of expr))
       ))
      ((app? expr) (or (free-var? x (rator-of expr)) (free-var? x (rand-of expr))))
      (else #f)   ;;ERROR! Just return false
     )
))                     




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fresh variable generates a fresh variable to be used in substitution
;; I am reserving the string that start with fresh for the purpose of static substitution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define suff 0)
(define fresh-var
  (lambda (expr)
    (set! suff (+ 1 suff))
    (string->symbol (string-append "fresh" (number->string suff)))
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  PART A
;;
;; Substitution 
;; - Implements Static Binding
;;
;; [e/x]y = y  (if x != y)
;; [e/x]x = e
;; [e/x](e1 e2) = ([e/x]e1 [e/x]e2)
;; [e/x](fun y e1) (fun y [e/x]e1)  (if x != y and y \not\in FV[e])
;; [e/x](fun x e1) = (fun z [z/y]e[e/x]e1)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(define subst (lambda (e x expr)   
    (cond 
      ((abs? expr)
         (if (eqv? x (var-of expr))
             expr        ;; [e/x](fun x e1) = (fun x e1)
             (if (free-var? (var-of expr) e)
                 ;;(make-abs (var-of expr) (subst e x (body-of expr)))
                 (let ((z (fresh-var (body-of expr))))       ;;TODO: I need to generate necessarily fresh variable
                 (make-abs z (subst e x (subst z (var-of expr) (body-of expr)))))
                 (make-abs (var-of expr) (subst e x (body-of expr))) ;; [e/x](fun y e1) = (fun y [e/x]e1)
             )     
         )
      )
      ((app? expr)      ;; [e/x](e1 e2) = ([e/x]e1 [e/x]e2)
         (make-app (subst e x (rator-of expr))
                   (subst e x (rand-of expr))
         )
      )
      ((var? expr)
         (if (eqv? x expr)
             e       ;; [e/x]x = e
             expr    ;; [e/x]y = y
         )
      )
      (else expr)    ;; Error! Just return the expr
    )
))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (reducible? expr) return true iff expr is reducible
;;  this is used as a helper for reduce function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define reducible? (lambda (expr)
     (cond
       ((var? expr) #f)
       ((abs? expr) (reducible? (body-of expr)))
       ((app? expr)
        (cond ((abs? (rator-of expr)) #t)
              ((var? (rator-of expr)) (reducible? (rand-of expr)))
              ((app? (rator-of expr)) 
                    (or (reducible? (rator-of expr)) (reducible? (rand-of expr))))
        ))
       (else #f)  ;;Error case
     )
))                     
        
            


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  PART B
;;
;; (reduce expr) returns the normal form of expr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define reduce (lambda (expr)
     (if (reducible? expr)
         (cond
           ((var? expr) expr)
           ((abs? expr) (reduce (make-abs (var-of expr) (reduce (body-of expr)))))   ;; Reduce body of abstraction
           ((app? expr)
            (cond
              ((var? (rator-of expr)) 
               (reduce (make-app (rator-of expr) (reduce (rand-of expr)))))   ;; reduce rand of expr
              ((app? (rator-of expr))
               (if (reducible? (rator-of expr))
                   (reduce (make-app (reduce (rator-of expr)) (rand-of expr)))
                   (reduce (make-app (rator-of expr) (reduce (rand-of expr))))))
              ((abs? (rator-of expr))
               (reduce (subst (rand-of expr) (var-of (rator-of expr)) (body-of (rator-of expr)))))))  ;; abstraction substitution reduction
          )
         expr)
))                 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (flat-prans expr) gets rid of extra paranthesis
;; (add-prans expr) adds parans if necessary to expr
;;  Helper functions for parse-lambda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define flat-prans (lambda (expr) 
    (cond 
      ((symbol? expr) expr) 
      ((and (list? expr) (= (length expr) 1) (list? (car expr)))
       (flat-prans (car expr)))
      ((and (list? expr) (= (length expr) 1) (var? (car expr)))
       (car expr))
      ((abs? expr)
       (make-abs (flat-prans (var-of expr)) (flat-prans (body-of expr))))
      ((app? expr)
       (make-app (flat-prans (rator-of expr)) (flat-prans (rand-of expr))))
      (else (map add-prans expr))
    )
))

(define add-prans (lambda (expr)
     (cond
      ((symbol? expr) expr) 
      ((and (list? expr) (> (length expr) 3) (eqv? (car expr) 'fun))
        (make-abs (cadr expr) (parse-lambda (cdr (cdr expr)))))
      ((and (list? expr) (> (length expr) 2) (not (eqv? (car expr) 'fun))) 
        (parse-lambda (append (list (list (car expr) (parse-lambda (cadr expr)))) (cdr (cdr expr)))))
      (else (map add-prans expr)) 
    )    
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  PART C
;;
;; (parse-lambda expr) returns the expr parenthesized conforming to grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parse-lambda (lambda (expr)                    
    (add-prans (flat-prans expr))   
))  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Combining results
;; (interpreter E) does the following 2 things
;; 1. Parses the expression E to conform to grammar of lambda-calculus
;; 2. Reduces the parsed expression into normal form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (interpret E) (reduce (parse-lambda E)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fibbunacci 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Church numbers 0,1,2,3,4,5,6
(define zero (make-abs 'f (make-abs 'x 'x)))
(define one (make-abs 'f (make-abs 'x (make-app 'f 'x))))
(define two (make-abs 'f (make-abs 'x (make-app 'f (make-app 'f 'x)))))
(define three (make-abs 'f (make-abs 'x (make-app 'f (make-app 'f (make-app 'f 'x))))))
(define four (make-abs 'f (make-abs 'x (make-app 'f (make-app 'f (make-app 'f (make-app 'f 'x)))))))
(define five (make-abs 'f (make-abs 'x (make-app 'f (make-app 'f (make-app 'f (make-app 'f (make-app 'f 'x))))))))
(define six (make-abs 'f (make-abs 'x (make-app 'f (make-app 'f (make-app 'f (make-app 'f (make-app 'f (make-app 'f 'x)))))))))


;; succ and plus functions for church numbers
(define succ (make-abs 'n (make-abs 'f (make-abs 'x (list (list 'n 'f) (list 'f 'x))))))
                                       
(define plus (lambda (a b)
  (make-app (make-abs 'm (make-app (make-abs 'n (make-abs 'f (make-abs 'x (list (list 'm 'f) (list (make-app 'n 'f) 'x))))) b)) a)))

;; lambda calculus booleans
(define true (make-abs 'a (make-abs 'b 'a)))
(define false (make-abs 'a (make-abs 'b 'b)))

;; lambda calculus pair
(define make-pair (lambda (s h t)
     (make-abs s (make-app (make-app s h) t))))             

;; Helper function for fib
;; Given a pair (a,b) outputs (b,a+b)
(define f (make-abs 'p (make-pair 's (make-app 'p false) (plus (make-app 'p true) (make-app 'p false)))))

;; fib Function
(define fib (lambda (church-number)
   (make-app (make-app (make-abs 'n (make-app (make-app 'n f) (make-pair 's  zero one))) church-number) true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing Fib function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display (interpret (fib zero)))
(newline)
(display (interpret (fib one)))
(newline)
(display (interpret (fib two)))
(newline)
(display (interpret (fib three)))
(newline)
(display (interpret (fib four)))
(newline)
(display (interpret (fib five)))
(newline)
(display (interpret (fib six)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My own testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

