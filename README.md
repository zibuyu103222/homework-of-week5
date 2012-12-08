homework-of-week5
=================

SICP code
;problem 1
(define (bitfunc x) (- (+(expt x 4) (expt x 2)) 14))

;problem 2
(define (bitfunc-rect x1 x2)
  (* (abs(bitfunc x1)) (- x2 x1)))

;problem 3
(define (len a b c) (/ (- c b) a))

(define (helper-recur d1 d2 d3)
(if (= d1 1)
    (bitfunc-rect d2 (+ d2 d3) )
    (+ (bitfunc-rect d2 (+ d2 d3) ) (helper-recur (- d1 1) (+ d2 d3) d3))))

(define (helper-iter e1 e2 e3 e4)
  (define n (bitfunc-rect e2 (+ e2 e3)))
(if (= e1 1)
    e4
 (helper-iter (- e1 1) (+ e2 e3) e3 (+ n e4))))

(define (bitfunc-integral-recur num-steps x1 x2)
  (define m (len num-steps x1 x2))
  (helper-recur num-steps x1 m))
 
(define (bitfunc-integral-iter num-steps x1 x2)
(define m (len num-steps x1 x2))
  (helper-iter num-steps x1 m 0))

;problem 4
(define (bitfunc-rect x1 x2 func)
  (* (abs(func x1)) (- x2 x1)))

(define (len a b c) (/ (- c b) a))

(define (helper-recur d1 d2 d3 func)
(if (= d1 1)
    (bitfunc-rect d2 (+ d2 d3) func)
    (+ (bitfunc-rect d2 (+ d2 d3) func) (helper-recur (- d1 1) (+ d2 d3) d3 func))))

(define (integral func num-steps x1 x2)
  (bitfunc-rect x1 x2 func)
(define m (len num-steps x1 x2))
  (helper-recur num-steps x1 m func))

;problem 5
(define (pi-func x) (sqrt (- 1 (* x x))))
(define (approx-pi num-steps)
(* 4 (integral pi-func num-steps 0 1)))

;problem 6
(define (rectangle func x1 x2)
(* (abs(func x1)) (- x2 x1)))

(define (trapezoid func x1 x2)
(/ (* (+ (abs(func x1)) (abs(func x2))) (- x2 x1)) 2) )

(define (len a b c) (/ (- c b) a))

(define (helper-recur d1 d2 d3 func piece)
(if (= d1 1)
    (piece func d2 (+ d2 d3))
    (+ (piece func d2 (+ d2 d3)) (helper-recur (- d1 1) (+ d2 d3) d3 func piece))))

(define (integral func num-steps x1 x2 piece)
  (piece func x1 x2)
(define m (len num-steps x1 x2))
  (helper-recur num-steps x1 m func piece))

(define (integral-with piece func num-steps x1 x2)
(integral func num-steps x1 x2 piece))

;problem 7
(define (pi-func x) (sqrt (- 1 (* x x))))
(define (better-pi num-steps)
(* 4 (integral-with rectangle pi-func num-steps 0 1)))

;problem 8
(define (deriv-variable var wrt)
(if(equal? var wrt)
  1
  0))

;problem 9
(define (deriv-constant constant wrt)
0)

;(define (derivative expr wrt)
;(cond ((symbol? expr) (deriv-constant expr wrt))
;(else (error "Don't know how to differentiate" expr))))

(define (derivative expr wrt)
  (if (symbol? expr)
      (deriv-variable expr wrt)
      (deriv-constant expr wrt)))

;problem 10
(define (deriv-sum expr wrt)
  (define a (car expr))
 (define d (derivative (car (cdr expr)) wrt))
  (define e (derivative (cdr (cdr expr)) wrt))
  (define (out a1 a2 a3) (list a1 a2 a3))
  (out a d e)  
)

;problem 11
(define (deriv-product expr wrt)
(define a (car expr))
 (define d (derivative (car (cdr expr)) wrt))
  (define e (derivative (cdr (cdr expr)) wrt))
  (define f (cdr (cdr expr)))
  (define (out a1 a2 a3 a4) (list '+ (list a1 (car (cdr expr)) a3) (list a1 a2 a4)))
  (out a d e f)  
  )