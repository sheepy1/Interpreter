#lang racket

;;一个Scheme计算器
(define calc
  (lambda (exp)
    (match exp
      [(? number? x) x]
      [`(,op ,e1 ,e2)
       (let ([v1 (calc e1)]
             [v2 (calc e2)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))


;;调用
(calc 2)
(calc '(+ 3 4))
(calc '(* 3 (- 5 2)))