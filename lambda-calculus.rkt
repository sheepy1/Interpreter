#lang racket
;;一个call-by-value (CBV)的Scheme解释器

;;空环境
(define env0 '())

;;对环境env进行扩展，加入（x . v）组成新的association list(键值对链表)
(define ext-env
  (lambda (x v env)
    (cons `(,x . ,v) env)))

;;取值。在环境env中查找x的值
(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond [(not p) x]
            [else (cdr p)]))))

;;闭包的数据结构
(struct Closure (f env))

;;解释器的递归定义（接受表达式exp和环境env）
;;共5种情况（变量，函数，调用，数字，算术表达式）
(define interpret
  (lambda (exp env)
    (match exp
      [(? symbol? x) (lookup x env)]                             ;;变量        
      [(? number? x) x]                                          ;;数字
      [`(lambda (,x) ,e)                                         ;;函数
       (Closure exp env)]
      [`(,e1 ,e2)                                                ;;调用
       (let ([v1 (interpret e1 env)]
             [v2 (interpret e2 env)])
         (match v1
           [(Closure `(lambda (,x) ,e) env1)                
            (interpret e (ext-env x v2 env1))]))]
      [`(,op ,e1 ,e2)
       (let ([v1 (interpret e1 env)]
             [v2 (interpret e2 env)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))

;;解释器的调用接口。封装interpret，把环境参数初始值设为env0
(define interp
  (lambda (exp)
    (interpret exp env0)))


;;简单调用示例
(interp '(+ 3 4))
;;>7

(interp '(* (- (+ 3 4) (/ 8 4)) 6))
;;>30

(interp '((lambda (x) (* 8 x)) 9))
;;>72

;;解释器只能接受一个参数，要接收两个参数时使用嵌套的函数
(interp '(((lambda (x) (lambda (y) (* x y))) 7) 3))
;;>21



(interp '(1 9))
;;> match: no matching clause for 1