#lang racket
(define (delete x ls)
  (if (null? ls) '()
      (if (eq? x (car ls)) (cdr ls)
          (cons (car ls)(delete x (cdr ls))))))

(define (delete2 x ls)
  (if (null? ls) '()
      (if (eq? x (car ls))
          (delete2 x (cdr ls))
          (cons (car ls) (delete2 x (cdr ls))))))

(define (square x) (* x x))

(define (count x ls)
  (if (null? ls) 0
      (if (eq? x (car ls))
          (+ 1 (count x (cdr ls)))
          (count x (cdr ls)))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1 ) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (append ls1 ls2)
  (cond ((null? ls1) ls2)
        (else
         (cons (car ls1)
               (append(cdr ls1) ls2)))))

(define (last ls)
  (cond ((null? (cdr ls)) (car ls))
       (else (last (cdr ls)))))

(define (filter l pred)
  (if (null? l) '()
      (if (pred (car l))
          (cons (car l) (filter (cdr l) pred))
          (filter (cdr l) pred))))