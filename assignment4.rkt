#lang racket

(define (hello name)
  (print "Hello, "
  (print name))




;Recursion: create the base case, avoid infinite loops by traveling to base case

(define mylis '(1 2 3 4 5))
(define evenlis '(1 2 3 4 5 6))
(define ok '(1 (2 3) 6 5 4 (7 8)))




(define (sum lis)
  (if (empty? lis)  ; if (base case: empty list)
      0             ; then: return the sum

      (+ (car lis) (sum (cdr lis))))) ; else: add head (car) to the sum of the tail (cdr = chop everything left in list) 


;no error checking
(define (mean lis)
  (/(sum lis) (length lis))) ; call sum, divide by length


;find index in list - zero-based
(define (elementAt lis i)
  (if (= i 0)
      (car lis)
      ;else
      (elementAt (cdr lis) (- i 1))))
  


;sorting algorithem - usage: (sort <list | deep list> lensort)
(define (lensort a b)
  (< length a) (length b))
      


;meadian = middle of sorted list
(define (median lis)
(let* ([sortedlis (sort lis <)] [len (length lis)] [middle (floor (/ len 2))]) ; let* creates a pointer, allows recursive definitions?
    (if (even? len)
        (/ (+ (elementAt sortedlis middle) (elementAt sortedlis (- middle 1))))
    ; else
        (elementAt sortedlis middle))))




(define (countElement lis elem)
  (cond ((empty? lis)
             0)
        ((= (car lis)) elem)
             (+ 1 (countElement (cdr lis) elem))
  (else (countElement (cdr lis) elem))))


(define (countAll lis1 lis2)
  (if (empty? lis1)
      '()
  ;else
      (append (list (list (car list) (countElement lis2 (car lis1))))
              (countAll (cdr lis1) lis2))))


(define (findMaxMode counts)
  (if (empty? counts)
      '()
  ;else
      (let ([headcount (car counts)]
            [tailmax (findMaxMode (cdr counts))])
        (cond ((empty? tailmax)
                     headcount)
              ((> (cdr headcount) (cadr tailmax)) headcount)
              (else tailmax))
        )
))

(define (findMaxDeep lis)
  (if (empty? lis)
      '()
  ;else
      (let ([head (car lis)] [tailmax (findMaxDeep (cdr lis))])
        (cond ((empty? tailmax)head)
              ((cond ((= (countElement tailmax) 1)
                      ((cond ((> (cdr head) (cadr tailmax)) head)))
                     )
              (else (findMaxDeep tailmax))))))))

