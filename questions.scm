(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement

(define (zip pairs)
  ; zipper is a one-arguement function that takes in a list and returns first element
  (define zipper (lambda (listy) (car listy)))
  ; zipper 2 is a one-arguement function that takes in a list and returns without first element
  (define zipper2 (lambda (listy) (cdr listy)))
  ; helper recurses: map(zipper) and then map(zipper2) until empty
  (define (helper pairs)
    (cond 
      ((null? pairs)
       nil
      )
      ((null? (car pairs))
       nil
      )
      (else
       (cons (map zipper pairs)
             (helper (map zipper2 pairs))
       )
      )
    )
  )
  (helper pairs)
)


;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
  (define (helper listy index)
    (cond
      ((null? s)
       nil
      )
      ((null? (cdr listy))
       (list (list index (car listy)))
      )
      (else
       (cons (list index (car listy))
             (helper (cdr listy) (+ index 1))
       )
      )
    )
  )
  (helper s 0)
)
  ; END PROBLEM 15

;; Problem 16

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
  (cond
        ((and (null? list1) (null? list2)) nil)
        ((null? list1) (cons (car list2) (merge comp list1 (cdr list2))))
        ((null? list2) (cons (car list1) (merge comp (cdr list1) list2)))
        ((comp (car list1) (car list2)) (cons (car list1) (merge comp (cdr list1) list2)))
        (else (cons (car list2) (merge comp list1 (cdr list2))))
        )

  )


(merge < '(1 5 7 9) '(4 8 10))
; expect (1 4 5 7 8 9 10)
(merge > '(9 7 5 1) '(10 8 4 3))
; expect (10 9 8 7 5 4 3 1)

;; Problem 17

(define (nondecreaselist s)
    ; return first section
  (define (helper1 s)
        (cond
              ((null? s) nil)
              ((null? (cdr s)) (cons(car s) nil))
              ((> (car s) (cadr s)) (cons (car s) nil))
              (else (cons (car s) (helper1 (cdr s))))
              )
      )

  ; return list without the first section
  (define (helper3 s)
          (cond
              ((null? s) nil)
              ((null? (cdr s)) nil)
              ((null? (cdr s)) s)
              ((> (car s) (cadr s)) (cdr s))
              (else (helper3 (cdr s)))
              )
      )

  ; recursing the list
  (define (helper2 s)
      (cond
              ((null? s) nil)
              (else (cons (helper1 s) (helper2 (helper3 s))))
              )
      )
  (helper2 s)
)


(nondecreaselist '(1 2 3 4 1 2 3 4 1 1 1 2 1 1 0 4 3 2 1))

;; Problem EC
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

; ; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond 
    ((atom? expr)
     ; BEGIN PROBLEM EC
     (expr)
     ; END PROBLEM EC
    )
    ((quoted? expr)
     ; BEGIN PROBLEM EC
     (list '(car expr)
           (let-to-lambda (cadr expr))
           (let-to-lambda (car (cddr expr)))
     )
     ; END PROBLEM EC
    )
    ((or (lambda? expr) (define? expr))
     ; (lambda (listy) (car listy))
     (let ((form (car expr))
           (params (cadr expr))
           (body (cddr expr))
          )
       ; BEGIN PROBLEM EC
       '(form (params) body)
       ; END PROBLEM EC
     )
    )
    ((let? expr)
     (let ((values (cadr expr))
           (body (cddr expr))
          )
       ; BEGIN PROBLEM EC
       ((lambda (car (zip values))
           (let-to-lambda body)
           (cadr (zip values))
         )
        )
       ; END PROBLEM EC
     )
    )
    (else
     ; BEGIN PROBLEM EC
     ((map let-to-lambda expr))
     ; END PROBLEM EC
    )
  )
)

;(let-to-lambda '(let((a 1) (b 2))(+ a b)))

;(let-to-lambda '(let((a 1))(let((b a))b)))