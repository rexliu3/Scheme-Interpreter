(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement

(define (zip pairs)
  'replace-this-line)


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
        ((or (null? list1) (null? list2)) nil)
        ((comp (car list1) (car list2)) (cons (car list1) (merge comp (cdr list1) list2)))
        (else (cons (car list2) (merge comp list1 (cdr list2))))
        )

  )
  ; END PROBLEM 16


(merge < '(1 5 7 9) '(4 8 10))
; expect (1 4 5 7 8 9 10)
(merge > '(9 7 5 1) '(10 8 4 3))
; expect (10 9 8 7 5 4 3 1)

;; Problem 17

(define (nondecreaselist1 s)
  (define (helper next s)
    ((cond
       ((null? (cdr s))
        (list s)
       )
       ((> (car (cdr s)) (car s))
        (append (car s) (car (cdr s)))
       )
       (()
       )
     )
    )
  )
  (helper (car s) s)
)

(define (nondecreaselist2 s)
  (cond
    ((null? s) nil)
    ((<= (car s) (cadr s))
     (cons (car s) (nondecreaselist (cdr s)))
    )
    ((> (car s) (cadr s))
     (list (car s) (cons (cadr s) (nondecreaselist (cddr s))))
    )
  )
)

(define (nondecreaselist s)

    ; return first section
  (define (helper1 s)
        (cond
              ((null? s) nil)
              ((null? (cdr s)) (car s))
              ((> (car s) (cadr s)) (car s))
              (else (cons (car s) (cons (helper1 (cdr s)) nil)))
              )
      )

  ; return list without the first section
  (define (helper3 s)
          (cond
              ((null? s) nil)
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

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM EC
         'replace-this-line
         ; END PROBLEM EC
         )
        ((quoted? expr)
         ; BEGIN PROBLEM EC
         'replace-this-line
         ; END PROBLEM EC
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM EC
           'replace-this-line
           ; END PROBLEM EC
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM EC
           'replace-this-line
           ; END PROBLEM EC
           ))
        (else
         ; BEGIN PROBLEM EC
         'replace-this-line
         ; END PROBLEM EC
         )))

