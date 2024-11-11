(load "load.scm")

(define var=? var-eq?)

(define (var-member? x l)
  (and (pair? l)
       (or (var=? x (car l))
           (var-member? x (cdr l)))))
           
(define (union l1 l2)
  (if (null? l1) l2
    (let ((rec (union (cdr l1) l2)))
      (if (var-member? (car l1) rec) rec
          (cons (car l1) rec)))))

(define (term-variables t)
  (cond
    ((pair? t) (union
                 (term-variables (car t))
                 (term-variables (cdr t))))
    ((var? t) (list t))
    (else '())))
    
(define (term-variableso t v)
  (project (t)
    (== v (term-variables t))))

;; An important one-to-one relation
(define (zipo l1 l2 zip)
  (conde
    ((== l1 '()) (== l2 '()) (== zip '()))
    ((fresh (a1 a2 d1 d2 rec)
       (== l1 `(,a1 . ,d1))
       (== l2 `(,a2 . ,d2))
       (== zip `((,a1 . ,a2) . ,rec))
       (zipo d1 d2 rec)))))

;; Create a new term with the same structure, but
;; replace each variable with its associated variable
;; defined in alist `a`.
(define (copy-term t a)
  (cond
    ((pair? t) (cons
                 (copy-term (car t) a)
                 (copy-term (cdr t) a)))
    ((var? t) (cdr (assp (lambda (t^) (var=? t t^)) a)))
    (else t)))

(define (copy-termo t1 t2)
  (fresh (v1 v2 a)
    (term-variableso t1 v1)
    (zipo v1 v2 a)
    (project (t1 a)
      (== t2 (copy-term t1 a)))))

(test "copy-termo/v1"
      (run* (a b)
            (fresh (x y z)
              (== a `(,x ,x ,y ,z))
              (copy-termo a b)))
      '(((_.0 _.0 _.1 _.2) (_.3 _.3 _.4 _.5))))
  
(test "copy-termo/v2"
      (letrec ((var-member? (lambda (x l)
                              (and (pair? l)
                                   (or (var=? x (car l))
                                       (var-member? x (cdr l)))))))
        
        (letrec ((union (lambda (l1 l2)
                          (if (null? l1) l2
                              (let ((rec (union (cdr l1) l2)))
                                (if (var-member? (car l1) rec) rec
                                    (cons (car l1) rec)))))))
          
          (letrec ((term-variables (lambda (t)
                                     (cond
                                       ((pair? t) (union
                                                   (term-variables (car t))
                                                   (term-variables (cdr t))))
                                       ((var? t) (list t))
                                       (else '())))))
            
            (letrec ((copy-term (lambda (t a)
                                  (cond
                                    ((pair? t) (cons
                                                (copy-term (car t) a)
                                                (copy-term (cdr t) a)))
                                    ((var? t) (cdr (assp (lambda (t^) (var=? t t^)) a)))
                                    (else t)))))
              
              (letrec ((term-variableso (lambda (t v)
                                          (project (t)
                                            (== v (term-variables t))))))
                
                (letrec ((zipo (lambda (l1 l2 zip)
                                 (conde
                                  ((== l1 '()) (== l2 '()) (== zip '()))
                                  ((fresh (a1 a2 d1 d2 rec)
                                     (== l1 `(,a1 . ,d1))
                                     (== l2 `(,a2 . ,d2))
                                     (== zip `((,a1 . ,a2) . ,rec))
                                     (zipo d1 d2 rec)))))))
                  
                  (letrec ((copy-termo (lambda (t1 t2)
                                         (fresh (v1 v2 a)
                                           (term-variableso t1 v1)
                                           (zipo v1 v2 a)
                                           (project (t1 a)
                                             (== t2 (copy-term t1 a)))))))
                  
                    (run* (a b)
                          (fresh (x y z)
                            (== a `(,x ,x ,y ,z))
                            (copy-termo a b))))))))))
      '(((_.0 _.0 _.1 _.2) (_.3 _.3 _.4 _.5))))


(test "copy-termo/midori"
      (run 1 (q)
           (evalo
            `(letrec ((var-member? (lambda (x l)
                                     (and (pair? l)
                                          (or (var=? x (car l))
                                              (var-member? x (cdr l)))))))
        
               (letrec ((union (lambda (l1 l2)
                                 (if (null? l1) l2
                                     (let ((rec (union (cdr l1) l2)))
                                       (if (var-member? (car l1) rec) rec
                                           (cons (car l1) rec)))))))
          
                 (letrec ((term-variables (lambda (t)
                                            (cond
                                              ((pair? t) (union
                                                          (term-variables (car t))
                                                          (term-variables (cdr t))))
                                              ((var? t) (list t))
                                              (else '())))))
            
                   (letrec ((copy-term (lambda (t a)
                                         (cond
                                           ((pair? t) (cons
                                                       (copy-term (car t) a)
                                                       (copy-term (cdr t) a)))
                                           ((var? t) (cdr (assp (lambda (t^) (var=? t t^)) a)))
                                           (else t)))))
              
                     (letrec ((term-variableso (lambda (t v)
                                                 (project (t)
                                                   (== v (term-variables t))))))
                
                       (letrec ((zipo (lambda (l1 l2 zip)
                                        (conde
                                         ((== l1 '()) (== l2 '()) (== zip '()))
                                         ((fresh (a1 a2 d1 d2 rec)
                                            (== l1 `(,a1 . ,d1))
                                            (== l2 `(,a2 . ,d2))
                                            (== zip `((,a1 . ,a2) . ,rec))
                                            (zipo d1 d2 rec)))))))
                  
                         (letrec ((copy-termo (lambda (t1 t2)
                                                (fresh (v1 v2 a)
                                                  (term-variableso t1 v1)
                                                  (zipo v1 v2 a)
                                                  (project (t1 a)
                                                    (== t2 (copy-term t1 a)))))))
                  
                           (run* (a b)
                                 (fresh (x y z)
                                   (== a `(,x ,x ,y ,z))
                                   (copy-termo a b))))))))))
            q))
            '(((_.0 _.0 _.1 _.2) (_.3 _.3 _.4 _.5))))
