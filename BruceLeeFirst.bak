
;; Scheme Example - The farmer, wolf, goat, cabbage problem.

(require racket/trace) ;; load trace function

;; this function does a breadth-first search
(define fwgc
  (λ (init goal)
    (do ( (path-list (list (list init))) ) ;; initialize the vars, we only have path-list, which is the Q
      
        ((or (null? path-list) (equal? goal (caar path-list))) ;; terminal condition
         (if (null? path-list) '() (reverse (car path-list)))) ;; return either empty or reverse of 1st path
      
        (set! path-list (extend path-list))) ;; loop body
                                             ;; path-list = extend(path-list);
   )
)

(define bfs fwgc)
;; check membership in a list, including sub-list of arbitary levels
;; (in 4 '(5 ((4 6)))
(define in 
  (lambda (a l)
    (cond ((null? l) #f)
          ((equal? a (car l)) #t)
          ((list? (car l)) 
           (or (in a (car l))(in a (cdr l))))
          (else (in a (cdr l))))))

(define (opposite side)
   (if (equal? side 'e) 'w 'e))

;; the following 4 are legal OPs 
(define farmer-take-self
  (lambda (L)
     (cons (opposite (car L)) (cdr L))))

(define farmer-take-wolf
  (lambda (L)
    (if (equal? (car L) (cadr L))
        (append (list (opposite (car L)) (opposite (cadr L))) (cddr L))
        L)))

(define farmer-take-goat
  (lambda (L)
    (if (equal? (car L) (caddr L))
        (list (opposite (car L)) (cadr L) (opposite (caddr L)) (cadddr L))
        L)))

(define farmer-take-cabbage
  (lambda (L)
    (if (equal? (car L) (cadddr L))
        (list (opposite (car L)) (cadr L) (caddr L) (opposite (cadddr L)))
        L)))

;; make a list of OPs so we could call them in order without having to spell them out. 
;; Can we do that in C++?
(define op-list 
  '(farmer-take-self farmer-take-wolf farmer-take-goat farmer-take-cabbage))
;; you can change this list to other op-list to bfs of different puzzles
;; (define op-list '(up down left right)

(define extend 
   (lambda (lst) ;; lst is the local name for path-list in fwgc
     (do ((ops op-list (cdr ops)) ;; local name for the op-list, allowing us to walk through the op-list
          (cur-node (caar lst)) ;; cur-node contains the list describing the current state such as (w w w w)
          (new-node '())) ;; initialize
       
       ((null? ops) (cdr lst))         ;; terminal condition, remove cur-node when return
       
       (set! new-node ((eval (car ops)) cur-node)) ;; this calls the function on cur-node
       (if (and (not (in new-node lst)) (not (unsafe new-node)))
           (set! lst (append lst (list (cons new-node (car lst)))))) ; adding a new path at the end of lst
     )
   )
)
;; farmerL = (car L)
;; wolfLocation = (cadr L)
;; goatL = (caddr L)
;; cabL = (cadddr L)
(define unsafe
  (lambda (L)
    (or (and (equal? (opposite (car L)) (cadr L)) (equal? (cadr L)(caddr L)))
       (and (equal? (opposite (car L)) (caddr L)) (equal? (caddr L)(cadddr L))))))

;; L = (n1 n2 n3 n4 n5 n6 d)
(define cross
  (lambda (L)
    (list (car L) (cadr L) (caddr L) (cadddr L) (caddr (cddr L))
          (caddr (cdddr L)) (opposite (cadddr (cdddr L))))))

(define across
  (lambda (L) ;; L = (2 2 1 1 0 0 e)
    (reverse (cons (opposite (car ( reverse L))) (cdr (reverse L))))))
;;                this part change the last ele to opp


        
