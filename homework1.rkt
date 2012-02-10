#lang racket

(require data/heap)

;globals
  (define GOAL
    (list 1 2 3 4 5 6 7 8 0))
  (define COORDINATES
    (list
     (list 0 0) (list 0 1) (list 0 2)
     (list 1 0) (list 1 1) (list 1 2)
     (list 2 0) (list 2 1) (list 2 2)))
  
;coordinates
  (define (index->coordinates index)
    (list-ref COORDINATES index))
  (define (coordinates->index coordinates)
    (+ (* 3 (first coordinates)) (second coordinates)))
  (define (index->number state index)
    (list-ref state index))
  (define (coordinates->number state coordinates)
    (index->number state (coordinates->index coordinates)))
  
  (define (number->index state number)
    (- (length state) (length (memv number state))))
  (define (number->coordinates state number)
    (index->coordinates (number->index state number)))
  
  (define (valid-coordinates? coordinates-and-action)
    (list? (member (first coordinates-and-action) COORDINATES)))
  
  (define (coordinates-for-valid-swap-with coordinates)
    (filter valid-coordinates?
            (list
             (list (list (- (first coordinates) 1) (second coordinates)) 'up)
             (list (list (+ (first coordinates) 1) (second coordinates)) 'down)
             (list (list (first coordinates) (- (second coordinates) 1)) 'left)
             (list (list (first coordinates) (+ (second coordinates) 1)) 'right))))
  
  (define (calculate-manhattan-distance key x y)
    (+ (abs (- (first (list-ref COORDINATES (- key 1))) x))
       (abs (- (second (list-ref COORDINATES (- key 1))) y))))
  
;vertex 
  (struct vertex (state action previous g h))
  
  (define (make-vertex state-and-action previous-vertex)
    (vertex (first state-and-action)
            (second state-and-action)
            previous-vertex
            (if (eq? 'none previous-vertex)
                0
                (+ 1 (vertex-g previous-vertex)))
            (calculate-h (first state-and-action))))
  
  (define (vertex-<= v1 v2)
    (<= (vertex-f v1) (vertex-f v2)))
  
  (define (vertex-f v)
    (+ (vertex-g v) (vertex-h v)))
  
  (define (print-solution last-vertex)
    (printf "Solved in ~a moves\n" (vertex-g last-vertex))
    (print-vertex last-vertex))
  
  (define (print-vertex vertex)
    (cond
      [(not (eq? 'none vertex))
       (print-vertex (vertex-previous vertex))
       (printf "\t move ~a to get ~a\n" (vertex-action vertex) (vertex-state vertex))]))
  
  (define (vertex-parent-state vertex)
    (if (vertex? (vertex-previous vertex))
        (vertex-state (vertex-previous vertex))
        null))
  
 ;states
  (define (calculate-h state)
    (apply + (flatten
               (map (lambda (i)
                      (map (lambda (j)
                             (if (eqv? 0 (list-ref state (coordinates->index (list i j)))) ;fix
                                 0
                                 (calculate-manhattan-distance (list-ref state (coordinates->index (list i j))) i j)))
                           (list 0 1 2)))
                    (list 0 1 2)))))
  
  (define (obtainable-states state)
    (map (lambda (target-and-action)
           (list
            (swap-tile state 0 (coordinates->number state (first target-and-action)))
            (second target-and-action)))
         (coordinates-for-valid-swap-with (number->coordinates state 0))))
  
  (define (goal? state)
    (equal? GOAL state))
  
  (define (swap-tile state source target)
    (map (lambda (number)
           (cond
             [(= number source) target]
             [(= number target) source]
             [else number]))
         state))
    
  
;a* graph search algorithm
  (define (EXPAND vertex)
    (map
     (lambda (next-state-and-action) (make-vertex next-state-and-action vertex))
     (remove
      (vertex-parent-state vertex)
      (obtainable-states (vertex-state vertex))
      (lambda (grandparent-state state-and-action) (equal? grandparent-state (first state-and-action))) )))
  
  (define (SOLVE puzzle)
    ;normalize puzzle to our concept
    (set! puzzle (flatten puzzle))
    
    ;setup OPEN heap and CLOSED hash table
    (define OPEN (make-heap (lambda (v1 v2) (vertex-<= v1 v2))))
    (define CLOSED (make-immutable-hash))
  
    ;insert initial state into OPEN
    (heap-add! OPEN (make-vertex (list puzzle 'initial) 'none))
  
    (define (loop)
      (cond
        [(empty? OPEN) (error "fail")]
        [else
         (define x (heap-min OPEN))
         (heap-remove-min! OPEN)
         
         (cond
           ((goal? (vertex-state x)) x)
           (else
            (hash-set CLOSED (vertex-state x) x)
            (for-each
             (lambda (y)
               (hash-ref CLOSED (vertex-state y) (lambda () (heap-add! OPEN y))))
             (EXPAND x))
            (loop)))]))
    (print-solution (loop)))

;5. test examples
  (printf "5. Test Examples \n")
  
  (SOLVE (list 6 4 2 1 5 3 7 0 8))
  (SOLVE (list 6 4 2 8 5 3 1 0 7))
  (SOLVE (list 6 4 7 8 5 0 3 2 1))
  (SOLVE (list 8 0 7 6 5 4 3 2 1))
