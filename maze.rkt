#lang racket/gui

(struct maze (rooms corridors))

(define (add-room mz room)
  (maze (set-add (maze-rooms mz) room) (maze-corridors mz)))
(define (add-corridor/unsafe mz corridor)
  (maze (maze-rooms mz) (set-add (maze-corridors mz) corridor)))
(define (add-corridors/unsafe mz corridors)
  (maze (maze-rooms mz) (set-union (maze-corridors mz) corridors)))

(define (north-of room)
  (if (> (cdr room) 0) (cons (car room) (sub1 (cdr room))) room))
(define (west-of room)
  (if (> (car room) 0) (cons (sub1 (car room)) (cdr room)) room))
(define (south-of room)
  (if (< (cdr room) 9) (cons (car room) (add1 (cdr room))) room))
(define (east-of room)
  (if (< (car room) 9) (cons (add1 (car room)) (cdr room)) room))

(define (draw-room dc room)
  (let ([x (* (car room) 50)]
        [y (* (cdr room) 50)])
    (send dc set-brush "white" 'transparent)
    (send dc set-pen "gray" 1 'short-dash)
    (send dc draw-rectangle x y 50 50)))
(define (draw-corridor dc corridor)
  (let* ([cor (set->list corridor)]
         [x1 (+ (* (car (first cor)) 50) 25)]
         [y1 (+ (* (cdr (first cor)) 50) 25)]
         [x2 (+ (* (car (second cor)) 50) 25)]
         [y2 (+ (* (cdr (second cor)) 50) 25)])
    (send dc set-pen "white" 25 'solid)
    (send dc draw-line x1 y1 x2 y2)))
(define (draw-maze dc maze)
  (set-for-each (maze-rooms maze) (curry draw-room dc))
  (set-for-each (maze-corridors maze) (curry draw-corridor dc)))

(define (random-from lst)
  (list-ref lst (random (length lst))))
(define (get-corridors/room maze room)
  (let ([north-room (north-of room)]
        [west-room (west-of room)]
        [south-room (south-of room)]
        [east-room (east-of room)])
    (define result (set))
    (unless (set-member? (maze-rooms maze) north-room) (set! result (set-add result (set room north-room))))
    (unless (set-member? (maze-rooms maze) west-room) (set! result (set-add result (set room west-room))))
    (unless (set-member? (maze-rooms maze) south-room) (set! result (set-add result (set room south-room))))
    (unless (set-member? (maze-rooms maze) east-room) (set! result (set-add result (set room east-room))))
    result))
(define (get-corridors maze)
  (foldr (lambda (room x)
           (set-union (get-corridors/room maze room) x)) (set) (set->list (maze-rooms maze))))
(define (find-maze-by-room mazes room)
  (findf (lambda (maze)
           (set-member? (maze-rooms maze) room)) (set->list mazes)))
(define (combine-mazes maze1 maze2 cor)
  (maze (set-union (maze-rooms maze1) (maze-rooms maze2))
        (set-add (set-union (maze-corridors maze1) (maze-corridors maze2)) cor)))

(define (get-step mazes)
  (let* ([random-maze (random-from (set->list mazes))]
         [random-corridor (random-from (set->list (get-corridors random-maze)))]
         [rc-list (set->list random-corridor)]
         [maze1 (find-maze-by-room mazes (first rc-list))]
         [maze2 (find-maze-by-room mazes (second rc-list))])
    (set-add (set-remove (set-remove mazes maze1) maze2) (combine-mazes maze1 maze2 random-corridor))))

;MAZES
(define MAZES (for*/set ([i '(0 1 2 3 4 5 6 7 8 9)]
                         [j '(0 1 2 3 4 5 6 7 8 9)])
                        (maze (set (cons i j)) (set))))
(for ([i (build-list 99 values)])
  (set! MAZES (get-step MAZES)))
;(display MAZES)

;GUI
(define frame (new frame%
                   [label "MAZE"]
                   [min-width 500]
                   [min-height 500]
                   [stretchable-width #f]
                   [stretchable-height #f]))
(define canvas (new canvas%
                    [parent frame]
                    [paint-callback	(lambda (canvas dc)
                                          (set-for-each MAZES (curry draw-maze dc)))]))
(send canvas set-canvas-background (make-object color% "darkred"))
(send frame show #t)