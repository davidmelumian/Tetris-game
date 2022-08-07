;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tetris) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "itunes.rkt" "teachpack" "2htdp")) #f)))
(define-struct block [x y])
;A Block is a structure
;(make-block Number Number)
;A  Number is a Number in the range [1-9]
;interpretation,block x and y position on the coordinate.
(define b0 (make-block 1 1))
(define b1 (make-block 5 9))
(define b2 (make-block 6 9))

;A Landscape is one of:
; - '()
; - (cons Block LandScape)
;interpretation, the list of landed blocks.
(define l0 '())
(define l1 (list b1 b2))

(define-struct tetris [block landscape])
;A Tetris is a Structure
;(make-tetris Block Landscape)
;interpretation, a black that drops,and a list of landed blocks.
(define t0 (make-tetris b0 '()))
(define t1 (make-tetris b0 (list b1)))
(define t2 (make-tetris b0 l1))
; interpretations
; (make-block x y) depicts a block whose left 
; corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting


;...Physical Constants:...
(define RADIUS 5 )
(define BASE-UNIT  (* RADIUS 2))
(define WIDTH (* RADIUS 20))
(define HEIGHT WIDTH)

;...:Graphical Constants:....
(define BLOCK
(overlay
 (square (- BASE-UNIT 1) "solid" "red")
 (square BASE-UNIT "outline" "black")))
(define MTS (empty-scene WIDTH HEIGHT "midnightblue"))
(define TEXT-GAME-OVER "GAME OVER!")
(define TEXT-COLOR "red")
(define TEXT-FONT 10)

;Block Image -> Block
;renders the block,multiplied by 10 px
(define (block-render b img)
(place-image BLOCK (+ (* (block-x b) BASE-UNIT) RADIUS) (+ RADIUS (* (block-y b) BASE-UNIT)) img))

;LandScape Image -> Image
;renders the list  of landed block
(define (landscape-render l img)
(cond
[(empty? l) img]
[(cons? l) (block-render (first l) (landscape-render (rest l) img))]))
(check-expect (landscape-render '() MTS) MTS)


;Tetris-> Image
;a main function for rendering blocks.
(define (tetris-render t)
(block-render (tetris-block t)(landscape-render (tetris-landscape t) MTS)))


;Tetris->Boolean
;checks whether a block is landed or a block landed on block.
(define (block-landed? t)
(or (= (block-y (tetris-block t)) 9)
    (my-member? (move-block(tetris-block t)) (tetris-landscape t))))
(check-expect (block-landed? (make-tetris (make-block 1 1) '())) #f)
(check-expect (block-landed? (make-tetris (make-block 5 9) '())) #t)
(check-expect (block-landed? (make-tetris (make-block 5 5)
(list (make-block 4 5) (make-block 5 5)))) #f)
 
;Any List of Any -> Boolean
;determines whether the first argument is in the list.
(define (my-member? a-value list)
(cond
[(empty? list) #f]
[(cons? list) (if (equal? a-value (first list))
                  #t
                  (my-member? a-value (rest list)))]))
(check-expect (my-member? "a" '()) #f)
(check-expect (my-member?  1 (list 5 4 3 2 1)) #t)

;Block -> Block
;moves the block  1 unit down.
(define (move-block b)
(make-block (block-x b) (+ (block-y b) 1)))
(check-expect (move-block (make-block 1 1)) (make-block 1 2))

;Block -> Block
;creates a block to the right,of the landed block.
;if block landed on most-edge of right,renders it on the left.
(define (new-block b)
(make-block (modulo (+ (block-x b) 1) BASE-UNIT) 0))
(check-expect (new-block (make-block 2 0)) (make-block 3 0))
(check-expect (new-block (make-block 8 0)) (make-block 9 0))
(check-expect (new-block (make-block 9 0)) (make-block 0 0))

;Tetris -> Tetris
;creates a new block,moves it down by 1 unit and changes to unmovable set.
(define (tetris-tock t)
(if (block-landed? t)
(make-tetris (new-block  (tetris-block t)) (cons (tetris-block t) (tetris-landscape t)))
(make-tetris (move-block (tetris-block t)) (tetris-landscape t))))
 
(check-expect (tetris-tock (make-tetris (make-block 5 9) '()))
(make-tetris (make-block 6 0) (list (make-block 5 9))))
(check-expect (tetris-tock (make-tetris (make-block 1 1) '()))
(make-tetris (make-block  1 2) '()))

;Block DX -> Block
;moves the block in horintal direction by dx.
(define (move-block-horizontal b dx)
(make-block (+ (block-x b) dx) (block-y b)))


;Tetris -> Tetris
;moves the block to the right. If the block at the edge of scene or
;there is a stack of blocks, ignores the movement.
(define (move-block-right t)
(if (or (= (block-x (tetris-block t)) 9)
        (my-member? (move-block-horizontal (tetris-block t) 1) (tetris-landscape t)))
    t
    (make-tetris (move-block-horizontal (tetris-block t) 1) (tetris-landscape t))))
(check-expect (move-block-right (make-tetris (make-block 5 6) '()))
(make-tetris (make-block 6 6) '()))


;Tetris-> Tetris
;moves the block to the left. If the block at the edge of scene or
;there is a stack of blocks,ignores the movement
(define (move-block-left t)
(if (or (= (block-x (tetris-block t)) 0)
        (my-member? (move-block-horizontal  (tetris-block t) -1 ) (tetris-landscape t)))
    t
    (make-tetris (move-block-horizontal (tetris-block t) -1) (tetris-landscape t))))

(check-expect (move-block-left (make-tetris (make-block 4 5) '()))
(make-tetris (make-block 3 5) '()))

;Tetris KeyEvent -> Tetris
;main function for movement
(define (tetris-control t ke)
(cond
[(key=? "right" ke) (move-block-right t)]
[(key=? "left" ke) (move-block-left t)]
[else t]))
(check-expect(tetris-control (make-tetris (make-block 5 5) '()) "left")
(make-tetris (make-block 4 5) '()))
(check-expect (tetris-control (make-tetris (make-block 5 5) '()) "right")
(make-tetris (make-block 6 5) '()))

;LandScape->Boolean
;checks whether one of the blocks is tounching the ceiling
(define (ceiling? l)
(cond
[(empty? l) #f]
[(cons? l) (or (= (block-y (first l)) 0)
               (ceiling? (rest l)))]))
(check-expect  (ceiling? '()) #f)
(check-expect (ceiling? (list (make-block 6 1) (make-block 2 2) (make-block 3 3))) #f)

;Tetris->Boolean
;main function for stop-when.
(define (tetris-end? t)
(ceiling? (tetris-landscape t)))

;LandScape Image -> Image
(define (tetris-render-aux l img)
(overlay (above (text TEXT-GAME-OVER TEXT-FONT "chocolate")
                (text (string-append "Stacked blocks: " (number->string (length l)))
                      TEXT-FONT "coral"))
         (rectangle WIDTH (* RADIUS 4)  "solid" (make-color 0 0 0 200)) img))

;Tetris-> Image
(define (tetris-final-render t)
(tetris-render-aux (tetris-landscape t) (tetris-render t)))

;Number -> Tetris
;launches the big-bang expression with some initial state
(define (tetris-main x)
(big-bang (make-tetris (make-block 0 0) '())
[on-tick tetris-tock x]
[to-draw tetris-render]
[on-key  tetris-control]
[stop-when tetris-end?  tetris-final-render]))

(tetris-main 0.1)
















 
















