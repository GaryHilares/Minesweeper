;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname minesweeper) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

;; Minesweeper game
(@htdw Board)

;; ==============
;; Constants

;; Window constants
(define WIDTH 800)
(define HEIGHT 600)
(define MTS (empty-scene WIDTH HEIGHT))

;; Tile constants
(define SQR-S 20)
(define SQR-AMO-X (floor (/ WIDTH SQR-S)))
(define SQR-AMO-Y (floor (/ HEIGHT SQR-S)))
(define TEXT-SIZE (* 3 (/ SQR-S 5)))

;; Mine constants
(define MINE 9)

;; ==============
;; Data definitions

(@htdd Tile)
(define-struct tile (num revealed?))
;; Tile is (make-tile Natural Boolean)
;; interp. num represents a mine if MINE,
;;         number of adjacent mines otherwise, and
;;         revealed? represents it has been revealed or not.
;; CONSTRAINT: num is in range 0-8 or is MINE.
(define T1 (make-tile 2 true))
(define T2 (make-tile 2 false))
(define T3 (make-tile 0 true))
(define T4 (make-tile 0 false))
(define T-BOOM (make-tile MINE true))
(define T-MINE (make-tile MINE false))

#;
(define (fn-for-tile t)
  (... (tile-num t)
       (tile-revealed? t)))

(@htdd Board)
;; Board is (listof (listof Tile)).
;; interp. A rectangular grid of tiles where the game is played.
;; CONSTRAINT: All of the lists within the main list are equal in size.
;; CONSTRAINT: Board is always valid according to minesweeper rules.
(define B1 empty)
(define B2 (list empty empty empty))
(define B3 (list (list T3)))
(define B4 (list (list T4)))
(define B5 (list (list T-MINE T2)
                 (list T2 T-MINE)))
(define B6 (list (list T-MINE T2)
                 (list T2 T-BOOM)))
(define B7 (list (list T3 T3)
                 (list T3 T4)))

#;
(define (fn-for-board b)
  (... (fn-for-lot (first b))
       (fn-for-board (rest b))))


;; =============
;; Functions

(@htdf main)
(@signature Board -> Board)
;; Start the game with (main (create-board <width> <height> <mine number>)).
;; <No check-expects for main function>.

(@template-origin htdw-main)
(define (main b)
  (big-bang b
    (to-draw render)
    (on-mouse handle-click)))


(@htdf render)
(@signature Board -> Image)
;; Renders the game board.
#; (define (render b) empty-image) ; stub
(check-expect (render B1) empty-image)
(check-expect (render B2) empty-image)
(check-expect (render B3) (overlay/align "center"
                                         "center"
                                         (text "0" TEXT-SIZE "black")
                                         (square SQR-S "outline" "black")))
(check-expect (render B4) (square SQR-S "outline" "black"))
(check-expect (render B5) (above (beside (square SQR-S "outline" "black")
                                         (square SQR-S "outline" "black"))
                                 (beside (square SQR-S "outline" "black")
                                         (square SQR-S "outline" "black"))))
(check-expect (render B6) (above (beside (square SQR-S "outline" "black")
                                         (square SQR-S "outline" "black"))
                                 (beside (square SQR-S "outline" "black")
                                         (overlay/align
                                          "center"
                                          "center"
                                          (square SQR-S "outline" "black")
                                          (text "M" TEXT-SIZE "black")))))
(check-expect (render B7) (above (beside (overlay/align
                                          "center"
                                          "center"
                                          (square SQR-S "outline" "black")
                                          (text "0" TEXT-SIZE "black"))
                                         (overlay/align
                                          "center"
                                          "center"
                                          (square SQR-S "outline" "black")
                                          (text "0" TEXT-SIZE "black")))
                                 (beside (overlay/align
                                          "center"
                                          "center"
                                          (square SQR-S "outline" "black")
                                          (text "0" TEXT-SIZE "black"))
                                         (square SQR-S "outline" "black"))))

(@template-origin use-abstract-fn)
(define (render b)
  (foldr above empty-image (map render-row b)))


(@htdf render-row)
(@signature (listof Tile) -> Image)
;; Renders a single row of the board.
#; (define (render-row r) empty-image) ; stub
(check-expect (render-row empty) empty-image)
(check-expect (render-row (list T3))
              (overlay/align "center"
                             "center"
                             (text "0" TEXT-SIZE "black")
                             (square SQR-S "outline" "black")))
(check-expect (render-row (list T4)) (square SQR-S "outline" "black"))
(check-expect (render-row (list T-MINE T2))
              (beside (square SQR-S "outline" "black")
                      (square SQR-S "outline" "black")))
(check-expect (render-row (list T2 T-MINE))
              (beside (square SQR-S "outline" "black")
                      (square SQR-S "outline" "black")))
(check-expect (render-row (list T2 T-BOOM))
              (beside (square SQR-S "outline" "black")
                      (overlay/align "center"
                                     "center"
                                     (square SQR-S "outline" "black")
                                     (text "M" TEXT-SIZE "black"))))
(check-expect (render-row (list T3 T3))
              (beside (overlay/align
                       "center"
                       "center"
                       (square SQR-S "outline" "black")
                       (text "0" TEXT-SIZE "black"))
                      (overlay/align
                       "center"
                       "center"
                       (square SQR-S "outline" "black")
                       (text "0" TEXT-SIZE "black"))))
(check-expect (render-row (list T3 T4))
              (beside (overlay/align
                       "center"
                       "center"
                       (square SQR-S "outline" "black")
                       (text "0" TEXT-SIZE "black"))
                      (square SQR-S "outline" "black")))

(@template-origin use-abstract-fn)
(define (render-row r)
  (foldr beside empty-image (map render-tile r)))


(@htdf render-tile)
(@signature Tile -> Image)
;; Renders a single tile of the board.
#; (define (render-tile t) empty-image) ; stub
(check-expect (render-tile T1) (overlay/align "center"
                                              "center"
                                              (text "2" TEXT-SIZE "black")
                                              (square SQR-S "outline" "black")))
(check-expect (render-tile T2) (square SQR-S "outline" "black"))
(check-expect (render-tile T3) (overlay/align "center"
                                              "center"
                                              (text "0" TEXT-SIZE "black")
                                              (square SQR-S "outline" "black")))
(check-expect (render-tile T4) (square SQR-S "outline" "black"))
(check-expect (render-tile T-BOOM)
              (overlay/align "center"
                             "center"
                             (text "M" TEXT-SIZE "black")
                             (square SQR-S "outline" "black")))
(check-expect (render-tile T-MINE) (square SQR-S "outline" "black"))

(@template-origin Tile)
(define (render-tile t)
  (if (not (tile-revealed? t))
      (square SQR-S "outline" "black")
      (if (= (tile-num t) MINE)
          (overlay/align "center"
                         "center"
                         (text "M" TEXT-SIZE "black")
                         (square SQR-S "outline" "black"))
          (overlay/align "center"
                         "center"
                         (text (number->string (tile-num t)) TEXT-SIZE "black")
                         (square SQR-S "outline" "black")))))

(@htdf handle-click)
(@signature Board Integer Integer MouseEvent -> Board)
;; Handle click.
#; (define (handle-click b x y me) b) ; stub
(check-expect (handle-click B4
                            (floor (/ SQR-S 2))
                            (floor (/ SQR-S 2))
                            "button-down")
              B3)
(check-expect (handle-click B4
                            (floor (/ SQR-S 2))
                            (floor (/ SQR-S 2))
                            "button-up")
              B4)
(check-expect (handle-click B5
                            (floor (/ SQR-S 2))
                            (floor (/ SQR-S 2))
                            "button-up")
              B5)
(check-expect (handle-click B3
                            (floor (/ SQR-S 2))
                            (floor (/ SQR-S 2))
                            "button-down")
              B3)
(check-expect (handle-click B5
                            (floor (/ SQR-S 2))
                            (floor (/ SQR-S 2))
                            "button-down")
              (list (list T-BOOM T2)
                    (list T2 T-MINE)))
(check-expect (handle-click B5 (add1 SQR-S) (floor (/ SQR-S 2)) "button-down")
              (list (list T-MINE T1)
                    (list T2 T-MINE)))
(check-expect (handle-click B5 (floor (/ SQR-S 2)) (add1 SQR-S) "button-down")
              (list (list T-MINE T2)
                    (list T1 T-MINE)))
(check-expect (handle-click B5 (add1 SQR-S) (add1 SQR-S) "button-down")
              (list (list T-MINE T2)
                    (list T2 T-BOOM)))
(check-expect (handle-click B7 (add1 SQR-S) (floor (/ SQR-S 2)) "button-down")
              B7)
(check-expect (handle-click B7 (floor (/ SQR-S 2)) (add1 SQR-S) "button-down")
              B7)
(check-expect (handle-click B7 (add1 SQR-S) (add1 SQR-S) "button-down")
              (list (list T3 T3) (list T3 T3)))


(@template-origin MouseEvent)
(define (handle-click b x y me)
  (cond [(mouse=? me "button-down")
         (reveal b (floor (/ x SQR-S)) (floor (/ y SQR-S)))]
        [else b]))


(@htdf reveal)
(@signature Board Integer Integer -> Board)
;; Reveals the tile at the x y coordinates.
(check-expect (reveal B4 0 0) B3)
(check-expect (reveal B3 0 0) B3)
(check-expect (reveal B5 0 0)
              (list (list T-BOOM T2)
                    (list T2 T-MINE)))
(check-expect (reveal B5 1 0)
              (list (list T-MINE T1)
                    (list T2 T-MINE)))
(check-expect (reveal B5 0 1)
              (list (list T-MINE T2)
                    (list T1 T-MINE)))
(check-expect (reveal B5 1 1)
              (list (list T-MINE T2)
                    (list T2 T-BOOM)))
(check-expect (reveal B7 1 0) B7)
(check-expect (reveal B7 0 1) B7)
(check-expect (reveal B7 0 1) B7)
(check-expect (reveal B7 1 1) (list (list T3 T3) (list T3 T3)))
#; (define (reveal b x y) b) ; stub

(@template-origin encapsulated Board (listof Tile) Tile accumulator)
(define (reveal b0 x0 y0)
  (local [(define (reveal--tile t)
            (make-tile (tile-num t) true))
          (define (reveal--row r x)
            (cond [(empty? r) (error "x not in bounds.")]
                  [(zero? x) (cons (reveal--tile (first r)) (rest r))]
                  [else (cons (first r) (reveal--row (rest r) (sub1 x)))]))
          (define (reveal--board b y)
            (cond [(empty? b) (error "y not in bounds.")]
                  [(zero? y) (cons (reveal--row (first b) x0) (rest b))]
                  [else (cons (first b) (reveal--board (rest b) (sub1 y)))]))]
    (reveal--board b0 y0)))


(@htdf create-board)
(@signature Natural Natural Natural -> Board)
;; Creates a nxm-sized Board with k mines at random positions.
#; (define (add-mines n m k) B5)
;; !!! Add check-expects

;; !!! Add template-origin
(define (create-board n m k)
  (local [(define (lst-get lst n)
            (cond [(empty? lst) (error "Index out of range")]
                  [(zero? n) (first lst)]
                  [else (lst-get (rest lst) (sub1 n))]))
          (define (lst-remove lst n)
            (cond [(empty? lst) (error "Index out of range")]
                  [(zero? n) (rest lst)]
                  [else (cons (first lst) (lst-remove (rest lst) (sub1 n)))]))
          (define (random-choices lst n)
            (cond [(zero? n) empty]
                  [else
                   (local [(define random-choice (random (length lst)))]
                     (cons (lst-get lst random-choice)
                           (random-choices (lst-remove lst random-choice)
                                           (sub1 n))))]))
          (define (generate-tile-number mine-positions y x)
            (if (member? (+ (* y n) x) mine-positions)
                MINE
                (+ (if (member? (+ (sub1 x) (* (sub1 y) n)) mine-positions) 1 0)
                   (if (member? (+ x (* (sub1 y) n)) mine-positions) 1 0)
                   (if (member? (+ (add1 x) (* (sub1 y) n)) mine-positions) 1 0)
                   (if (member? (+ (sub1 x) (* y n)) mine-positions) 1 0)
                   (if (member? (+ (add1 x) (* y n)) mine-positions) 1 0)
                   (if (member? (+ (sub1 x) (* (add1 y) n)) mine-positions) 1 0)
                   (if (member? (+ x (* (add1 y) n)) mine-positions) 1 0)
                   (if (member? (+ (add1 x) (* (add1 y) n)) mine-positions)
                       1
                       0))))
          (define (generate-row mine-positions y x)
            (cond [(= x n) empty]
                  [else (cons (make-tile
                               (generate-tile-number mine-positions y x)
                               false)
                              (generate-row mine-positions y (add1 x)))]))
          (define (generate-map mine-positions y)
            (cond [(= y m) empty]
                  [else (cons (generate-row mine-positions y 0)
                              (generate-map mine-positions (add1 y)))]))]
    (generate-map (random-choices (build-list (* n m) identity) k) 0)))
