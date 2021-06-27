(in-package :chess)

(defun make-board ()
  (fset:convert
   'fset:seq
   (format
    nil
    "         ~%         ~% rnbqkbnr~% pppppppp~% ........~% ........~% ........~% ........~% PPPPPPPP~% RNBQKBNR~%         ~%         ~%")))

(defun make-board-state (&optional (headers (fset:seq '("Event" "?")
                                                      '("Site" "?")
                                                      '("Date" "????.??.??")
                                                      '("Round" "?")
                                                      '("White" "?")
                                                      '("Black" "?")
                                                      '("Result" "*"))))
  (fset:map (:board (make-board))
            (:turn +white+)
            (:white-captures (fset:empty-seq))
            (:black-captures (fset:empty-seq))
            (:uci-moves (fset:empty-seq))
            (:san-moves (fset:empty-seq))
            (:num-half-moves 0)
            (:num-full-moves 1)
            (:white-kingside-castle t)
            (:white-queenside-castle t)
            (:black-kingside-castle t)
            (:black-queenside-castle t)
            (:en-passant-index nil)
            (:previous-board-states (fset:empty-seq))
            (:headers headers)))

(defun print-board (board)
  (dotimes (n 8)
    (let* ((start-index (* 10 (+ n 2)))
           (rank (fset:subseq board
                              (+ start-index 1)
                              (+ start-index 9)))
           (rank-list (fset:convert 'list rank)))
      (format t "~{~a~^ ~}~%" rank-list))))

(defun index->row-num (index)
  (when (<= 20 index 99)
    (- (floor index 10) 2)))

(defun index->col-num (index)
  (when (<= 20 index 99)
    (rem index 10)))

(defun index->file (index)
  (case (index->col-num index)
    (1 #\a)
    (2 #\b)
    (3 #\c)
    (4 #\d)
    (5 #\e)
    (6 #\f)
    (7 #\g)
    (8 #\h)))

(defun index->rank (index)
  (- 8 (index->row-num index)))

(defun square->index (square)
  (let ((file-addition (case (char square 0)
                         (#\a 1)
                         (#\b 2)
                         (#\c 3)
                         (#\d 4)
                         (#\e 5)
                         (#\f 6)
                         (#\g 7)
                         (#\h 8)
                         (otherwise -1)))
        (rank-multiplier (- 8
                            (digit-char-p (char square 1)))))
    (when (and (<= 0 rank-multiplier 7)
               (<= 1 file-addition 8))
      (+ (* rank-multiplier 10)
         file-addition
         20))))

(defun index->square (index)
  (let ((file (index->file index))
        (rank (index->rank index)))
    (format nil "~a~a" file rank)))

(defun move->uci-move-name (move)
  (str:concat (index->square (first move)) (index->square (second move)) (third move)))

(defun piece-belongs-to-color (piece color)
  (member piece (if (eq +white+ color)
                    +white-pieces+
                    +black-pieces+)))

(defun enemy-color (color)
  (if (eq +white+ color)
      +black+
      +white+))

(defun enemy-pieces (color)
  (if (eq +white+ color)
      +black-pieces+
      +white-pieces+))

(defun enemy-piece? (color piece)
  (piece-belongs-to-color piece (enemy-color color)))

(defun is-rook? (piece)
  (or (eq piece +white-rook+)
      (eq piece +black-rook+)))

(defun is-knight? (piece)
  (or (eq piece +white-knight+)
      (eq piece +black-knight+)))

(defun is-bishop? (piece)
  (or (eq piece +white-bishop+)
      (eq piece +black-bishop+)))

(defun is-queen? (piece)
  (or (eq piece +white-queen+)
      (eq piece +black-queen+)))

(defun is-king? (piece)
  (or (eq piece +white-king+)
      (eq piece +black-king+)))

(defun is-pawn? (piece)
  (or (eq piece +white-pawn+)
      (eq piece +black-pawn+)))

(defun same-rank? (source-index dest-index)
  (= (index->row-num source-index) (index->row-num dest-index)))

(defun same-file? (source-index dest-index)
  (= (index->col-num source-index) (index->col-num dest-index)))

(defun same-diagonal? (source-index dest-index)
  (let ((source-row-num (index->row-num source-index))
        (source-col-num (index->col-num source-index))
        (dest-row-num (index->row-num dest-index))
        (dest-col-num (index->col-num dest-index)))
    (= (abs (- dest-row-num source-row-num))
       (abs (- dest-col-num source-col-num)))))

(defun empty? (board index)
  (eq +empty-square+ (fset:@ board index)))

(defun range (start end &key (step 1))
  (alexandria:iota (ceiling (abs (- end start))
                            (abs step))
                   :start start
                   :step step))

(defun get-pieces (board color)
  (loop for index from 0
        for piece in (fset:convert 'list board)
        when (piece-belongs-to-color piece color)
          collect (list piece index)))

(defun get-pinning-pieces (board color)
  (remove-if-not
   (lambda (piece-index)
     (member (first piece-index)
             (list +white-rook+ +white-bishop+ +white-queen+
                   +black-rook+ +black-bishop+ +black-queen+)))
   (get-pieces board color)))

;; FIXME might be better logic (but uglier code) to not have to rely on board here
(defun in-board? (board index)
  (not (str:blank? (fset:@ board index))))

(defun validate-moves (color board moves &key (include-attacking? nil))
  (remove-if
   (lambda (index)
     (or (not (in-board? board index))
         (and (not include-attacking?)
              (piece-belongs-to-color (fset:@ board index) color))))
   moves))

(defun generate-moves-in-step (color board index step &key (include-attacking? nil))
  (loop for new-index = (+ index step) then (+ new-index step)
        while (and (in-board? board new-index)
                   (empty? board new-index))
        collect new-index into moves
        finally (return (if (and (in-board? board new-index)
                                 (or (enemy-piece? color (fset:@ board new-index))
                                     include-attacking?))
                            (append moves (list new-index))
                            moves))))


(defun generate-rook-moves (color board index &key (include-attacking? nil))
  (apply #'append
         (map
          'list
          (lambda (x) (generate-moves-in-step color board index x :include-attacking? include-attacking?))
          (fset:@ +piece-directions+ +rook+))))

(defun generate-knight-moves (color board index &key (include-attacking? nil))
  (validate-moves
   color
   board
   (map 'list
        (lambda (index-dir) (+ index index-dir))
        (fset:@ +piece-directions+ +knight+))
   :include-attacking? include-attacking?))

(defun generate-bishop-moves (color board index &key (include-attacking? nil))
  (apply #'append
         (map
          'list
          (lambda (step) (generate-moves-in-step color board index step :include-attacking? include-attacking?))
          (fset:@ +piece-directions+ +bishop+))))

(defun generate-queen-moves (color board index &key (include-attacking? nil))
  (append (generate-rook-moves color board index :include-attacking? include-attacking?)
          (generate-bishop-moves color board index :include-attacking? include-attacking?)))


(defun generate-king-moves (color board index &key (include-attacking? nil))
  (validate-moves
   color
   board
   (map 'list
        (lambda (index-dir) (+ index index-dir))
        (fset:@ +piece-directions+ +king+))
   :include-attacking? include-attacking?))

(defun generate-pawn-moves (en-passant-index color board index &key (include-attacking? nil))
  (let* ((white? (eq +white+ color))
         (promotion? (= (index->row-num index)
                        (if white? 1 6)))
         (pawn-piece (if white? +white-pawn+ +black-pawn+))
         (move-indices (map 'list
                            (lambda (index-dir) (+ index index-dir))
                            (fset:@ +piece-directions+ pawn-piece)))
         (moves (remove-if
                 (lambda (dest-index)
                   (let* ((double-move (= (+ +south+ +south+) (abs (- dest-index index))))
                          (single-move (= +south+ (abs (- dest-index index))))
                          (diagonal-move (not (or double-move
                                                  single-move))))
                     (or (not (in-board? board dest-index))
                         (and (not include-attacking?)
                              (piece-belongs-to-color (fset:@ board dest-index) color))
                         (and single-move
                              (not (empty? board dest-index)))
                         (and double-move
                              (or
                               (not (empty? board dest-index))
                               (not (empty? board (+ dest-index (if white? +south+ +north+))))))
                         (and double-move
                              (not (= (index->row-num index)
                                      (if white? 6 1))))
                         (and diagonal-move
                              (not (or (enemy-piece? color (fset:@ board dest-index))
                                       (eq dest-index en-passant-index)))))))
                 move-indices)))
    (apply
     #'append
     (map 'list
          (lambda (dest-index)
            (if promotion?
                (map 'list
                     (lambda (promotion-piece) (list dest-index promotion-piece))
                     (list "q" "r" "b" "n"))
                (list dest-index)))
          moves))))

(defun find-intermediate-indicies (i1 i2)
  ;; Note this only works if the pieces are in the same file/rank/diagonal
  ;; Otherwise they could result in out of the board errors
  ;; (the index will still be in the board but the path would have travelled outside the board).
  (let* ((source-col-num (index->col-num i1))
         (dest-col-num (index->col-num i2))
         (backwards-move? (< i2 i1))
         (backwards-diagonal? (> source-col-num dest-col-num))
         (same-file? (same-file? i1 i2))
         (same-rank? (same-rank? i1 i2))
         (same-diagonal? (same-diagonal? i1 i2))
         (step (cond
                 ((and same-rank? backwards-move?) +west+)
                 ((and same-rank? (not backwards-move?)) +east+)
                 ((and same-file? backwards-move?) +north+)
                 ((and same-file? (not backwards-move?)) +south+)
                 ((and same-diagonal? backwards-move? (not backwards-diagonal?)) (+ +north+ +east+))
                 ((and same-diagonal? backwards-move? backwards-diagonal?) (+ +north+ +west+))
                 ((and same-diagonal? (not backwards-move?) backwards-diagonal?) (+ +south+ +west+))
                 ((and same-diagonal? (not backwards-move?) (not backwards-diagonal?)) (+ +south+ +east+))))
         (indices (range (+ i1 step) i2 :step step)))
    indices))

(defun find-pinned-pieces (board color)
  (let ((enemy-pinning-pieces (get-pinning-pieces board (enemy-color color)))
        (king-index (fset:position (if (eq color +white+) +white-king+ +black-king+) board)))
    (remove
     nil
     (map 'list
          (lambda (piece-index)
            (let* ((piece (first piece-index))
                   (index (second piece-index))
                   (same-file? (same-file? index king-index))
                   (same-rank? (same-rank? index king-index))
                   (same-diagonal? (same-diagonal? index king-index)))
              (when (or (and (is-rook? piece)
                             (or same-file? same-rank?))
                        (and (is-bishop? piece)
                             same-diagonal?)
                        (and (is-queen? piece)
                             (or same-file? same-rank? same-diagonal?)))
                (let* ((indices (find-intermediate-indicies index king-index))
                       (pieces (arrows:->>  indices
                                            (map 'list (lambda (index) (list (fset:@ board index) index)))
                                            (remove-if (lambda (piece-index) (eq (first piece-index) +empty-square+))))))
                  (if (and pieces
                           (= 1 (length pieces))
                           (piece-belongs-to-color (first (first pieces)) color))
                      (second (first pieces))
                      nil)))))
          enemy-pinning-pieces))))

(defun generate-psuedo-legal-moves (board color en-passant-index &key (pieces (get-pieces board color)) (include-attacking? nil))
  (apply #'append
         (map
          'list
          (lambda (x)
            (let*  ((piece (first x))
                    (index (second x))
                    (moves-fn (cond
                                ((is-rook? piece) #'generate-rook-moves)
                                ((is-knight? piece) #'generate-knight-moves)
                                ((is-bishop? piece) #'generate-bishop-moves)
                                ((is-queen? piece) #'generate-queen-moves)
                                ((is-king? piece) #'generate-king-moves)
                                ((is-pawn? piece) (alexandria:curry #'generate-pawn-moves en-passant-index)))))
              (map 'list
                   (lambda (dest-index)
                     (if (is-pawn? piece)
                         (append (list index) (list dest-index))
                         (list index dest-index)))
                   (funcall moves-fn color board index :include-attacking? include-attacking?))))
          pieces)))

(defun generate-psuedo-legal-moves-from-board-state (board-state)
  (generate-psuedo-legal-moves
   (fset:@ board-state :board)
   (enemy-color (fset:@ board-state :turn))
   (fset:@ board-state :en-passant-index)))

(defun generate-castling-moves (board color castling-rights attacked-indices)
  (arrows:cond-> '()
                 ((and (eq +white+ color)
                       (fset:@ castling-rights :white-queenside-castle)
                       (every (lambda (index)
                                (and
                                 (empty? board index)
                                 (not (fset:contains? attacked-indices index))))
                              (list 92 93 94)))
                  (append (list (list 95 93))))

                 ((and (eq +white+ color)
                       (fset:@ castling-rights :white-kingside-castle)
                       (every (lambda (index)
                                (and
                                 (empty? board index)
                                 (not (fset:contains? attacked-indices index))))
                              (list 96 97)))
                  (append (list (list 95 97))))

                 ((and (eq +black+ color)
                       (fset:@ castling-rights :black-queenside-castle)
                       (every (lambda (index)
                                (and
                                 (empty? board index)
                                 (not (fset:contains? attacked-indices index))))
                              (list 22 23 24)))
                  (append (list (list 25 23))))

                 ((and (eq +black+ color)
                       (fset:@ castling-rights :black-kingside-castle)
                       (every (lambda (index)
                                (and
                                 (empty? board index)
                                 (not (fset:contains? attacked-indices index))))
                              (list 26 27)))
                  (append (list (list 25 27))))))

(defun generate-enemy-piece->moves (board-state)
  (let* ((board (fset:@ board-state :board))
         (color (fset:@ board-state :turn))
         (enemy-color (enemy-color color)))
    (reduce
     (lambda (acc piece)
       (fset:with acc piece (generate-psuedo-legal-moves
                             board
                             enemy-color
                             (fset:@ board-state :en-passant-index)
                             :pieces (list piece)
                             :include-attacking? t)))
     (get-pieces board enemy-color)
     :initial-value (fset:empty-map))))

(defun generate-legal-moves (board-state  &optional (enemy-piece->moves (generate-enemy-piece->moves board-state)))
  (let* ((color (fset:@ board-state :turn))
         (board (fset:@ board-state :board))
         (en-passant-index (fset:@ board-state :en-passant-index))
         (pinned-pieces (find-pinned-pieces board color))
         (king-piece (if (eq color +white+) +white-king+ +black-king+))
         (king-index (fset:position king-piece board))
         (enemy-piece-attacking-king->moves (fset:filter (lambda (_ v)
                                                           (declare (ignore _))
                                                           (some (lambda (move) (= king-index (second move))) v))
                                                         enemy-piece->moves))
         (attacked-indices (fset:reduce
                            (lambda (acc moves)
                              (if moves
                                  (fset:union acc (fset:convert 'fset:set (fset:image #'second moves)))
                                  acc))
                            (fset:range enemy-piece->moves)
                            :initial-value (fset:empty-set)))
         (psuedo-legal-king-moves (generate-king-moves color board king-index))
         (legal-king-moves (arrows:->> psuedo-legal-king-moves
                                       (remove-if (lambda (dest-index) (fset:contains? attacked-indices dest-index)))
                                       (map 'list (lambda (dest-index) (list king-index dest-index)))))
         (in-check? (fset:contains? attacked-indices king-index))
         (king-castling-moves (when (not in-check?)
                                (generate-castling-moves board color board-state attacked-indices)))
         (other-psuedo-legal-moves (generate-psuedo-legal-moves
                                    board
                                    color
                                    en-passant-index
                                    :pieces (remove-if
                                             (lambda (x)
                                               (or (member (second x) pinned-pieces)
                                                   (eq (first x) king-piece)))
                                             (get-pieces board color))))
         (enemy-pieces-attacking-king-squares (arrows:->> enemy-piece-attacking-king->moves
                                                          fset:domain
                                                          (fset:image (lambda (enemy-piece)
                                                                        (let ((enemy-piece-index (second enemy-piece)))
                                                                          (fset:with (fset:convert 'fset:set (find-intermediate-indicies enemy-piece-index king-index))
                                                                                     enemy-piece-index))))
                                                          (fset:convert 'list)))
         (other-legal-moves (if in-check?
                                (remove-if-not
                                 (lambda (move)
                                   (every
                                    (lambda (indices)
                                      (fset:contains? indices (second move)))
                                    enemy-pieces-attacking-king-squares))
                                 other-psuedo-legal-moves)
                                other-psuedo-legal-moves)))
    (append legal-king-moves king-castling-moves other-legal-moves)))

(defun is-legal-move? (board-state move)
  (member move (map 'list #'move->uci-move-name (generate-legal-moves board-state)) :test #'equal))

(defun push-move (board-state move &optional (san-move (uci->san-move-name board-state move)))
  (ppcre:register-groups-bind
      (source-square dest-square promotion)
      ("([a-h][1-8])([a-h][1-8])([qbnr]|$)" move :sharedp t)
    (let* ((source-index (square->index source-square))
           (dest-index (square->index dest-square))
           (board (fset:@ board-state :board))
           (color (fset:@ board-state :turn))
           (piece (fset:@ board source-index))
           (promotion-piece (when (string/= promotion "")
                              (case promotion
                                ("q" (if (eq color +white+) +white-queen+ +black-queen+))
                                ("b" (if (eq color +white+) +white-bishop+ +black-bishop+))
                                ("n" (if (eq color +white+) +white-knight+ +black-knight+))
                                ("r" (if (eq color +white+) +white-rook+ +black-rook+)))))
           (new-piece (if promotion-piece promotion-piece piece))
           (white-kingside-castle? (and (= source-index 95)
                                        (= dest-index 97)))
           (white-queenside-castle? (and (= source-index 95)
                                         (= dest-index 93)))
           (black-kingside-castle? (and (= source-index 25)
                                        (= dest-index 27)))
           (black-queenside-castle? (and (= source-index 25)
                                         (= dest-index 23)))
           (new-board (cond
                        (white-kingside-castle? (arrows:-> board
                                                           (fset:with source-index +empty-square+)
                                                           (fset:with dest-index new-piece)
                                                           (fset:with 98 +empty-square+)
                                                           (fset:with 96 +white-rook+)))
                        (white-queenside-castle? (arrows:-> board
                                                            (fset:with source-index +empty-square+)
                                                            (fset:with dest-index new-piece)
                                                            (fset:with 91 +empty-square+)
                                                            (fset:with 94 +white-rook+)))
                        (black-kingside-castle? (arrows:-> board
                                                           (fset:with source-index +empty-square+)
                                                           (fset:with dest-index new-piece)
                                                           (fset:with 28 +empty-square+)
                                                           (fset:with 26 +black-rook+)))
                        (black-queenside-castle? (arrows:-> board
                                                            (fset:with source-index +empty-square+)
                                                            (fset:with dest-index new-piece)
                                                            (fset:with 21 +empty-square+)
                                                            (fset:with 24 +black-rook+)))
                        (t  (arrows:-> board
                                       (fset:with source-index +empty-square+)
                                       (fset:with dest-index new-piece)))))
           (pawn-move? (is-pawn? piece))
           (capture? (not (empty? board dest-index)))
           (white-captures (fset:@ board-state :white-captures))
           (new-white-captures (if (and capture? (eq +white+ color))
                                   (fset:with-last white-captures (fset:@ board dest-index))
                                   white-captures))
           (black-captures (fset:@ board-state :black-captures))
           (new-black-captures (if (and capture? (eq +black+ color))
                                   (fset:with-last black-captures (fset:@ board dest-index))
                                   black-captures))
           (new-half-move-count (if (or pawn-move? capture?)
                                    0
                                    (+ (fset:@ board-state :num-half-moves) 1)))
           (num-full-moves (fset:@ board-state :num-full-moves))
           (new-full-move-count (if (eql +black+ color)
                                    (+ 1 num-full-moves)
                                    num-full-moves))
           (en-passant-index (if (or (and (eq +white+ color)
                                          (= 6 (index->row-num source-index))
                                          (= 4 (index->row-num dest-index)))
                                     (and (eq +black+ color)
                                          (= 1 (index->row-num source-index))
                                          (= 3 (index->row-num dest-index))))
                                 (/ (+ dest-index source-index) 2)
                                 nil))
           (new-moves (fset:with-last (fset:@ board-state :uci-moves) move))
           (new-san-moves (fset:with-last (fset:@ board-state :san-moves) san-move))
           (previous-board-states (fset:with-last (fset:@ board-state :previous-board-states) (fset:less board-state :previous-board-states)))
           (white-kingside-castle (and (fset:@ board-state :white-kingside-castle)
                                       (/= source-index 95 98)
                                       (not white-kingside-castle?)
                                       (not white-queenside-castle?)))
           (white-queenside-castle (and (fset:@ board-state :white-queenside-castle)
                                        (/= source-index 95 91)
                                        (not white-kingside-castle?)
                                        (not white-queenside-castle?)))
           (black-kingside-castle (and (fset:@ board-state :black-kingside-castle)
                                       (/= source-index 25 28)
                                       (not black-kingside-castle?)
                                       (not black-queenside-castle?)))
           (black-queenside-castle (and (fset:@ board-state :black-queenside-castle)
                                        (/= source-index 25 21)
                                        (not black-kingside-castle?)
                                        (not black-queenside-castle?))))
      (arrows:-> board-state
                 (fset:with :board new-board)
                 (fset:with :turn (enemy-color color))
                 (fset:with :white-captures new-white-captures)
                 (fset:with :black-captures new-black-captures)
                 (fset:with :num-half-moves new-half-move-count)
                 (fset:with :num-full-moves new-full-move-count)
                 (fset:with :uci-moves new-moves)
                 (fset:with :san-moves new-san-moves)
                 (fset:with :en-passant-index en-passant-index)
                 (fset:with :previous-board-states previous-board-states)
                 (fset:with :white-kingside-castle white-kingside-castle)
                 (fset:with :white-queenside-castle white-queenside-castle)
                 (fset:with :black-kingside-castle black-kingside-castle)
                 (fset:with :black-queenside-castle black-queenside-castle)))))

(defun push-moves (board-state moves)
  (reduce #'push-move moves :initial-value board-state))

(defun find-move (board-state source-piece source-file source-rank capture? dest-square promotion &optional (legal-moves (generate-legal-moves board-state)))
  (let* ((board (fset:@ board-state :board))
         (color (fset:@ board-state :turn))
         (legal-move-triplets (map 'list (lambda (move) (cons (fset:@ board (first move)) move)) legal-moves))
         (dest-index (square->index dest-square))
         (dest-empty? (empty? board dest-index))
         (source-piece (if source-piece source-piece "P"))
         (piece-pred (alexandria:switch (source-piece :test #'equal)
                       ("P" #'is-pawn?)
                       ("N" #'is-knight?)
                       ("B" #'is-bishop?)
                       ("K" #'is-king?)
                       ("Q" #'is-queen?)
                       ("R" #'is-rook?)))
         (potential-moves (remove-if (lambda (triplet)
                                       (destructuring-bind (tpiece tsource-index tdest-index) triplet
                                         (or (and source-piece
                                                  (not (funcall piece-pred tpiece)))
                                             (and source-file
                                                  (not (eq (char source-file 0) (index->file tsource-index))))
                                             (and source-rank
                                                  (not (= source-rank (index->rank tsource-index))))
                                             (not (= dest-index tdest-index))
                                             (and capture?
                                                  dest-empty?)
                                             (and (not capture?)
                                                  (not dest-empty?))
                                             (and promotion
                                                  (or (not (is-pawn? tpiece))
                                                      (not (= (index->rank dest-index)
                                                              (if (eq +white+ color) 8 1))))))))
                                     legal-move-triplets))
         (move (when (= 1 (length potential-moves))
                 (first potential-moves))))
    (when move
      (move->uci-move-name (list (second move) (third move) (string-trim '(#\=) (if promotion promotion "")))))))

(defun san->uci-move-name (board-state san)
  (cond
    ((member san (list "O-O" "O-O+" "O-O#") :test #'equal)
     (let ((white? (eq +white+ (fset:@ board-state :turn))))
       (find-move board-state "K" "e" (if white? 1 8) nil (if white? "g1" "g8") nil)))


    ((member san (list "O-O-O" "O-O-O+" "O-O-O#") :test #'equal)
     (let ((white? (eq +white+ (fset:@ board-state :turn))))
       (find-move board-state "K" "e" (if white? 1 8) nil (if white? "g1" "g8") nil)))

    (t (ppcre:register-groups-bind
           (source-piece source-file (#'parse-integer source-rank) capture? dest-square promotion)
           ("^([NBKRQ])?([a-h])?([1-8])?([\x])?([a-h][1-8])(=[nbrq])?[\+#]?$" san :sharedp t)
         (find-move board-state source-piece source-file source-rank capture? dest-square promotion)))))

(defun uci->san-move-name (board-state uci)
  (let* ((move-str (cond
                     ((member uci '("e1g1" "e8g8") :test #'equal) "O-O")
                     ((member uci '("e1c1" "e8c8") :test #'equal) "O-O-O")
                     (t (ppcre:register-groups-bind
                            (source-square dest-square promotion)
                            ("([a-h][1-8])([a-h][1-8])([qbnr]|$)" uci :sharedp t)
                          (let* ((board (fset:@ board-state :board))
                                 (source-index (square->index source-square))
                                 (dest-index (square->index dest-square))
                                 (piece (fset:@ board source-index))
                                 (legal-moves (generate-legal-moves board-state))
                                 (legal-move-triplets (map 'list (lambda (move) (cons (fset:@ board (first move)) move)) legal-moves))
                                 (possible-moves (remove-if-not (lambda (move)
                                                                  (and (eq piece (first move))
                                                                       (= dest-index (third move))))
                                                                legal-move-triplets))
                                 (ambigious-move? (> (length possible-moves) 1))
                                 (piece-str (cond
                                              ((is-pawn? piece) "")
                                              ((is-knight? piece) "N")
                                              ((is-bishop? piece) "B")
                                              ((is-rook? piece) "R")
                                              ((is-queen? piece) "Q")
                                              ((is-king? piece) "K")))
                                 (source-file (if (and ambigious-move?
                                                       (not (apply #'eq (map 'list (lambda (x) (index->file (second x))) possible-moves))))
                                                  (index->file source-index)
                                                  ""))
                                 (source-rank (if (and ambigious-move?
                                                       (equal "" source-file))
                                                  (index->rank source-index)
                                                  ""))
                                 (capture? (if (empty? board dest-index)
                                               ""
                                               "x"))
                                 (promotion-str (if (not (str:blank? promotion))
                                                    (format nil "=~a" promotion)
                                                    "")))
                            (format nil "~a~a~a~a~a~a"
                                    piece-str
                                    source-file
                                    source-rank
                                    capture?
                                    dest-square
                                    promotion-str))))))
         (new-board-state (push-move board-state uci move-str))
         (check-str (if (in-check? new-board-state)
                        "+"
                        ""))
         (checkmate-str (if (is-checkmate? new-board-state)
                            "#"
                            "")))
    (format nil "~a~a~a" move-str check-str checkmate-str)))

(defun push-san-move (board-state san)
  (push-move board-state (san->uci-move-name board-state san) san))

(defun push-san-moves (board-state san-moves)
  (reduce #'push-san-move san-moves :initial-value board-state))

(defun pop-move (board-state)
  (fset:with (fset:last (fset:@ board-state :previous-board-states))
             :previous-board-states
             (fset:less-last (fset:@ board-state :previous-board-states))))

(defun board-empty-validation (board-state)
  (let* ((board (fset:@ board-state :board))
         (white-pieces (get-pieces board +white+))
         (black-pieces (get-pieces board +black+)))
    (if (and (>= (length white-pieces) 1)
             (>= (length black-pieces) 1))
        (values t nil)
        (values nil :EMPTY))))

(defun board-white-king-validation (board-state)
  (let* ((board (fset:@ board-state :board))
         (num-white-kings (fset:count +white-king+ board)))
    (cond
      ((< num-white-kings 1) (values nil :NO-WHITE-KING))
      ((> num-white-kings 1) (values nil :TOO-MANY-WHITE-KINGS))
      (t (values t nil)))))

(defun board-black-king-validation (board-state)
  (let* ((board (fset:@ board-state :board))
         (num-black-kings (fset:count +black-king+ board)))
    (cond
      ((< num-black-kings 1) (values nil :NO-BLACK-KING))
      ((> num-black-kings 1) (values nil :TOO-MANY-BLACK-KINGS))
      (t (values t nil)))))

(defun board-white-pieces-validation (board-state)
  (let ((num-white-pieces (length (get-pieces (fset:@ board-state :board) +white+))))
    (if (> num-white-pieces 16)
        (values nil :TOO-MANY-WHITE-PIECES)
        (values t nil))))

(defun board-black-pieces-validation (board-state)
  (let ((num-black-pieces (length (get-pieces (fset:@ board-state :board) +black+))))
    (if (> num-black-pieces 16)
        (values nil :TOO-MANY-BLACK-PIECES)
        (values t nil))))

(defun board-white-pawns-validation (board-state)
  (let* ((board (fset:@ board-state :board))
         (num-white-pawns (fset:count +white-pawn+ board)))
    (if (> num-white-pawns 8)
        (values nil :TOO-MANY-WHITE-PAWNS)
        (values t nil))))

(defun board-black-pawns-validation (board-state)
  (let* ((board (fset:@ board-state :board))
         (num-black-pawns (fset:count +black-pawn+ board)))
    (if (> num-black-pawns 8)
        (values nil :TOO-MANY-BLACK-PAWNS)
        (values t nil))))

(defun board-white-pawns-backrank-validation (board-state)
  (let* ((white-pieces (get-pieces (fset:@ board-state :board) +white+))
         (white-pawns (remove-if-not (lambda (x) (eq (first x) +white-pawn+)) white-pieces))
         (white-pawn-indices (map 'list #'second white-pawns)))
    (if (some (lambda (x) (= 8 (index->rank x))) white-pawn-indices)
        (values nil :WHITE-PAWN-ON-BACKRANK)
        (values t nil))))

(defun board-black-pawns-backrank-validation (board-state)
  (let* ((black-pieces (get-pieces (fset:@ board-state :board) +black+))
         (black-pawns (remove-if-not (lambda (x) (eq (first x) +black-pawn+)) black-pieces))
         (black-pawn-indices (map 'list #'second black-pawns)))
    (if (some (lambda (x) (= 1 (index->rank x))) black-pawn-indices)
        (values nil :BLACK-PAWN-ON-BACKRANK)
        (values t nil))))

(defun board-white-kingside-castle-validation (board-state)
  (if (and (fset:@ board-state :white-kingside-castle)
           (or (not (eq (fset:@ (fset:@ board-state :board) 95) +white-king+))
               (not (eq (fset:@ (fset:@ board-state :board) 98) +white-rook+))))
      (values nil :INVALID-WHITE-KINGSIDE-CASTLE-RIGHT)
      (values t nil)))

(defun board-white-queenside-castle-validation (board-state)
  (if (and (fset:@ board-state :white-queenside-castle)
           (or (not (eq (fset:@ (fset:@ board-state :board) 95) +white-king+))
               (not (eq (fset:@ (fset:@ board-state :board) 91) +white-rook+))))
      (values nil :INVALID-WHITE-QUEENSIDE-CASTLE-RIGHT)
      (values t nil)))

(defun board-black-kingside-castle-validation (board-state)
  (if (and (fset:@ board-state :black-kingside-castle)
           (or (not (eq (fset:@ (fset:@ board-state :board) 25) +black-king+))
               (not (eq (fset:@ (fset:@ board-state :board) 28) +black-rook+))))
      (values nil :INVALID-BLACK-KINGSIDE-CASTLE-RIGHT)
      (values t nil)))

(defun board-black-queenside-castle-validation (board-state)
  (if (and (fset:@ board-state :black-queenside-castle)
           (or (not (eq (fset:@ (fset:@ board-state :board) 25) +black-king+))
               (not (eq (fset:@ (fset:@ board-state :board) 21) +black-rook+))))
      (values nil :INVALID-BLACK-QUEENSIDE-CASTLE-RIGHT)
      (values t nil)))

(defun board-en-passant-square-validation (board-state)
  (let* ((ep-index (fset:@ board-state :en-passant-index))
         (ep-rank (when ep-index (index->rank ep-index)))
         (board (fset:@ board-state :board))
         (invalid-3-ep-rank (and (eq 3 ep-rank)
                                 (or
                                  (not (empty? board ep-index))
                                  (not (empty? board (+ +south+ ep-index)))
                                  (not (eq (fset:@ board (+ +north+ ep-index)) +white-pawn+)))))
         (invalid-6-ep-rank (and (eq 6 ep-rank)
                                 (or
                                  (not (empty? board ep-index))
                                  (not (empty? board (+ +north+ ep-index)))
                                  (not (eq (fset:@ board (+ +south+ ep-index)) +black-pawn+))))))
    (if (and ep-index
             (or
              (not (or (eq 3 ep-rank)
                       (eq 6 ep-rank)))
              invalid-3-ep-rank
              invalid-6-ep-rank))
        (values nil :INVALID-EN-PASSANT-SQUARE)
        (values t nil))))

(defun board-opposite-check-validation (board-state)
  (if (in-check? (fset:with board-state :turn (enemy-color (fset:@ board-state :turn))))
      (values nil :OPPOSITE-CHECK)
      (values t nil)))

(defun board-checkers-validation (board-state)
  (let* ((color (fset:@ board-state :turn))
         (king-piece (if (eq color +white+) +white-king+ +black-king+))
         (king-index (fset:position king-piece (fset:@ board-state :board)))
         (enemy-piece->moves (generate-enemy-piece->moves board-state))
         (checker-piece->moves (fset:filter (lambda (_ v) (declare (ignore _)) (member king-index (map 'list #'second v))) enemy-piece->moves))
         (checkers (fset:reduce (lambda (acc k _) (declare (ignore _)) (cons (first k) acc)) checker-piece->moves :initial-value '()))
         (sorted-checkers (sort checkers #'eq))
         (pawn (if (eq +white+ color) +white-pawn+ +black-pawn+))
         (bishop (if (eq +white+ color) +white-bishop+ +black-bishop+))
         (knight (if (eq +white+ color) +white-knight+ +black-knight+))
         (invalid-checkers? (or (> (length sorted-checkers) 2)
                                (and (= (length sorted-checkers) 2)
                                     (member sorted-checkers (list (list pawn pawn)
                                                                   (list bishop pawn)
                                                                   (list knight pawn)
                                                                   (list bishop bishop)
                                                                   (list knight knight)))))))
    (if invalid-checkers?
        (values nil :INVALID-CHECKERS)
        (values t nil))))

(defun valid-board-state? (board-state)
  (let* ((validation-fns (list #'board-empty-validation
                               #'board-white-king-validation
                               #'board-black-king-validation
                               #'board-white-pieces-validation
                               #'board-black-pieces-validation
                               #'board-white-pawns-validation
                               #'board-black-pawns-validation
                               #'board-white-pawns-backrank-validation
                               #'board-black-pawns-backrank-validation
                               #'board-white-kingside-castle-validation
                               #'board-white-queenside-castle-validation
                               #'board-black-kingside-castle-validation
                               #'board-black-queenside-castle-validation
                               #'board-en-passant-square-validation
                               #'board-opposite-check-validation
                               #'board-checkers-validation
                               ;; TODO Maybe implement these:
                               ;; #'board-kings-adjacent-validation ;Make sure kings are separated 1 square apart.
                               ;; #'board-promoted-pieces-validation ;Make there aren't more promoted pieces that missing pawns. not sure how to do this?
                               ;; #'board-pawn-formation-validation ;Make sure if pawns are doubled there are enough enemy pieces missing.
                               ;; see https://chess.stackexchange.com/questions/1482/how-do-you-know-when-a-fen-position-is-legal for a long list of maybe's.
                               ))
         (result (reduce
                  (lambda (acc validation-fn)
                    (multiple-value-bind (valid? reason)
                        (funcall validation-fn board-state)
                      (arrows:-> acc
                                 (fset:with :valid? (and valid? (fset:@ acc :valid?)))
                                 (fset:with :reasons (fset:with (fset:@ acc :reasons) reason)))))
                  validation-fns
                  :initial-value (fset:map (:valid? t) (:reasons (fset:empty-set))))))
    (values (fset:@ result :valid?) (fset:less (fset:@ result :reasons) nil))))

(defun fen->board-state (fen)
  (ppcre:register-groups-bind
      (board-str turn-str castling-rights-str ep-square-str (#'parse-integer num-half-moves num-full-moves))
      ("^([rnbqkpRNBQKP12345678\/]+) ([wb]) (K?Q?k?q?|-) ([a-h][36]|-) ([0-9]+) ([0-9]+)$" fen :sharedp t)
    (let* ((board (reduce
                   (lambda (acc c)
                     (let ((new-cs (case c
                                     ((#\r #\n #\b #\q #\k #\p #\R #\N #\B #\Q #\K #\P) (fset:seq c))
                                     ((#\/) (fset:seq #\newline #\space))
                                     ((#\1) (fset:seq #\.))
                                     ((#\2) (fset:seq #\. #\.))
                                     ((#\3) (fset:seq #\. #\. #\.))
                                     ((#\4) (fset:seq #\. #\. #\. #\.))
                                     ((#\5) (fset:seq #\. #\. #\. #\. #\.))
                                     ((#\6) (fset:seq #\. #\. #\. #\. #\. #\.))
                                     ((#\7) (fset:seq #\. #\. #\. #\. #\. #\. #\.))
                                     ((#\8) (fset:seq #\. #\. #\. #\. #\. #\. #\. #\.)))))
                       (fset:concat acc new-cs)))
                   board-str
                   :initial-value (fset:empty-seq)))
           (board (fset:concat (fset:convert 'fset:seq (format nil "         ~%         ~% "))
                               board
                               (fset:convert 'fset:seq (format nil "~%         ~%         ~%")))))
      (fset:map (:board board)
                (:turn (cond
                         ((string= turn-str "w") +white+)
                         ((string= turn-str "b") +black+)))
                (:uci-moves (fset:empty-seq))
                (:num-half-moves num-half-moves)
                (:num-full-moves num-full-moves)
                (:white-kingside-castle (find #\K castling-rights-str :test #'equal))
                (:white-queenside-castle (find #\Q castling-rights-str :test #'equal))
                (:black-kingside-castle (find #\k castling-rights-str :test #'equal))
                (:black-queenside-castle (find #\q castling-rights-str :test #'equal))
                (:en-passant-index (if (string= "-" ep-square-str)
                                       nil
                                       (square->index ep-square-str)))
                (:previous-board-states (fset:empty-seq))))))

(defun board-state->fen (board-state)
  (let* ((board (fset:@ board-state :board))
         (board-ranks (loop for n upto 7
                            for start-index = (* 10 (+ n 2))
                            for rank = (fset:subseq board
                                                    (+ start-index 1)
                                                    (+ start-index 9))
                            for fen-rank = (fset:reduce
                                            (lambda (acc x)
                                              (if (eq +empty-square+ x)
                                                  (let ((last-item (fset:last acc)))
                                                    (if (numberp last-item)
                                                        (fset:with acc (- (fset:size acc) 1) (+ 1 last-item))
                                                        (fset:with acc (fset:size acc) 1)))
                                                  (fset:with acc (fset:size acc) x)))
                                            rank
                                            :initial-value (fset:empty-seq))
                            for rank-list = (fset:convert 'list fen-rank)
                            for rank-string = (format nil "~{~A~}" rank-list)
                            collect rank-string))
         (board-string (format nil "~{~A~^/~}" board-ranks))
         (turn-string (if (eq +white+ (fset:@ board-state :turn)) "w" "b"))
         (white-kingside-castle-string (if (fset:@ board-state :white-kingside-castle) "K" ""))
         (white-queenside-castle-string (if (fset:@ board-state :white-queenside-castle) "Q" ""))
         (black-kingside-castle-string (if (fset:@ board-state :black-kingside-castle) "k" ""))
         (black-queenside-castle-string (if (fset:@ board-state :black-queenside-castle) "q" ""))
         (no-castling-string (if (and (str:empty? white-kingside-castle-string)
                                      (str:empty? white-queenside-castle-string)
                                      (str:empty? black-kingside-castle-string)
                                      (str:empty? black-queenside-castle-string))
                                 "-"
                                 ""))
         (ep-index (fset:@ board-state :en-passant-index))
         (ep-square-string (if ep-index (index->square ep-index) "-"))
         (half-moves (fset:@ board-state :num-half-moves))
         (full-moves (fset:@ board-state :num-full-moves)))
    (format nil "~a ~a ~a~a~a~a~a ~a ~a ~a"
            board-string
            turn-string
            white-kingside-castle-string
            white-queenside-castle-string
            black-kingside-castle-string
            black-queenside-castle-string
            no-castling-string
            ep-square-string
            half-moves
            full-moves)))

(defun pgn->board-state (pgn)
  (let ((games (str:split (format nil "~%~%") pgn)))
    (if (> (length games) 2)
        :muliple-games-not-supported
        (let* ((headers (str:lines (first games)))
               (parsed-headers (reduce
                                (lambda (acc header)
                                  (ppcre:register-groups-bind
                                      (header-name header-value)
                                      ("^\\[(\\w+) \\\"([^\\]\\\"]+)\\\"\\]$" header :sharedp t)
                                    (fset:with-last acc (list header-name header-value))))
                                headers
                                :initial-value (fset:empty-seq)))
               (moves (str:split " " (second games)))
               (parsed-moves (loop for index from 0 to (max 0 (- (length moves) 2))
                                   for move in moves
                                   when (not (= 0 (mod index 3)))
                                     collect move)))
          (push-san-moves (make-board-state parsed-headers) parsed-moves)))))

(defun board-state->pgn (board-state)
  (let* ((headers (fset:@ board-state :headers))
         (pgn-headers (str:join (format nil "~%")
                                (fset:convert 'list
                                              (fset:image
                                               (lambda (header)
                                                 (format nil "[~a \"~a\"]" (first header) (second header)))
                                               headers))))
         (san-moves (fset:@ board-state :san-moves))
         (pgn-moves (loop for i from 0 to (floor (/ (fset:size san-moves) 2))
                          for white-move = (or (fset:@ san-moves (* 2 i)) "")
                          for black-move = (or (fset:@ san-moves (+ (* 2 i) 1)) "")
                          append (list (format nil "~a." (+ 1 i)) white-move black-move)))
         (game-outcome (outcome board-state t))
         (outcome-str (case game-outcome
                        (:white-checkmate "1-0")
                        (:black-checkmate "0-1")
                        ((:insufficient-material
                          :stalemate
                          :fifty-moves-draw
                          :seventy-five-moves-draw
                          :threefold-repition-draw
                          :fivefold-repition-draw) "0.5-0.5")
                        (otherwise "*")))
         (pgn-moves-str (str:join " " pgn-moves)))
    (format nil "~a~%~%~a~a~%" pgn-headers pgn-moves-str outcome-str)))

(defun is-seventy-five-moves-draw? (board-state &optional (legal-moves (generate-legal-moves board-state)))
  (and (not (null legal-moves))
       (>= (fset:@ board-state :num-half-moves) 150)))

(defun is-repition? (n board-state)
  (let* ((board-frequencies (fset:reduce
                             (lambda (acc bs)
                               (let ((board (fset:@ bs :board)))
                                 (if (multiple-value-bind (_ in-acc?) (fset:@ acc board) (declare (ignore _)) in-acc?)
                                     (fset:with acc board (+ 1 (fset:@ acc board)))
                                     (fset:with acc board 1))))
                             (fset:@ board-state :previous-board-states)
                             :initial-value (fset:map ((fset:@ board-state :board) 1))))
         (board-frequencies-iter (fset:iterator board-frequencies)))
    (loop for k = (funcall board-frequencies-iter :get)
          until (null k)
          for v = (fset:@ board-frequencies k)
          for draw? = (>= v n)
          until draw?
          finally (return draw?))))

(defun is-fivefold-repition-draw? (board-state &optional (legal-moves (generate-legal-moves board-state)))
  (and (not (null legal-moves))
       (is-repition? 5 board-state)))

(defun is-fifty-moves-draw? (board-state claim-draw &optional (legal-moves (generate-legal-moves board-state)))
  (and claim-draw
       (not (null legal-moves))
       (>= (fset:@ board-state :num-half-moves) 100)))

(defun is-threefold-repition-draw? (board-state claim-draw &optional (legal-moves (generate-legal-moves board-state)))
  (and claim-draw
       (not (null legal-moves))
       (is-repition? 3 board-state)))

(defun is-draw? (board-state claim-draw)
  (or (is-seventy-five-moves-draw? board-state)
      (is-fivefold-repition-draw? board-state)
      (and claim-draw
           (or (is-fifty-moves-draw? board-state claim-draw)
               (is-threefold-repition-draw? board-state claim-draw)))))

(defun is-insufficient-material? (board-state)
  (let* ((board (fset:@ board-state :board))
         (sorted-white-pieces (sort (get-pieces board +white+) #'eq))
         (sorted-black-pieces (sort (get-pieces board +black+) #'eq)))
    (or (and (= 1 (length sorted-white-pieces))
             (eq +white-king+ (first sorted-white-pieces))
             (= 1 (length sorted-black-pieces))
             (eq +black-king+ (first sorted-black-pieces)))
        (and (= 1 (length sorted-white-pieces))
             (eq +white-king+ (first sorted-white-pieces))
             (member (length sorted-black-pieces) (list 2 3))
             (or (eq (list +black-bishop+ +black-king+) sorted-black-pieces)
                 (eq (list +black-king+ +black-knight+) sorted-black-pieces)
                 (eq (list +black-king+ +black-knight+ +black-knight+) sorted-black-pieces)))
        (and (= 1 (length sorted-black-pieces))
             (eq +black-king+ (first sorted-black-pieces))
             (member (length sorted-white-pieces) (list 2 3))
             (or (fset:equal? (fset:set +white-bishop+ +white-king+) sorted-white-pieces)
                 (fset:equal? (fset:set +white-king+ +white-knight+) sorted-white-pieces)
                 (fset:equal? (fset:set +white-king+ +white-knight+ +white-knight+) sorted-white-pieces))))))

(defun in-check? (board-state &optional (enemy-psuedo-legal-moves (generate-psuedo-legal-moves-from-board-state board-state)))
  (let* ((color (fset:@ board-state :turn))
         (board (fset:@ board-state :board))
         (attacked-indices (arrows:->> enemy-psuedo-legal-moves
                                       (map 'list #'second)
                                       (fset:convert 'fset:set)))
         (king-piece (if (eq color +white+) +white-king+ +black-king+))
         (king-index (fset:position king-piece board)))
    (fset:contains? attacked-indices king-index)))

(defun is-stalemate? (board-state &optional
                                    (legal-moves (generate-legal-moves board-state))
                                    (enemy-psuedo-legal-moves (generate-psuedo-legal-moves-from-board-state board-state)))
  (and (null legal-moves)
       (not (in-check? board-state enemy-psuedo-legal-moves))))

(defun is-checkmate? (board-state &optional
                                    (legal-moves (generate-legal-moves board-state))
                                    (enemy-psuedo-legal-moves (generate-psuedo-legal-moves-from-board-state board-state)))
  (and (null legal-moves)
       (in-check? board-state enemy-psuedo-legal-moves)))

(defun outcome (board-state claim-draw &optional
                                         (legal-moves (generate-legal-moves board-state))
                                         (enemy-psuedo-legal-moves (generate-psuedo-legal-moves-from-board-state board-state)))
  (cond
    ((is-checkmate? board-state legal-moves enemy-psuedo-legal-moves) (if (eq +white+ (fset:@ board-state :turn))
                                                                          :black-checkmate
                                                                          :white-checkmate))
    ((is-insufficient-material? board-state) :insufficient-material)
    ((is-stalemate? board-state legal-moves enemy-psuedo-legal-moves) :stalemate)
    ((is-seventy-five-moves-draw? board-state legal-moves) :seventy-five-moves-draw)
    ((is-fivefold-repition-draw? board-state legal-moves) :fivefold-repition-draw)
    ((is-fifty-moves-draw? board-state claim-draw legal-moves) :fifty-moves-draw)
    ((is-threefold-repition-draw? board-state claim-draw legal-moves) :threefold-repition-draw)))

(defun is-game-over? (board-state claim-draw)
  (or (is-checkmate? board-state)
      (is-insufficient-material? board-state)
      (is-stalemate? board-state)
      (is-draw? board-state claim-draw)))

(defun get-move (board-state)
  (let* ((prompt (lambda (s)
                   (format t "~&~a: " s)
                   (finish-output)
                   (read-line))))
    (loop for move = (funcall prompt "Please enter a valid move")
          for uci-move-name = (san->uci-move-name board-state move)
          until uci-move-name
          finally (return move))))

(defun play-game ()
  (loop for board-state = (make-board-state) then (push-san-move board-state move)
        for pb = (print-board (fset:@ board-state :board))
        for enemy-piece->moves = (generate-enemy-piece->moves board-state)
        for legal-moves = (generate-legal-moves board-state enemy-piece->moves)
        for color = (fset:@ board-state :turn)
        for en-passant-index = (fset:@ board-state :en-passant-index)
        for board = (fset:@ board-state :board)
        for enemy-psuedo-legal-moves = (generate-psuedo-legal-moves board (enemy-color color) en-passant-index :include-attacking? t)
        for outcome = (outcome board-state t legal-moves)
        until (not (null outcome))
        for move = (get-move board-state)
        finally (format t "~a~%" outcome)))

;; TODO
;; DONE Lookup legal moves and enemy attacked squares only once in turn.
;; DONE The previous refactor means there is a lot of duplicate code. I think one solution is to create the same fns that accept board state instead of all things
;; DONE FEN import and export
;; DONE SAN moves
;; PGN import/export
;; UI
;; AI
