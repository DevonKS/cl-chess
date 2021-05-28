(in-package :chess)

(defun make-board ()
  (fset:convert
   'fset:seq
   (format
    nil
    "         ~%         ~% rnbqkbnr~% pppppppp~% ........~% ........~% ........~% ........~% PPPPPPPP~% RNBQKBNR~%         ~%         ~%")))

(defun make-board-state ()
  (fset:map (:board (make-board))
            (:turn WHITE)
            (:moves (fset:empty-seq))
            (:num-half-moves 0)
            (:num-full-moves 0)
            (:white-kingside-castle t)
            (:white-queenside-castle t)
            (:black-kingside-castle t)
            (:black-queenside-castle t)
            (:en-passant-index nil)
            (:previous-board-states (fset:empty-seq))))

(defun print-board (board)
  (dotimes (n 8)
    (let* ((start-index (* 10 (+ n 2)))
           (rank (fset:subseq board
                              (+ start-index 1)
                              (+ start-index 9)))
           (rank-list (fset:convert 'list rank)))
      (format t "~{~a~^ ~}~%" rank-list))))

(defun row-num (index)
  (when (<= 20 index 99)
    (- (floor index 10) 2)))

(defun col-num (index)
  (when (<= 20 index 99)
    (rem index 10)))

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
  (let* ((row-num (row-num index))
         (col-num (col-num index))
         (file (case col-num
                 (1 #\a)
                 (2 #\b)
                 (3 #\c)
                 (4 #\d)
                 (5 #\e)
                 (6 #\f)
                 (7 #\g)
                 (8 #\h)))
         (rank (- 8 row-num)))
    (format nil "~a~a" file rank)))

(defun move->uci-move-name (move)
  (str:concat (index->square (first move)) (index->square (second move)) (third move)))

(defun piece-belongs-to-color (piece color)
  (member piece (if (eq WHITE color)
                    WHITE-PIECES
                    BLACK-PIECES)))

(defun enemy-color (color)
  (if (eq WHITE color)
      BLACK
      WHITE))

(defun enemy-pieces (color)
  (if (eq WHITE color)
      BLACK-PIECES
      WHITE-PIECES))

(defun enemy-piece? (color piece)
  (piece-belongs-to-color piece (enemy-color color)))

(defun is-rook? (piece)
  (or (eq piece WHITE-ROOK)
      (eq piece BLACK-ROOK)))

(defun is-knight? (piece)
  (or (eq piece WHITE-KNIGHT)
      (eq piece BLACK-KNIGHT)))

(defun is-bishop? (piece)
  (or (eq piece WHITE-BISHOP)
      (eq piece BLACK-BISHOP)))

(defun is-queen? (piece)
  (or (eq piece WHITE-QUEEN)
      (eq piece BLACK-QUEEN)))

(defun is-king? (piece)
  (or (eq piece WHITE-KING)
      (eq piece BLACK-KING)))

(defun is-pawn? (piece)
  (or (eq piece WHITE-PAWN)
      (eq piece BLACK-PAWN)))

(defun same-rank? (source-index dest-index)
  (= (row-num source-index) (row-num dest-index)))

(defun same-file? (source-index dest-index)
  (= (col-num source-index) (col-num dest-index)))

(defun same-diagonal? (source-index dest-index)
  (let ((source-row-num (row-num source-index))
        (source-col-num (col-num source-index))
        (dest-row-num (row-num dest-index))
        (dest-col-num (col-num dest-index)))
    (= (abs (- dest-row-num source-row-num))
       (abs (- dest-col-num source-col-num)))))

(defun empty? (board index)
  (eq EMPTY-SQUARE (fset:@ board index)))

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
             (list WHITE-ROOK WHITE-BISHOP WHITE-QUEEN
                   BLACK-ROOK BLACK-BISHOP BLACK-QUEEN)))
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
          (fset:@ PIECE-DIRECTIONS ROOK))))

(defun generate-knight-moves (color board index &key (include-attacking? nil))
  (validate-moves
   color
   board
   (map 'list
        (lambda (index-dir) (+ index index-dir))
        (fset:@ PIECE-DIRECTIONS KNIGHT))
   :include-attacking? include-attacking?))

(defun generate-bishop-moves (color board index &key (include-attacking? nil))
  (apply #'append
         (map
          'list
          (lambda (step) (generate-moves-in-step color board index step :include-attacking? include-attacking?))
          (fset:@ PIECE-DIRECTIONS BISHOP))))

(defun generate-queen-moves (color board index &key (include-attacking? nil))
  (append (generate-rook-moves color board index :include-attacking? include-attacking?)
          (generate-bishop-moves color board index :include-attacking? include-attacking?)))


(defun generate-king-moves (color board index &key (include-attacking? nil))
  (validate-moves
   color
   board
   (map 'list
        (lambda (index-dir) (+ index index-dir))
        (fset:@ PIECE-DIRECTIONS KING))
   :include-attacking? include-attacking?))

(defun generate-pawn-moves (en-passant-index color board index &key (include-attacking? nil))
  (let* ((white? (eq WHITE color))
         (promotion? (= (row-num index)
                        (if white? 1 6)))
         (pawn-piece (if white? WHITE-PAWN BLACK-PAWN))
         (move-indices (map 'list
                            (lambda (index-dir) (+ index index-dir))
                            (fset:@ PIECE-DIRECTIONS pawn-piece)))
         (moves (remove-if
                 (lambda (dest-index)
                   (let* ((double-move (= (+ SOUTH SOUTH) (abs (- dest-index index))))
                          (single-move (= SOUTH (abs (- dest-index index))))
                          (diagonal-move (not (or double-move
                                                  single-move))))
                     (or (not (in-board? board dest-index))
                         (and (not include-attacking?)
                              (piece-belongs-to-color (fset:@ board dest-index) color))
                         (and (or double-move single-move)
                              (not (empty? board dest-index)))
                         (and double-move
                              (not (= (row-num index)
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

(defun find-pinned-pieces (board color)
  (let ((enemy-pinning-pieces (get-pinning-pieces board (enemy-color color)))
        (king-index (fset:position (if (eq color WHITE) WHITE-KING BLACK-KING) board)))
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
                (let* ((source-col-num (col-num index))
                       (dest-col-num (col-num king-index))
                       (backwards-move? (< king-index index))
                       (backwards-diagonal? (> source-col-num dest-col-num))
                       ;; Note steps of this form only work because we know the piece is in the same file/rank/diagonal as the king
                       ;; Otherwise they could result in out of the board errors
                       ;; (the index will still be in the board but the path would have travelled outside the board).
                       (step (cond
                               ((and same-rank? backwards-move?) WEST)
                               ((and same-rank? (not backwards-move?)) EAST)
                               ((and same-file? backwards-move?) NORTH)
                               ((and same-file? (not backwards-move?)) SOUTH)
                               ((and same-diagonal? backwards-move? (not backwards-diagonal?)) (+ NORTH EAST))
                               ((and same-diagonal? backwards-move? backwards-diagonal?) (+ NORTH WEST))
                               ((and same-diagonal? (not backwards-move?) backwards-diagonal?) (+ SOUTH WEST))
                               ((and same-diagonal? (not backwards-move?) (not backwards-diagonal?)) (+ SOUTH EAST))))
                       (indices (range (+ index step) king-index :step step))
                       (pieces (arrows:->>  indices
                                            (map 'list (lambda (index) (list (fset:@ board index) index)))
                                            (remove-if (lambda (piece-index) (eq (first piece-index) EMPTY-SQUARE))))))
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

(defun generate-castling-moves (board color castling-rights attacked-indices)
  (arrows:cond-> '()
                 ((and (eq WHITE color)
                       (fset:@ castling-rights :white-queenside-castle)
                       (every (lambda (index)
                                (and
                                 (empty? board index)
                                 (not (fset:contains? attacked-indices index))))
                              (list 92 93 94)))
                  (append (list (list 95 93))))

                 ((and (eq WHITE color)
                       (fset:@ castling-rights :white-kingside-castle)
                       (every (lambda (index)
                                (and
                                 (empty? board index)
                                 (not (fset:contains? attacked-indices index))))
                              (list 96 97)))
                  (append (list (list 95 97))))

                 ((and (eq BLACK color)
                       (fset:@ castling-rights :black-queenside-castle)
                       (every (lambda (index)
                                (and
                                 (empty? board index)
                                 (not (fset:contains? attacked-indices index))))
                              (list 22 23 24)))
                  (append (list (list 25 23))))

                 ((and (eq BLACK color)
                       (fset:@ castling-rights :black-kingside-castle)
                       (every (lambda (index)
                                (and
                                 (empty? board index)
                                 (not (fset:contains? attacked-indices index))))
                              (list 25 27)))
                  (append (list (list 26 27))))))

(defun generate-legal-moves (board-state)
  (let* ((color (fset:@ board-state :turn))
         (board (fset:@ board-state :board))
         (en-passant-index (fset:@ board-state :en-passant-index))
         (pinned-pieces (find-pinned-pieces board color))
         (enemy-piece->moves (reduce
                              (lambda (acc piece)
                                (setf (gethash piece acc) (generate-psuedo-legal-moves board (enemy-color color) en-passant-index :pieces (list piece)))
                                acc)
                              (get-pieces board (enemy-color color))
                              :initial-value (make-hash-table)))
         (attacked-indices (arrows:->> (generate-psuedo-legal-moves board (enemy-color color) en-passant-index :include-attacking? t)
                                       (map 'list #'second)
                                       (fset:convert 'fset:set)))
         (king-piece (if (eq color WHITE) WHITE-KING BLACK-KING))
         (king-index (fset:position king-piece board))
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
                                                   (eq (first x) king)))
                                             (get-pieces board color))))
         (other-legal-moves (if in-check?
                                (remove-if-not
                                 (lambda (move)
                                   (every
                                    (lambda (indices)
                                      (fset:contains? indices (second move)))
                                    (arrows:->> enemy-piece->moves
                                                alexandria:hash-table-values
                                                (map 'list
                                                     (lambda (moves)
                                                       (fset:convert 'fset:set (alexandria:flatten moves)))))))
                                 other-psuedo-legal-moves)
                                other-psuedo-legal-moves)))
    (append legal-king-moves king-castling-moves other-legal-moves)))

(defun is-legal-move? (board-state move)
  (member move (map 'list #'move->uci-move-name (generate-legal-moves board-state)) :test #'equal))

(defun push-move (board-state move)
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
                               ("q" (if (eq color WHITE) WHITE-QUEEN BLACK-QUEEN))
                               ("b" (if (eq color WHITE) WHITE-BISHOP BLACK-BISHOP))
                               ("n" (if (eq color WHITE) WHITE-KNIGHT BLACK-KNIGHT))
                               ("r" (if (eq color WHITE) WHITE-ROOK BLACK-ROOK)))))
          (new-piece (if promotion-piece promotion-piece piece))
          (new-board (arrows:-> board
                                (fset:with source-index EMPTY-SQUARE)
                                (fset:with dest-index new-piece)))
          (pawn-move? (is-pawn? piece))
          (capture? (not (empty? board dest-index)))
          (new-half-move-count (if (or pawn-move? capture?)
                                   0
                                   (+ (fset:@ board-state :num-half-moves) 1)))
          (num-full-moves (fset:@ board-state :num-full-moves))
          (new-full-move-count (if (eql BLACK color)
                                   (+ 1 num-full-moves)
                                   num-full-moves))
          (en-passant-index (if (or (and (eq WHITE color)
                                         (= 6 (row-num source-index))
                                         (= 4 (row-num dest-index)))
                                    (and (eq BLACK color)
                                         (= 1 (row-num source-index))
                                         (= 3 (row-num dest-index))))
                                (/ (+ dest-index source-index) 2)
                                nil))
          (new-moves (fset:with-last (fset:@ board-state :moves) move))
          (previous-board-states (fset:with-last (fset:@ board-state :previous-board-states) board-state))
          (white-kingside-castle (and (fset:@ board-state :white-kingside-castle)
                                      (/= source-index 95 98)))
          (white-queenside-castle (and (fset:@ board-state :white-queenside-castle)
                                       (/= source-index 95 91)))
          (black-kingside-castle (and (fset:@ board-state :black-kingside-castle)
                                      (/= source-index 25 28)))
          (black-queenside-castle (and (fset:@ board-state :black-queenside-castle)
                                       (/= source-index 25 21))))
     (progn
       (setf (fset:@ board-state :board) new-board)
       (setf (fset:@ board-state :turn) (enemy-color color))
       (setf (fset:@ board-state :num-half-moves) new-half-move-count)
       (setf (fset:@ board-state :num-full-moves) new-full-move-count)
       (setf (fset:@ board-state :moves) new-moves)
       (setf (fset:@ board-state :en-passant-index) en-passant-index)
       (setf (fset:@ board-state :previous-board-states) previous-board-states)
       (setf (fset:@ board-state :white-kingside-castle) white-kingside-castle)
       (setf (fset:@ board-state :white-queenside-castle) white-queenside-castle)
       (setf (fset:@ board-state :black-kingside-castle) black-kingside-castle)
       (setf (fset:@ board-state :black-queenside-castle) black-queenside-castle)
       board-state))))

(defun push-moves (board-state moves)
  (reduce #'push-move moves :initial-value board-state))

(defun pop-move (board-state)
  (fset:last (fset:@ board-state :previous-board-states)))

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
         (sorted-white-pieces (sort (get-pieces board WHITE) #'eq))
         (sorted-black-pieces (sort (get-pieces board BLACK) #'eq)))
    (or (and (= 1 (length sorted-white-pieces))
             (eq WHITE-KING (first sorted-white-pieces))
             (= 1 (length sorted-black-pieces))
             (eq BLACK-KING (first sorted-black-pieces)))
        (and (= 1 (length sorted-white-pieces))
             (eq WHITE-KING (first sorted-white-pieces))
             (member (length sorted-black-pieces) (list 2 3))
             (or (eq (list BLACK-BISHOP BLACK-KING) sorted-black-pieces)
                 (eq (list BLACK-KING BLACK-KNIGHT) sorted-black-pieces)
                 (eq (list BLACK-KING BLACK-KNIGHT BLACK-KNIGHT) sorted-black-pieces)))
        (and (= 1 (length sorted-black-pieces))
             (eq BLACK-KING (first sorted-black-pieces))
             (member (length sorted-white-pieces) (list 2 3))
             (or (fset:equal? (fset:set WHITE-BISHOP WHITE-KING) sorted-white-pieces)
                 (fset:equal? (fset:set WHITE-KING WHITE-KNIGHT) sorted-white-pieces)
                 (fset:equal? (fset:set WHITE-KING WHITE-KNIGHT WHITE-KNIGHT) sorted-white-pieces))))))

;; FIXME pass-in enemy-moves for perf??
(defun in-check? (board-state)
  (let* ((color (fset:@ board-state :turn))
         (board (fset:@ board-state :board))
         (en-passant-index (fset:@ board-state :en-passant-index))
         (enemy-psuedo-legal-moves (generate-psuedo-legal-moves board (enemy-color color) en-passant-index))
         (attacked-indices (arrows:->> enemy-psuedo-legal-moves
                                       (map 'list #'second)
                                       (fset:convert 'fset:set)))
         (king-piece (if (eq color WHITE) WHITE-KING BLACK-KING))
         (king-index (fset:position king-piece board)))
    (fset:contains? attacked-indices king-index)))

(defun is-stalemate? (board-state &optional (legal-moves (generate-legal-moves board-state)))
  (and (null legal-moves)
       (not (in-check? board-state))))

(defun is-checkmate? (board-state &optional (legal-moves (generate-legal-moves board-state)))
  (and (null legal-moves)
       (in-check? board-state)))

(defun outcome (board-state claim-draw)
  (cond
    ((is-checkmate? board-state) (list (enemy-color (fset:@ board-state :turn)) :checkmate))
    ((is-insufficient-material? board-state) :insufficient-material)
    ((is-stalemate? board-state) :stalemate)
    ((is-seventy-five-moves-draw? board-state) :seventy-five-moves-draw)
    ((is-fivefold-repition-draw? board-state) :fivefold-repition-draw)
    ((is-fifty-moves-draw? board-state claim-draw) :fifty-moves-draw)
    ((is-threefold-repition-draw? board-state claim-draw) :threefold-repition-draw)))

(defun is-game-over? (board-state claim-draw)
  (or (is-checkmate? board-state)
      (is-insufficient-material? board-state)
      (is-stalemate? board-state)
      (is-draw? board-state claim-draw)))

(defun get-move (board-state)
  (let* ((legal-moves (generate-legal-moves board-state))
         (legal-move-names (map 'list #'move->uci-move-name legal-moves))
         (prompt (lambda (s)
                   (format t "~&~a: " s)
                   (finish-output)
                   (read-line))))
    (loop for move = (funcall prompt "Please enter a valid move")
          until (member move legal-move-names :test #'string=)
          finally (return move))))

(defun play-game ()
  (loop for board-state = (make-board-state) then (push-move board-state move)
        for pb = (print-board (fset:@ board-state :board))
        for outcome = (outcome board-state t)
        until (not (null outcome))
        for move = (get-move board-state)
        finally (format t "~a~%" outcome)))

;; TODO
;; Lookup legal moves and enemy attacked squares only once in turn.
;; FEN import and export
;; SAN moves
;; AI
