(ns toy-counting-game.core)

(defn read_move [] (do
  (print "Next move? ")
  (flush)
  (try
    (Integer/parseInt (read-line))
    (catch Exception e 0)
  )
))

(defn is_higher [next current]
  (> next current)
)

(defn is_within_range [next current range]
  (<= (- next current) range)
)

(defn is_within_limit [next limit]
  (<= next limit)
)

(defn is_valid_move [next current limit range]
  (and
    (is_higher next current)
    (is_within_range next current range)
    (is_within_limit next limit)
  )
)

(defn optimal_moves [limit range]
  (take-while
    pos?
    (iterate (fn [x] (- x range 1)) (- limit 1))
  )
)

(defn all_moves [limit]
  (take-while
    (fn [x] (<= x limit))
    (iterate inc 0)
  )
)

(defn winning_strategy [current limit range]
  (first (filter
    (fn [x] (is_valid_move x current limit range))
    (concat
      (optimal_moves limit range)
      (all_moves limit)
    )
  ))
)

(defn generate_move [current limit range]
  (let [next (winning_strategy current limit range)]
    (do
      (println (str "I choose... " next))
      (identity next)
    )
  )
)

(def turn (atom true))

(declare play_game)
(defn make_move [next current limit range]
  (if (is_valid_move next current limit range)
    (do
      (swap! turn not)
      (play_game next limit range)
    )
    (do
      (.println *err* "Not a valid move.")
      (play_game current limit range)
    )
  )
)

(defn move [current limit range]
  (if @turn
    (make_move (read_move) current limit range)
    (make_move (generate_move current limit range) current limit range)
  )
)

(defn play_game [current limit range]
  (if (== current limit)
    (do
      (if @turn
        (println "You win.")
        (println "You lose.")
      )
      (System/exit 0)
    )
    (do
      (println (str "Current value: " current))
      (move current limit range)
    )
  )
)

(defn display_help []
  (do
    (println "Usage: toy_counting_game LIMIT RANGE PLAYER")
    (println "   LIMIT  - how high to play the game to")
    (println "   RANGE  - how many numbers above the current value are valid")
    (println "   PLAYER - Who goes first? '1' for human or '2' for CPU")
    (System/exit -1)
  )
)

(defn parse_args [args]
  (try
    (doall (map
      (fn [x]
        (let [result (Integer/parseInt x)]
          (if (> result 0)
            result
            (throw (IllegalArgumentException. (str "Argument must be > 0 but got " result)))
          )
        )
      )
      args
    ))
    (catch Exception e
      (do
        (println (.getMessage e))
        (display_help)
      )
    )
  )
)


(defn -main [& args]
  (if (== (count args) 3)
    (let [[limit range player] (parse_args args)]
      (reset! turn (== 1 player))
      (play_game 0 limit range)
    )
    (display_help)
  )
)
