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

(defn -main [& args]
  (play_game 0 21 3)
)
