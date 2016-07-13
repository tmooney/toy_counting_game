(ns toy-counting-game.core)

(defprotocol GameProtocol
  (is_higher [this next])
  (is_within_range [this next])
  (is_within_limit [this next])
  (is_valid_move [this next])
)
(deftype Game [current limit range]
  GameProtocol
  (is_higher [this next]
    (> next current)
  )
  (is_within_range [this next]
    (<= (- next current) range)
  )
  (is_within_limit [this next]
    (<= next limit)
  )
  (is_valid_move [this next]
    (and
      (is_higher this next)
      (is_within_range this next)
      (is_within_limit this next)
    )
  )
)

(defn read_move [] (do
  (print "Next move? ")
  (flush)
  (try
    (Integer/parseInt (read-line))
    (catch Exception e 0)
  )
))

(defn optimal_moves [game]
  (take-while
    pos?
    (iterate (fn [x] (- x (.range game) 1)) (- (.limit game) 1))
  )
)

(defn all_moves [game]
  (take-while
    (fn [x] (<= x (.limit game)))
    (iterate inc 0)
  )
)

(defn winning_strategy [game]
  (first (filter
    (fn [x] (.is_valid_move game x))
    (concat
      (optimal_moves game)
      (all_moves game)
    )
  ))
)

(defn generate_move [game]
  (let [next (winning_strategy game)]
    (do
      (println (str "I choose... " next))
      (identity next)
    )
  )
)

(def turn (atom true))

(declare play_game)
(defn make_move [next game]
  (if (.is_valid_move game next)
    (do
      (swap! turn not)
      (play_game (Game. next (.limit game) (.range game)))
    )
    (do
      (.println *err* "Not a valid move.")
      (play_game game)
    )
  )
)

(defn move [game]
  (if @turn
    (make_move (read_move) game)
    (make_move (generate_move game) game)
  )
)

(defn play_game [game]
  (if (== (.current game) (.limit game))
    (do
      (if @turn
        (println "You win.")
        (println "You lose.")
      )
      (System/exit 0)
    )
    (do
      (println (str "Current value: " (.current game)))
      (move game)
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
      (play_game (Game. 0 limit range))
    )
    (display_help)
  )
)
