(require '[clojure.java.io :as io] )
(require '[clojure.string :as cstr])

;Eda Arikan - 131044050

;keyword list
(def all_keywords '("and", "or", "not", "equal", "append", 
                    "concat", "set", "deffun", "for", "while", "if",
                    "then" , "else", "true", "false", "null"))


;operators list
(def all_operators '("+", "-", "/", "*", "(", ")", "\\"))


;https://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Clojure
;is numeric check
(defn numeric? [s]
  (if-let [s (seq s)]
    (let [s (if (= (first s) \-) (next s) s)
          s (drop-while #(Character/isDigit %) s)
          s (if (= (first s) \.) (next s) s)
          s (drop-while #(Character/isDigit %) s)]
      (empty? s))))


;get nth element from list
(defn get_nth_element [list x y] 
  (nth (cstr/split (clojure.string/replace (clojure.string/replace (apply str(cstr/split (nth list x) #" "))  #"\)" " ) ")   #"\(" " ( ")  #" ") y)  
)


;get size of inner loop array element
(defn get_size [list x]
  (count (cstr/split (clojure.string/replace (clojure.string/replace (apply str(cstr/split (nth list x) #" "))  #"\)" " ) ")   #"\(" " ( ")  #" "))
)


;lexer function
;read file, split array by spaces, put space character each parentheses back and front
;split again by spaces for 2d list and check each item
(defn lexer [filename]
  ;read file to list
  (def data-file (io/resource filename))
  (def list (clojure.string/split (slurp data-file) #"\s+")) 
  
  (loop [x 0]
    (when (< x  (count list))
      (loop [y 0]
        (when (< y (get_size list x))
	           
	  (if (not= -1 (.indexOf all_operators (get_nth_element list x y)))
            (println (format "%10s" (get_nth_element list x y)) " is operator" )
            (if (not= -1 (.indexOf all_keywords (get_nth_element list x y)))
              (println (format "%10s" (get_nth_element list x y)) " is keyword" )

              (if (= "true"  (get_nth_element list x y))
                (println (format "%10s" (get_nth_element list x y)) " is binary value" )
                (if (= "false"  (get_nth_element list x y))
                  (println (format "%10s" (get_nth_element list x y)) " is binary value" )
	           
                   (if (= -1 (.indexOf all_operators (get_nth_element list x y))) 
	             (if (not= 0 (count (cstr/trim (get_nth_element list x y))))
	               (if (= true (numeric? (get_nth_element list x y)))
                          (println (format "%10s" (get_nth_element list x y)) " is integer" )
	                    (println (format "%10s" (get_nth_element list x y)) " is identifier" )
	                    )))))))
	      
       (recur (+ y 1)))) 
      (recur (+ x 1))))
)


