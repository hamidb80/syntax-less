(defn ast-data-type [data]
  (let [t (type data)]
    (match t
      :nil      :nil
      :boolean  :boolean
      :number   :number
      
      :string   :string
      :symbol   :symbol
      :keyword  :keyword

      :array    :vector
      :tuple    :vector
      
      :table    :dict
      :struct   :dict
      
      (error (string "it is impossible for a AST node to be a " t))
    )))


(defn visualize-impl [cfg code acc]

  )

(defn visualize ``
  produces HTML output from Lisp code
  - cfg:: fn-symbol -> fn(expr): HTML
  ``
  [cfg code]

  (pp code))

# USAGE -----------------------------------

(visualize {} 
  '("init"))


# (print "init") -> :vector
# print          -> :symbol
# "init"         -> :string
