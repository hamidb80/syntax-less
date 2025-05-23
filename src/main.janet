# UTILS ----------------------------------------------------

(defn- flat-string-impl (lst acc)
  (each s lst
    (let [t (type/reduced s)]
      (match 
        :string  (buffer/push acc s)
        :tuple   (flat-string-impl s acc)
                (error (string `invalid type ` t))))))

(defn  flat-string (& args) # args is nested list of string
  (let 
    [acc @""] 
    (flat-string-impl args acc)
    acc))

(defn ast-data-type [node]
  (let [t (type node)]
    (match t
      :nil      :nil
      :boolean  :boolean
      :number   :number
      
      :string   :string
      :symbol   :symbol
      :keyword  :keyword

      :array    :array
      :tuple    (match (tuple/type node)
                  :parens   :call
                  :brackets :brackets)
      
      :table    :table
      :struct   :struct
      
      (error (string "it is impossible for a AST node to be a " t))
    )))

# IMPL -----------------------------------------------------

(defn visualize-impl [cfg code acc]
  )

(defn visualize ``
  produces HTML output from Lisp code
  - cfg:: fn-symbol -> fn(expr): HTML
  ``
  [cfg code]

  (let 
    [acc @[]] 
    (visualize-impl cfg code acc)
    acc))

# USAGE -----------------------------------

(visualize {} 
  '("init"))


# (print "init") -> :vector
# print          -> :symbol
# "init"         -> :string
