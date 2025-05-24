# UTILS ----------------------------------------------------

(defn- flat-string-impl (lst acc)
  (each s lst
    (let [t (type s)]
      (match 
        :tuple   (flat-string-impl s acc)
        :array   (flat-string-impl s acc)
                 (buffer/push acc (string s))))))

(defn  flat-string (& args) # args is nested list of string
  (let 
    [acc @""] 
    (flat-string-impl args acc)
    acc))

(defn  ast-data-type [node]
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
      
      (error (string "it is impossible for a AST node to be " t))
    )))

# IMPL -----------------------------------------------------

(def push array/push)

(def div-el (cls content)
  [`<div ` `class="` cls `"`  `>` content `</div>`])

(defn visualize-impl [cfg lookup node acc]
  (def handle (fn [node-] (visualize-impl cfg lookup node- acc)))

  (match (ast-data-type node)
    :call     (let [callee (first node)
                    f    (lookup callee)]
                   (f cfg handle node acc))
    :symbol   (push (div-el `ast-symbol` node))
    :string   (push (div-el `ast-string` node))
    )
  )

(defn lookup/init [sym handle acc]
  (match sym
    (fn [cfg lookup node acc] (each node ))))

(defn visualize ``
  produces HTML output from Lisp code
  - lookup :: fn-symbol -> fn(cfg lookup node acc)
  ``
  [cfg lookup code]

  (let 
    [acc @[]] 
    (visualize-impl cfg lookup code acc)
    acc))

# USAGE -----------------------------------

(visualize {} {}
  '(do 
      (print "init")
      ))


# (print "init") -> :vector
# print          -> :symbol
# "init"         -> :string
