# UTILS ----------------------------------------------------

(defn- flat-string-impl (lst acc)
  (each s lst
    (let [t (type s)]
      (match t
        :tuple   (flat-string-impl s acc)
        :array   (flat-string-impl s acc)
        (buffer/push acc (string s))))))

(defn  flat-string (lst) # lst is nested list of string
  (let 
    [acc @""]
    (flat-string-impl lst acc)
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

(defn div-el (cls content)
  [`<div ` `class="` cls `"`  `>` content `</div>`])

(defn visualize-impl [cfg lookup node acc]
  (match (ast-data-type node)
    :call     (let [callee (first node)
                    f    (lookup callee)]
                   (f visualize-impl cfg lookup node acc))
    :symbol   (push acc (div-el `ast-symbol` node))
    :string   (push acc (div-el `ast-string` node))
    )
  )

(defn lookup [sym]
  (match sym
    (fn [handle cfg lookup node acc] 
      (push acc (div-el "open-paren" "("))
      (each child-node node 
        (handle cfg lookup child-node acc))
      (push acc (div-el "close-paren" ")"))
    )))

(defn visualize ``
  produces HTML output from Lisp code
  - lookup :: fn-symbol -> fn(cfg lookup node acc)
  ``
  [cfg lookup code]

  (let 
    [acc @[]] 
    (visualize-impl cfg lookup code acc)
    (flat-string acc)))

# USAGE -----------------------------------

(defn file/put (path content)
  (def        f (file/open path :w))
  (file/write f content)
  (file/close f))

(let 
  [c (visualize {} lookup '(print "init"))]
  (file/put "./play.html" c))

# (print "init") -> :vector
# print          -> :symbol
# "init"         -> :string
