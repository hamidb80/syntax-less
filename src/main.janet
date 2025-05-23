(defn data-type [data]
  (match (type data)
    :nil      :nil
    :boolean  :boolean
    :number   :number

    :array    :vector
    :tuple    :vector

    :table    :dict
    :struct   :dict

    :string   :string
    :buffer   :string
   
    :symbol   :symbol
    :keyword  :keyword

    :function  :function
    :cfunction :cfunction
    :fiber     :fiber
    ))

(defn syntaxless/config 
  "symbol -> fn(expr): HTML"
  []
  {})

(defn syntaxless/visualize
  "produces HTML output from Lisp code"
  [cfg code]

  (match (data-type code)
    :)

  (pp code))


(syntaxless/visualize {} 
  '(print "init"))
