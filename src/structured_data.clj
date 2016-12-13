(ns structured-data)

(defn do-a-thing [x]
  (let [xplus (+ x x)]
    (Math/pow xplus xplus)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a b c] v]
    (+ a c)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1)))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1)))

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle
        side1 (+ x2 y1)
        side2 (+ x1 y2)]
    (if (== side1 side2) true false)))

(defn area [rectangle]
    (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [px py] point]
    (if (and (<= x1 px x2) (<= y1 py y2)) true false)))

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner
        point1 [x1 x2]
        point2 [y1 y2]]
    (if (and (contains-point? outer point1) (contains-point? outer point2)) true false)))

;;(def china {:name "China Miéville", :birth-year 1972})
;;(def octavia {:name "Octavia E. Butler"
;;             :birth-year 1947
;;             :death-year 2006})
;;(def friedman {:name "Daniel Friedman" :birth-year 1944})
;;(def felleisen {:name "Matthias Felleisen"})

;;(def cities {:title "The City and the City" :authors #{china}})
;;(def wild-seed {:title "Wild Seed", :authors #{octavia}})
;;(def embassytown {:title "Embassytown", :authors #{china}})
;;(def little-schemer {:title "The Little Schemer"
;;                     :authors #{friedman, felleisen}})

;;(def books [cities, wild-seed, embassytown, little-schemer])

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false))

(defn add-author [book new-author]
  (let [original book
        authors (:authors book)
        new (assoc original :authors (conj authors new-author))]
  new))

(defn alive? [author]
  (if (contains? author :death-year) false true))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second-element (fn [e] (get e 1))]
    (map second-element collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq)) true false))

(defn stars [n]
 (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [original-size (count a-seq)
        new-size (count (distinct a-seq))]
    (if (== original-size new-size) false true)))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (if (contains? (:authors book) author) true false))

;; defn authors and defn all-author-names do not work work if def authors set of authors is defined
(defn authors [books]
  (set (apply clojure.set/union (map :authors books))))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [author-name (:name author)
        years (str " (" (:birth-year author) " - " (:death-year author) ")")]
    (if (contains? author :birth-year) (str author-name years) author-name)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [book-title (:title book)
        authors (:authors book)]
    (str book-title ", written by " (authors->string authors))))

(defn books->string [books]
  (let [amnt-books (count books)
        any-books (cond
                               (== amnt-books 1) (str amnt-books " book. ")
                               (> amnt-books 1) (str amnt-books " books. ")
                               :else (str "No books."))]
    (if (> amnt-books 0)
      (str any-books (apply str(interpose ". " (map book->string books))) ".") (str any-books))))

(defn books-by-author [author books]
    (filter (fn [book] (has-author? book author)) books))

;;(def authors #{china, felleisen, octavia, friedman})

(defn author-by-name [name authors]
  (let [find-author? (fn [author] (if (= (:name author) name) true false))]
    (first (filter (fn [author] (find-author? author)) (seq authors)))))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) (seq authors)))

;;(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
;;(def christopher {:name "Christopher Tolkien" :birth-year 1924})
;;(def kay {:name "Guy Gavriel Kay" :birth-year 1954})

;;(def silmarillion {:title "Silmarillion"
;;                   :authors #{jrrtolkien, christopher, kay}})

;;(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
;;(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

;;(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

(defn has-a-living-author? [book]
  (let [book-authors (:authors book)]
    (if (empty? (living-authors book-authors)) false true)))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
