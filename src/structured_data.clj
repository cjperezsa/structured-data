(ns structured-data)

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2)))

(defn spiff [v]
  (let [fst (if-not (get v 0) 0 (get v 0))
        trd (if-not (get v 2) 0 (get v 2))]
    (+ fst trd)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[fst _ trd] v]
    (+ fst trd)))

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
 (= (width rectangle) (height rectangle)))

(defn area [rectangle]
 (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and (<= x1 x x2)
         (<= y1 y y2))))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1)
         (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)
        newauthors (conj authors new-author)]
    (assoc book :authors newauthors)))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [scnd (fn [v] (get v 1))]
    (map scnd collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (< (count (set a-seq))
     (count a-seq)))

(defn old-book->new-book [book]
  (let [authors (:authors book)
        newauthors (set authors)]
    (assoc book :authors newauthors)))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        date (if (contains? author :death-year)
               (str " ("(:birth-year author) " - "(:death-year author) ")")
               (if (contains? author :birth-year)
                 (str " (" (:birth-year author) " - )")
                 ""))]
    (str name date)))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (if (empty? books)
    "No books."
    (if (= 1 (count books))
      (str (count books) " book. " (book->string (first books)) ".")
      (str (count books) " books. "
           (apply str (interpose ", " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [bok] (has-author? bok author)) books))

(defn author-by-name [name authors]
  (first (filter (fn [auth] (= name (:name auth))) authors)))

(defn living-authors [authors]
  (filter (fn [auth] (alive? auth)) authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books))

; %________%
