(ns skiing.core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:gen-class))

(defn parse-line
  [s]
  (try (->> (str/split s #" ")
            (mapv #(Integer/parseUnsignedInt %)))
       (catch Exception e
         (println "Unable to parse line")
         (System/exit 1))))

(defn list-files
  []
  (let [d (io/file (System/getProperty "user.dir"))]
    (filter (fn [f] (str/ends-with? (.getName f) ".txt"))
            (file-seq d))))

(defn read-grid
  [name]
  (println "Reading grid")
  (try (with-open [f (io/reader name)]
         (mapv parse-line (rest (line-seq f))))
       (catch Exception e
         (println "Unable to open file")
         (System/exit 1))))

(defn ->elevations
  "Takes a grid of elevations and 'flattens' it,
  returning a map from coordinates to elevations."
  [grid]
  (println "Flattening grid")
  (reduce-kv
   (fn [acc y v]
     (reduce-kv
      (fn [acc x v]
        (assoc acc [x y] {:elevation v
                          :path-len  1
                          :descent   0}))
      acc
      v))
   {}
   grid))

(defn neighbours
  [elevations [x y]]
  (filter
   identity
   (mapv (partial get elevations)
         [[(inc x) y]
          [(dec x) y]
          [x (inc y)]
          [x (dec y)]])))

(defn neighbour-coords
  [elevations [x y]]
  (keep (fn [coord]
          (when (get elevations coord) coord))
        [[(inc x) y]
         [(dec x) y]
         [x (inc y)]
         [x (dec y)]]))

(defn topological-sort
  [f]
  (fn [coll]
    (println "Sorting" (count coll) "elevations")
    (loop [coll   coll
           sorted []]
      (let [[ripe unripe] (f coll)]
        (if-not (seq ripe)
          sorted
          (recur unripe
                 (into sorted ripe)))))))

(def ski-sort
  (topological-sort
   (fn [coll]
     (let [elevations'
           (into {} coll)

           {ripe true, unripe false}
           (group-by
            (fn arbiter [[coord {:keys [elevation]}]]
              (every? #(>= elevation (:elevation %))
                      (neighbours elevations' coord)))
            elevations')]
       [ripe unripe]))))

(defn should-ski?
  "Checks whether `cell2` should be skiid-to via `cell1`."
  [cell1 cell2]
  (cond (>= (:path-len cell1) (dec (:path-len cell2)))
        true

        (and (>= (:path-len cell1) (dec (:path-len cell2)))
             (> (+ (:elevation cell1) (:descent cell1))
                (+ (:elevation cell2) (:descent cell2))))
        true

        :else
        false))

(defn update-cell
  [higher lower]
  (if (should-ski? higher lower)
    (assoc lower
           :path-len (inc (:path-len higher))
           :descent (- (+ (:descent higher) (:elevation higher))
                       (:elevation lower)))
    lower))

(defn relax
  "Takes a map of elevations, a cell, and a list of lower neighbours,
  and updates the neighbours through discerning skiing."
  [elevations element neighbour-coords]
  (reduce (fn [elevs ncoord]
            (update elevs ncoord (partial update-cell element)))
          elevations
          neighbour-coords))

(defn calculate-paths
  [grid]
  (let [elevations (->elevations grid)
        topo       (ski-sort elevations)]
    (println "Calculating ski routes")
    (reduce (fn [acc [coord {:keys [elevation] :as element}]]
              (if-let [neighbours
                       (->> coord
                            (neighbour-coords acc)
                            (filterv #(> elevation (:elevation (get acc %)))))]
                (relax acc (get acc coord) neighbours)
                acc))
            elevations
            topo)))

(defn spit-answer
  [file-name]
  (->> file-name
       read-grid
       calculate-paths
       vals
       (group-by :path-len)
       (apply max-key key)
       val
       (apply max-key :descent)
       (#(str (:path-len %) (:descent %)))))

(defn prompt-file
  []
  (let [files (list-files)]
    (println "SELECT A FILE:")
    (println "==============")
    (dorun
     (map-indexed #(println %1 "->" (.getPath %2))
                  files))
    (->> (read-line)
         parse-line
         first
         (nth files))))

(defn -main
  ([] (-main (prompt-file)))
  ([file-name] (-> file-name spit-answer println)))
