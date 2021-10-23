(ns reddit-tree.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [reddit-tree.graph :as rt-g]
   [reddit-tree.miserables :as miserables]
   [rid3.core :as rid3 :refer [rid3->]]
   [cljs-http.client :as http]
   [cljs.core.async :refer [<!]]
   [reagent.core :as r]
   [reagent.dom :as d]))


;; Useful for determining structure: https://jsonformatter.curiousconcept.com/#
(defn print-comment-bodies [comments-json]
  ;; (prn "call" (keys comments-json))
  (prn (:body comments-json))
  ;; json can either have :data or :comments, in alternating sub-dicts
  (if (:data comments-json)
     (print-comment-bodies (:data comments-json))
     (if (:children comments-json)
       (mapv print-comment-bodies (:children comments-json))
       nil)))


(defn ?assoc
  "Same as assoc, but skip the assoc if v is nil."
  [m & kvs]
  (->> kvs
    (partition 2)
    (filter second)
    (map vec)
    (into m)))


(defn is-map [thing]
  (or (instance? cljs.core/PersistentHashMap thing)
      (instance? cljs.core/PersistentArrayMap thing)))


(defn simplify-comment-tree
  "Removes redundant data fields levels from json. And collapses :replies to :children."
  [comments-json]
  (cond
    (is-map comments-json)
    (let [data (:data comments-json)]
      (dissoc
        (?assoc data
                :children (cond
                            (:children data) (simplify-comment-tree (:children data))
                            (= "" (:replies data)) []
                            (is-map (:replies data)) (simplify-comment-tree (:children (:data (:replies data))))
                            :else []))
                ;; :children (simplify-comment-tree (:children data))
                ;; :replies (simplify-comment-tree (:replies data))))
        ;; Removes :replies because we already stored that information under
        ;; :children.
        :replies))
    (instance? cljs.core/PersistentVector comments-json)
    (mapv simplify-comment-tree comments-json)
    :else
    comments-json))


(defn filter-fields
  "Removes all nil values, and all non-provided fields from the input json structure."
  [comments-json & fields]
  (cond
    (or (instance? cljs.core/PersistentHashMap comments-json) (instance? cljs.core/PersistentArrayMap comments-json))
    (let [non-nil-fields (filter (fn [f] (some? (f comments-json))) fields)]
      (zipmap non-nil-fields
              (map (fn [f] (apply filter-fields (f comments-json) fields))
                   non-nil-fields)))
    ;; This is a way to do it with hard-coded fields, ignoring nils:
    ;; (?assoc :score (filter-fields (:score comments-json))
    ;;         :body (filter-fields (:body comments-json))
    ;;         :replies (filter-fields (:replies comments-json))
    ;;         :children (filter-fields (:children comments-json))
    ;;         :data (filter-fields (:data comments-json)))
    (instance? cljs.core/PersistentVector comments-json)
    (mapv (fn [e] (apply filter-fields e fields)) comments-json)
    :else
    comments-json))


(def example-data
  [{:children [{:score 2, :children []}]}  ;; This is "OP" (Original Post)
   {:children [{:score 1, :body "A", :children []}
               {:score 1, :body "B", :children []}
               {:score 1, :body "C", :children []}
               {:score 0, :body "D",
                :children [{:score 1, :body "E",
                            :children [{:score 2, :body "F", :children []}]}
                           {:score 1, :body "G", :children []}]}]}])

(def example-graph
  {:nodes [{:name "OP" :size 2}   ;; Index 0
           {:name "A"  :size 1}   ;; Index 1
           {:name "B"  :size 1}   ;; Index 2
           {:name "C"  :size 1}   ;; Index 3
           {:name "D"  :size 1}   ;; Index 4
           {:name "E"  :size 1}   ;; Index 5
           {:name "F"  :size 1}   ;; Index 6
           {:name "G"  :size 1}]  ;; Index 7
   :links [
           ;; These are special cases pointing OP to the direct children.
           {:source "OP" :target "A" :value 1}
           {:source "OP" :target "B" :value 1}
           {:source "OP" :target "C" :value 1}
           {:source "OP" :target "D" :value 1}
           ;; These are parents pointing to their children.
           {:source "D" :target "E" :value 1}
           {:source "D" :target "G" :value 1}
           {:source "E" :target "F" :value 1}]})


;; Converts a reddit score to a value that can be used as a :size for nodes or
;; a :value for edges.
(defn score-to-value [score]
  (+ 5 score))


;; Note that the into calls in get-nodes/get-links may be O(n) (prepending to a
;; vector).

(defn get-nodes [comment]
  (into [{:name (get comment :body "OP")
          :size (score-to-value (get comment :score 1))}]
        (apply concat (mapv get-nodes (:children comment)))))

(defn get-links
  ([comment] (get-links "root" comment))
  ([parent-name comment]
   (let [children (:children comment)
         name (get comment :body "OP")
         score (:score comment)]
     (into [{:source parent-name :target name :value (score-to-value score)}]
           (apply concat (mapv (partial get-links name) children))))))

(defn named-links-to-indexed-links [nodes links]
  (let [name-to-idx (into {} (map-indexed (fn [i n] [(:name n) i]) nodes))]
    (mapv (fn [link] (assoc link
                            :source (get name-to-idx (:source link))
                            :target (get name-to-idx (:target link))))
          links)))

;; Takes return from simplify-comment-tree and creates nodes/links dataset like
;; found in miserables.cljs. In other words, converts map like example-data to
;; example-graph.
(defn make-reddit-comment-data-into-graph [data]
  (let [nodes (into [{:name "root" :size 1}]
                    (get-nodes data))
        links (get-links data)]
    {:nodes nodes
     :links (named-links-to-indexed-links nodes links)}))

;; (prn "hello" (make-reddit-comment-data-into-graph example-data))


(def reddit-post-data (r/atom {:empty "map"}))
(def reddit-comment-data (r/atom {:empty "map"}))
(def reddit-comment-graph (r/atom {:empty "map"}))

(defn update-reddit-data! [link]
  (go
    (let [response (<! (http/get (str link ".json")
                                 {:with-credentials? false}))]
      (prn "response" (count (:body response)) (type (:body response)))
      ;; We are updating the reddit-comment-data atom here with info in this
      ;; async function. This means that when we access the atom later it's
      ;; possible that this code hasn't run yet, and that it is still empty!
      (let [simplified-data
            (simplify-comment-tree
              (filter-fields
                (:body response) :title :selftext :score :body :replies :children :data))
            [post-data comment-data] simplified-data]
         (reset! reddit-post-data (first (:children post-data)))
         (reset! reddit-comment-data comment-data)
         (reset! reddit-comment-graph (make-reddit-comment-data-into-graph comment-data))
         (prn "graph" @reddit-comment-graph)))))


;; -------------------------
;; State

(defn atom-input [value]
  [:input {:type "text"
           :value @value
           :on-change #((update-reddit-data! @value)
                        (reset! value (-> % .-target .-value)))}])

;; Based on https://polymorphiclabs.io/posts-output/2018-01-13-reagent-d3-force-directed-graph/
(defn force-viz [ratom]
  [rid3/viz
    {:id "force"
     :ratom ratom
     :svg {:did-mount (fn [node ratom]
                        (rid3-> node
                            (.attr "width" 1000)
                            (.attr "height" 1000)
                            (.style "background-color" "grey")))}}])

;; -------------------------
;; Views

(defn home-page []
  (let [input-value (r/atom "https://www.reddit.com/r/Hydroponics/comments/p6jlip/growing_medium_falling_out_of_net_pots")]
    (update-reddit-data! @input-value)
    (fn [] [:div [:h2 "Welcome to Reagent"]
            [:div [:p "URL: " @input-value]
                  [:p "Title: " (:title @reddit-post-data)]
                  [:p "Post Text: " (:selftext @reddit-post-data)]
                  [:p "Change it: "] [atom-input input-value]
                  [:p "Graph Data: " @reddit-comment-graph]
                  [:p "Raw Data: " @reddit-comment-data]]
            (rt-g/viz reddit-comment-graph)
            ;; (force-viz reddit-comment-data)
            ;; (rt-g/viz (r/atom miserables/data))
            [:div [:p "End of page"]]])))


;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
