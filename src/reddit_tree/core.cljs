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


(defn simplify-comment-tree
  "Removes redundant data fields levels from json."
  [comments-json]
  (cond
    (or (instance? cljs.core/PersistentHashMap comments-json) (instance? cljs.core/PersistentArrayMap comments-json))
    (let [data (:data comments-json)]
      (?assoc data
              :children (simplify-comment-tree (:children data))
              :replies (simplify-comment-tree (:replies data))))
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


(def reddit-comment-data (r/atom {:empty "map"}))


(defn get-reddit-comments [link]
  (go
    (let [response (<! (http/get (str link ".json")
                                 {:with-credentials? false}))]
      (prn (count (:body response)))
      (prn (type (:body response)))
      ;; We are updating the reddit-comment-data atom here with info in this
      ;; async function. This means that when we access the atom later it's
      ;; possible that this code hasn't run yet, and that it is still empty!
      (reset! reddit-comment-data
              (filter-fields (:body response) :score :body :replies :children :data)))))


;; -------------------------
;; State

(defn atom-input [value]
  [:input {:type "text"
           :value @value
           :on-change #(reset! value (-> % .-target .-value))}])

;; Based on https://polymorphiclabs.io/posts-output/2018-01-13-reagent-d3-force-directed-graph/
(defn force-viz [ratom]
  (prn "sup" @ratom)
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
  (get-reddit-comments "https://www.reddit.com/r/Hydroponics/comments/p6jlip/growing_medium_falling_out_of_net_pots")
  (let [input-value (r/atom "foo")]
    (fn [] [:div [:h2 "Welcome to Reagent"]
            [:div [:p "The value: " @input-value] [:p "Change it: "] [atom-input input-value]]
            (force-viz reddit-comment-data)
            (rt-g/viz (r/atom miserables/data))
            [:div input-value]])))


;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
