(ns reddit-tree.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [reddit-tree.graph :as rt-g]
   [reddit-tree.miserables :as miserables]
   [cljs-time.core :as time]
   [cljs-time.coerce :as ctime]
   [cljs-time.format :as ftime]
   [rid3.core :as rid3 :refer [rid3->]]
   [cljs-http.client :as http]
   [cljs.core.async :refer [<!]]
   [goog.string :as gstring]
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
        ;; Removes :replies because we already stored that information under
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
  (if (<= score 0)
    1
    (+ 5 (* 2 (Math/log10 score)))))

(def max-time-secs (r/atom 0))
(def slider-secs-after-op (r/atom 0))
  
;; Converts a time a comment was posted after OP into an opacity with which to
;; display that comment.
(def min-opacity 0.1)
(def max-opacity 1.0)
(defn time-to-opacity [secs]
  (let [time-frac (- 1 (min 1.0 (/ (float secs) @max-time-secs)))]
    (+ min-opacity (* (- max-opacity min-opacity) time-frac))))


;; Note that the into calls in get-nodes/get-links may be O(n) (prepending to a
;; vector).

(defn get-nodes
  ([comment] (get-nodes 0 comment))
  ([depth comment]
   ;; Some comments in the returned json are just hash strings, presumably to
   ;; save bandwidth. This is determined by the "limit" query parameter
   ;; provided.
   (let [is-hash (string? comment)
         is-op (and (not is-hash) (not (contains? comment :body)))
         size (score-to-value (get comment :score 10))]
     ;; Not sure where the extra OP node is coming from, we ignore it here when
     ;; making the graph.
     (if (and is-op (empty? (:children comment)))
       []
       (into [{:name (cond is-op "OP" is-hash (str "hash_" comment) :else (:body comment))
               :group depth  ;; (if is-op 1 2)
               :depth depth
               :size size
               :link (get comment :permalink "")
               :id (get comment :secs-after-op 0)
               :score (get comment :score 0)
               :secs-after-op (get comment :secs-after-op 0)
               :opacity (if is-op 1.0 (time-to-opacity (get comment :secs-after-op 0)))}]
             (apply concat (mapv (partial get-nodes (+ 1 depth)) (:children comment))))))))

(defn get-links
  ([parent-name comment]
   (let [children (:children comment)
         is-hash (string? comment)
         name (if is-hash comment (get comment :body "OP"))]
     (into [{:source parent-name :target name :value 1}]
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
  (let [nodes (get-nodes data)
        links (get-links "OP" data)]
    {:nodes nodes
     :links (named-links-to-indexed-links nodes links)}))

;; (prn "hello" (make-reddit-comment-data-into-graph example-data))

(defn add-secs-after-op [post-data comment-data]
  (let [op-time (:created (first (:children post-data)))]
    (cond
      (is-map comment-data)
      (assoc comment-data
             :secs-after-op (- (:created comment-data) op-time)
             :children (add-secs-after-op post-data (:children comment-data)))
      (instance? cljs.core/PersistentVector comment-data)
      (mapv (partial add-secs-after-op post-data) comment-data)
      :else
      comment-data)))

(defn get-max-time-secs [comment-data]
  (cond
    (is-map comment-data)
    (max (:secs-after-op comment-data) (get-max-time-secs (:children comment-data)))
    (instance? cljs.core/PersistentVector comment-data)
    (if (empty? comment-data) 0
      (apply max (mapv get-max-time-secs comment-data)))
    :else
    comment-data))

(def reddit-post-data (r/atom {:empty "map"}))
(def reddit-comment-data (r/atom {:empty "map"}))
(def reddit-comment-graph (r/atom {:nodes [] :links []}))

(defn update-reddit-data! [link]
  (go
    (let [response (<! (http/get (str link ".json?limit=10000")
                                 {:with-credentials? false}))]
      ;; (prn "response" (count (:body response)) (type (:body response)))
      ;; We are updating the reddit-comment-data atom here with info in this
      ;; async function. This means that when we access the atom later it's
      ;; possible that this code hasn't run yet, and that it is still empty!
      (let [simplified-data
            (simplify-comment-tree
              (filter-fields
                (:body response) :title :selftext :score :body :replies :children :data :created :permalink))
            [post-data comment-data] simplified-data
            time-updated-comment-data (add-secs-after-op post-data comment-data)]
         (reset! max-time-secs (get-max-time-secs time-updated-comment-data))
         (reset! slider-secs-after-op @max-time-secs)
         (reset! reddit-post-data (first (:children post-data)))
         (reset! reddit-comment-data time-updated-comment-data)
         (reset! reddit-comment-graph
                 (make-reddit-comment-data-into-graph time-updated-comment-data))))))


(defn format-reddit-timestamp [timestamp]
  (ftime/unparse (:rfc822 ftime/formatters) (ctime/from-long (* 1000 timestamp))))


(defn sanitize-reddit-url
  "Makes sure we can append .json to the given url and get a proper response from reddit."
  [url]
  (if (= "/" (subs url (count url) (count url)))
    (subs url 0 (count url))
    url))


(defn url-input [value]
  [:input {:type "text"
           :size 100
           :value @value
           :on-change (fn [e]
                        (reset! value (sanitize-reddit-url (-> e .-target .-value)))
                        (update-reddit-data! @value))}])

(defn update-nodes!
  "Updates all nodes in reddit-comment-graph with the given function (that takes and returns a node)."
  [update-func]
  (swap! reddit-comment-graph
         (fn [data]
           (update data :nodes
                   (fn [nodes] (map update-func nodes))))))

(defn secs-after-op-log-slider [value max]
  [:input {:type "range"
           :value (Math/log value)
           :step 0.1
           :min 0.01
           :max (Math/log max)
           :style {:width "100%"}
           :on-change (fn [e]
                        (let [new-value (.. e -target -value)]
                          (reset! slider-secs-after-op (Math/exp new-value))
                          (update-nodes!
                            (fn [node]
                              (let [selected-by-slider
                                    (<= 0 (- @slider-secs-after-op (:secs-after-op node)))]
                                (assoc node :opacity
                                       (if selected-by-slider 1 0)))))))}])

(defn secs-after-op-slider [value max]
  [:input {:type "range"
           :value value
           :min 0
           :max max
           :style {:width "100%"}
           :on-change (fn [e]
                        (let [new-value (.. e -target -value)]
                          (reset! slider-secs-after-op  new-value)
                          (update-nodes!
                            (fn [node]
                              (let [selected-by-slider
                                    (<= 0 (- @slider-secs-after-op (:secs-after-op node)))]
                                (assoc node :opacity
                                       (if selected-by-slider 1 0)))))))}])

(defn secs-to-days-hrs-mins-secs-str
  "Converts seconds to a string with hours, minutes, and seconds."
  [secs]
  (let [str-days (int (/ secs 60 60 24))
        str-hours (int (mod (/ secs 60 60) 24))
        str-mins (int (mod (/ secs 60) 60))
        str-secs (int (mod secs 60))]
    (str str-days " days, " str-hours " hours, " str-mins " minutes, and "
         str-secs " seconds.")))
  

;; -------------------------
;; Views

(def test-urls
  ["https://www.reddit.com/r/interestingasfuck/comments/qew7al/train_to_machu_picchu_with_a_balcony/"
   "https://www.reddit.com/r/Hydroponics/comments/p6jlip/growing_medium_falling_out_of_net_pots"])

(defn home-page []
  (let [input-value (r/atom (nth test-urls 0))]
    (update-reddit-data! @input-value)
    (fn [] [:div [:h2 "Reddit Comment Analyzer"]
            [:div
             [:p "Enter URL Here:" [:br] [url-input input-value]]
             [:p [:b (:title @reddit-post-data)] [:br] " posted on "
              (format-reddit-timestamp (:created @reddit-post-data))]
             ;; [:p "Post Text: " (:selftext @reddit-post-data)]
             [:span "Scroll through time:" [secs-after-op-slider @slider-secs-after-op @max-time-secs]]
             [:span "Log scale:" [secs-after-op-log-slider @slider-secs-after-op @max-time-secs]]
             [:p "Time after OP: "
              (secs-to-days-hrs-mins-secs-str @slider-secs-after-op)]]
             ;; [:p "Secs after OP: " @slider-secs-after-op]
            [rt-g/viz (r/track rt-g/prechew reddit-comment-graph)]
            [:p "Double click on nodes to go directly to the comment they "
             "represent."]
            [:p "Each node in the graph is a comment. The nodes are sized by "
             "their score (upvotes - downvotes). Their opacity represents "
             "their posting time relative to the original post (OP) - darker "
             "is older (closer to OP). All comments will have the same "
             "(minimum) opacity if they were posted more than "
             (gstring/format "%.2f" (/ @max-time-secs 60 60 24))
             " days after the original post."]
            [:div (count (:nodes @reddit-comment-graph)) " total comments,"
             [:ul
              [:li (count (filter #(>= (:score %) 1000) (:nodes @reddit-comment-graph))) " with score greater than 1000,"]
              [:li (count (filter #(< 99 (:score %) 1001) (:nodes @reddit-comment-graph))) " with score between 100 and 1000,"]
              [:li (count (filter #(< 49 (:score %) 101) (:nodes @reddit-comment-graph))) " with score between 50 and 100,"]
              [:li (count (filter #(< 14 (:score %) 51) (:nodes @reddit-comment-graph))) " with score between 15 and 50,"]
              [:li (count (filter #(< 6 (:score %) 16) (:nodes @reddit-comment-graph))) " with score between 5 and 15,"]
              [:li (count (filter #(< 2 (:score %) 6) (:nodes @reddit-comment-graph))) " with score between 2 and 5,"]
              [:li (count (filter #(= (:score %) 1) (:nodes @reddit-comment-graph))) " with score of 1,"]
              [:li (count (filter #(< (:score %) 0) (:nodes @reddit-comment-graph))) " with score less than 0."]]]
            [:div {:class "dev-notes"} "Source code can be found at " [:a {:href "https://github.com/kovasap/reddit-tree"} "github.com/kovasap/reddit-tree"] "."
              [:details
               [:summary "Raw Data"]
               [:p "Graph Data: " @reddit-comment-graph]
               [:p "Post Data: " @reddit-post-data]
               [:p "Comment Data: " @reddit-comment-data]]]])))


;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
