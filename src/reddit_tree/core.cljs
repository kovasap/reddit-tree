(ns reddit-tree.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
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


(defn is-comment? [json]
  (and (:score json) (:body json)))


;; Returns nested map like {:score  0 :body "" :children [{:score  ...}]}
(defn get-comments-tree [comments-json]
  (prn (keys (select-keys comments-json [:score :body :data :children])))
  (prn (type comments-json))
  (if (instance? cljs.core/PersistentArrayMap comments-json)
    (if (is-comment? comments-json)
      (assoc (select-keys comments-json [:score :body])
             :children (get-comments-tree (:children comments-json)))
      (get-comments-tree (:data comments-json)))
    (if (nil? comments-json)
      comments-json
      (mapv get-comments-tree comments-json))))
    

(defn get-reddit-comments [link]
  (go (let [response (<! (http/get (str link ".json")
                                   {:with-credentials? false}))]
        (prn (count (:body response)))
        (prn (get-comments-tree (:body response))))))
        ;; mapv is NOT lazy, so we get prints right away!
        ;;(mapv print-comment-bodies (:body response)))))


;; -------------------------
;; State

(defn atom-input [value]
  [:input {:type "text"
           :value @value
           :on-change #(reset! value (-> % .-target .-value))}])

(defn force-viz [ratom]
  [rid3/viz
    {:id "force"
     :ratom ratom
     :svg {:did-mount (fn [node ratom]
                        (rid3-> node
                            (.attr "width" 1000)
                            (.attr "height" 1000)
                            (.style "background-color" "grey")))}}])

(defn make-html-comment-tree [ratom])
  
    

;; -------------------------
;; Views

(defn home-page []
  (get-reddit-comments "https://www.reddit.com/r/Hydroponics/comments/p6jlip/growing_medium_falling_out_of_net_pots")
  (let [input-value (r/atom "foo")]
    (fn [] [:div [:h2 "Welcome to Reagent"]
            [:div [:p "The value: " @input-value] [:p "Change it: "] [atom-input input-value]]
            [:div (prn input-value)]])))


;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
