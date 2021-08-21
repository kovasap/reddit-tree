(ns reddit-tree.core
    (:require
      [reagent.core :as r]
      [reagent.dom :as d]))

;; -------------------------
;; State

(defn atom-input [value]
  [:input {:type "text"
           :value @value
           :on-change #(reset! value (-> % .-target .-value))}])

;; -------------------------
;; Views

(defn home-page []
  (let [input-value (r/atom "foo")]
    (fn [] [:div [:h2 "Welcome to Reagent"]
            [:div [:p "The value: " @input-value] [:p "Change it: "] [atom-input input-value]]])))
  

;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [home-page] (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root))
