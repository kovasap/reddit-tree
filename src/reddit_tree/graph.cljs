;; Taken from https://gist.github.com/prook/9e5cc9144d34a991978a2fd31b4ee487
;; and comment thread https://github.com/gadfly361/rid3/issues/10.

(ns reddit-tree.graph
  (:require
   [goog.string :as gstring]
   [goog.string.format]
   [rid3.core :as rid3 :refer [rid3->]]))


(defn create-sim
  [viz-state]
  (let [{:keys [width height]} @viz-state]
    (doto (js/d3.forceSimulation)
      (.stop)
      (.force "link" (-> (js/d3.forceLink)
                         (.id #(.-index %))))
      (.force "charge" (js/d3.forceManyBody))
      (.force "center" (js/d3.forceCenter (/ width 2) (/ height 2)))
      (.on "tick" (fn []
                    (when-let [s (:links-sel @viz-state)]
                      (rid3-> s
                              {:x1 #(.. % -source -x)
                               :y1 #(.. % -source -y)
                               :x2 #(.. % -target -x)
                               :y2 #(.. % -target -y)}))
                    (when-let [s (:nodes-sel @viz-state)]
                      (rid3-> s
                              {:cx #(.-x %)}
                              {:cy #(.-y %)})))))))

(defn create-drag
  [sim]
  (-> (js/d3.drag)
      (.on "start" (fn started
                     [_d _ _]
                     (if (-> js/d3 .-event .-active zero?)
                       (doto sim
                         (.alphaTarget 0.3)
                         (.restart)))))
      (.on "drag" (fn dragged
                    [d _ _]
                    (let [event (.-event js/d3)]
                      (prn "d" d)
                      (prn "event" event)
                      (set! (.-fx d) (.-x event))
                      (set! (.-fy d) (.-y event)))))
      (.on "end" (fn ended
                   [d _ _]
                   (if (-> js/d3 .-event .-active zero?)
                     (.alphaTarget sim 0))
                   (set! (.-fx d) nil)
                   (set! (.-fy d) nil)))))


(defn merge-nodes
  [orig new id]
  (let [orig-map (into {} (map-indexed (fn [i n] [(id n) i]) orig))]
    (doseq [n new]
      (when-let [old (aget orig (orig-map (id n)))]
        (when-let [x (.-x old)] (set! (.-x n) x))
        (when-let [y (.-y old)] (set! (.-y n) y))
        (when-let [vx (.-vx old)] (set! (.-vx n) vx))
        (when-let [vy (.-vy old)] (set! (.-vy n) vy))
        (when-let [fx (.-fx old)] (set! (.-fx n) fx))
        (when-let [fy (.-fy old)] (set! (.-fy n) fy))))
    new))

(defn update-sim! [sim alpha-target {:keys [links nodes]}]
  (let [old-nodes (.nodes sim)
        new-nodes (merge-nodes old-nodes nodes #(.-name %))]
    (doto sim
      (.nodes new-nodes)
      (-> (.force "link") (.links links))
      (.alpha alpha-target)
      (.restart))))

(defn get-node-hover-text-sel [node viz-state]
  (js/d3.selectAll (gstring/format ".c%s" (.-id node))))


(defn viz
  [ratom]
  (let [viz-state (atom {:width 800
                         :height 800
                         :hover-text-sel nil
                         :links-sel nil
                         :nodes-sel nil})
        sim (create-sim viz-state)
        drag (create-drag sim)
        ;; See
        ;; https://github.com/d3/d3-scale-chromatic/blob/main/README.md#api-reference
        ;; for options.
        color (js/d3.scaleOrdinal (.-schemeSet1 js/d3))]
    (fn [ratom]
      [rid3/viz {:id     "rid3-force-demo"
                 :ratom  ratom
                 :svg    {:did-mount  (fn [svg ratom]
                                        (let [{:keys [width height]} @viz-state]
                                          (rid3-> svg
                                                  {:width   width
                                                   :height  height
                                                   :viewBox #js [0 0 width height]}))
                                        (update-sim! sim 1 @ratom))
                          :did-update (fn [svg ratom]
                                        (update-sim! sim 0.3 @ratom))}
                 :pieces [{:kind            :elem-with-data
                           :class           "links"
                           :tag             "line"
                           :prepare-dataset (fn [ratom] (:links @ratom))
                           :did-mount       (fn [sel _ratom]
                                              (swap! viz-state assoc :links-sel sel)
                                              (rid3-> sel
                                                      {:stroke         "#999"
                                                       :stroke-opacity 0.6
                                                       :stroke-width   #(-> (.-value %)
                                                                            js/Math.sqrt)}))}
                          {:kind            :elem-with-data
                           :class           "hover-text"
                           :tag             "text"
                           ;; :prepare-dataset (fn [ratom] (take 1 (:nodes @ratom)))
                           :prepare-dataset (fn [ratom] (:nodes @ratom))
                           :did-mount       (fn [sel _ratom]
                                              (swap! viz-state assoc :hover-text-sel sel)
                                              (rid3-> sel
                                                      {:cx 100
                                                       :cy 100
                                                       :class #(str "c" (.-id %))
                                                       :style {:opacity 0
                                                               :color "red"}}
                                                      (.text #(.-name %))))}
                          {:kind            :elem-with-data
                           :class           "nodes"
                           :tag             "circle"
                           :prepare-dataset (fn [ratom] (:nodes @ratom))
                           :did-mount       (fn [sel _ratom]
                                              (swap! viz-state assoc :nodes-sel sel)
                                              (rid3-> sel
                                                      {:stroke         "#fff"
                                                       :stroke-width   1.5
                                                       :r              #(.-size %)
                                                       :fill           #(color (.-group %))
                                                       :fill-opacity   #(.-opacity %)}
                                                      (.on "mouseover" (fn [event node]
                                                                         (-> (get-node-hover-text-sel node viz-state)
                                                                             (.attr "x" (.-x node))
                                                                             (.attr "y" (.-y node))
                                                                             (.style "color" "green")
                                                                             (.style "opacity" 1))))
                                                      (.on "mouseout" #(.style (:hover-text-sel @viz-state) "opacity" 0))
                                                      (.call drag)))}]}])))

(defn prechew
  [app-state]
  (-> @app-state
      (update :nodes clj->js)
      (update :links clj->js)))
