;; Taken from https://gist.github.com/prook/9e5cc9144d34a991978a2fd31b4ee487
;; and comment thread https://github.com/gadfly361/rid3/issues/10.

(ns reddit-tree.graph
  (:require
   [rid3.core :as rid3 :refer [rid3->]]))

(defn viz
  [ratom]
  (prn "inviz" @ratom)
  (let [{:keys [links nodes]} @ratom
        width 950
        height 800
        nodes-group "nodes"
        node-tag "circle"
        links-group "links"
        link-tag "line"
        component-id "rid3-force-demo"
        links (clj->js links)
        nodes (clj->js nodes)
        nodes-sel (volatile! nil)
        links-sel (volatile! nil)
        sim (doto (js/d3.forceSimulation nodes)
              (.force "link" (-> (js/d3.forceLink links)
                                 (.id #(.-index %))))
              (.force "charge" (js/d3.forceManyBody))
              (.force "center" (js/d3.forceCenter (/ width 2) (/ height 2)))
              (.on "tick" (fn []
                            (when-let [s @links-sel]
                              (rid3-> s
                                      {:x1 #(.. % -source -x)
                                       :y1 #(.. % -source -y)
                                       :x2 #(.. % -target -x)
                                       :y2 #(.. % -target -y)}))
                            (when-let [s @nodes-sel]
                              (rid3-> s
                                      {:cx #(.-x %)}
                                      {:cy #(.-y %)})))))
        color (js/d3.scaleOrdinal (.-schemeCategory10 js/d3))
        drag (-> (js/d3.drag)
                 (.on "start" (fn started
                                [_d _ _]
                                (if (-> js/d3 .-event .-active zero?)
                                  (doto sim
                                    (.alphaTarget 0.3)
                                    (.restart)))))
                 (.on "drag" (fn dragged
                               [d _ _]
                               (let [event (.-event js/d3)]
                                 (set! (.-fx d) (.-x event))
                                 (set! (.-fy d) (.-y event)))))
                 (.on "end" (fn ended
                              [d _ _]
                              (if (-> js/d3 .-event .-active zero?)
                                (.alphaTarget sim 0))
                              (set! (.-fx d) nil)
                              (set! (.-fy d) nil))))]
    [rid3/viz {:id     component-id
               :ratom  ratom
               :svg    {:did-mount (fn [svg _ratom]
                                     (rid3-> svg
                                             {:width   width
                                              :height  height
                                              :viewBox #js [0 0 width height]}))}
               :pieces [{:kind            :elem-with-data
                         :class           links-group
                         :tag             link-tag
                         :prepare-dataset (fn [_ratom] links)
                         :did-mount       (fn [sel _ratom]
                                            (vreset! links-sel sel)
                                            (rid3-> sel
                                                    {:stroke         "#999"
                                                     :stroke-opacity 0.6
                                                     :stroke-width   #(-> (.-value %)
                                                                          js/Math.sqrt)}))}
                        {:kind            :elem-with-data
                         :class           nodes-group
                         :tag             node-tag
                         :prepare-dataset (fn [_ratom] nodes)
                         :did-mount       (fn [sel _ratom]
                                            (vreset! nodes-sel sel)
                                            (rid3-> sel
                                                    {:stroke       "#fff"
                                                     :stroke-width 1.5
                                                     :r            #(.-size %)
                                                     :fill         #(color (.-group %))}
                                                    (.call drag)))}]}]))
