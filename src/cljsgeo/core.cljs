(ns cljsgeo.core
  (:require [cljsjs.topojson :as topojson]
            [cljsjs.d3 :as d3]
            [cljs.core.async :as async :refer  [<! >!]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(def π (.-PI js/Math))
(def τ (* 2 π))

(def d3-json (.-json js/d3))
(def d3-geo (.-geo js/d3))
(def d3-select (.-select js/d3))
(def d3-timer (.-timer js/d3))

(def feature (.-feature js/topojson))
(def mesh (.-mesh js/topojson))

(defn albers-projection
  []
  (-> (.albers d3-geo)
      (.center #js [0 55.4])
      (.rotate #js [4.4 0])
      (.parallels #js [50 60])
      (.scale 6000)
      (.translate #js [480 580])))

(defn draw-solid
  [uk]
  (let [svg (d3-select "#map")
        path (.path d3-geo)
        proj (.projection path (albers-projection))]
    (-> (.append svg "path")
        (.datum (feature uk (.. uk -objects -subunits)))
        (.attr "d" proj))))

(defn draw-parts
  [svg uk projection]
  (let [path (.pointRadius (.path d3-geo) 2)
        proj (.projection path projection)
        feat (feature uk (.. uk -objects -subunits))
        not-irl-boundary (mesh uk (.. uk -objects -subunits) #(and (not= %1 %2) (not= (.-id %1) "IRL")))
        irl-boundary (mesh uk (.. uk -objects -subunits) #(and (= %1 %2) (= (.-id %1) "IRL")))]
    ;; subunit shapes
    (-> svg
        (.selectAll ".subunit")
        (.data (.-features feat))
        (.enter)
        (.append "path")
        (.attr "class" (fn [d] (str "subunit " (.-id d))))
        (.attr "d" proj))
    ;; boundaries
    (-> svg
        (.append "path")
        (.datum not-irl-boundary)
        (.attr "d" proj)
        (.attr "class" "subunit-boundary"))
    (-> svg
        (.append "path")
        (.datum irl-boundary)
        (.attr "d" proj)
        (.attr "class" "subunit-boundary IRL"))
    ;; place circles
    #_(-> svg
        (.append "path")
        (.datum (feature uk (.. uk -objects -places)))
        (.attr "d" proj)
        (.attr "class" "place"))
    ;; place labels
    #_(-> svg
        (.selectAll ".place-label")
        (.data (.-features (feature uk (.. uk -objects -places))))
        (.enter)
        (.append "text")
        (.attr "class" "place-label")
        (.attr "transform" (fn [d] (str "translate(" (projection (.. d -geometry -coordinates)) ")")))
        (.attr "dy" ".35em")
        (.text (fn [d] (.. d -properties -name))))
    ;; country labels
    #_(-> svg
        (.selectAll ".subunit-label")
        (.data (.-features (feature uk (.. uk -objects -subunits))))
        (.enter)
        (.append "text")
        (.attr "class" (fn [d] (str "subunit-label " (.-id d))))
        (.attr "transform" (fn [d] (str "translate(" (.centroid proj d) ")")))
        (.attr "dy" ".35em")
        (.text (fn [d] (.. d -properties -name))))))

#_(d3-json "uk.json" (fn [err uk]
                     (if err
                       (println err)
                       (draw-parts (d3-select "#map") uk (albers-projection)))))

(comment
 (-> (d3-select "#world")
     (.attr "width")
     )
 )

(defn draw-world
  [svg data rotation]
  (let [graticule (.graticule d3-geo)
        projection (-> (.orthographic d3-geo)
                       (.scale 200)
                       (.translate #js [200 200])
                       (.clipAngle 80)
                       (.rotate #js [rotation -45 0])
                       (.precision 0.1))
        path (-> (.path d3-geo)
                 (.projection projection)) ]
    (when svg
      (-> svg
          (.selectAll "*")
          (.remove))
      (-> svg
          (.append "defs")
          (.append "path")
          (.datum #js {:type "Sphere"})
          (.attr "id" "sphere")
          (.attr "d" path))
      (-> svg
          (.append "use")
          (.attr "class" "stroke")
          (.attr "xlink:href" "#sphere"))
      (-> svg
          (.append "use")
          (.attr "class" "fill")
          (.attr "xlink:href" "#sphere"))
      (-> svg
          (.append "path")
          (.datum graticule)
          (.attr "class" "graticule")
          (.attr "d" path))
      (-> svg
          (.append "path")
          (.datum (feature data (.. data -objects -land)))
          (.attr "class" "land")
          (.attr "d" path))
      (-> svg
          (.append "path")
          (.datum (mesh data (.. data -objects -countries) (fn [a b] (not= a b))))
          (.attr "class" "subunit-boundary")
          (.attr "d" path))
      (-> svg
          (.append "path")
          (.datum (mesh data (.. data -objects -states) (fn [a b] (not= a b))))
          (.attr "class" "subunit-boundary")
          (.attr "d" path))
      (draw-parts svg data projection))))

(defn clear!
  [ctx x y width height]
  (.clearRect ctx x y width height)
  ctx)

(defn stroke!
  [ctx width style f & args]
  (.beginPath ctx)
  (apply f args)
  (aset ctx "lineWidth" width)
  (aset ctx "strokeStyle" style)
  (.stroke ctx)
  ctx)

(defn fill!
  [ctx style f & args]
  (.beginPath ctx)
  (apply f args)
  (aset ctx "fillStyle" style)
  (.fill ctx)
  ctx)


(defn draw-world-canvas
  [canvas world]
  (let [ctx (-> canvas (.node) (.getContext "2d"))
        width (-> canvas (.node) (.-width))
        height (-> canvas (.node) (.-height))
        graticule ((.graticule d3-geo))
        projection (-> (.orthographic d3-geo)
                       (.scale 400)
                       (.translate #js [400 400])
                       (.clipAngle 80)
                       (.rotate #js [0 -15 0])
                       (.precision 0.1))
        path (-> (.path d3-geo)
                 (.projection projection)
                 (.pointRadius 1)
                 (.context ctx))
        land (feature world (aget world "objects" "land"))
        boundaries (mesh world (aget world "objects" "countries") (fn [a b] (not= a b)))
        states (mesh world (aget world "objects" "states") (fn [a b] (not= a b)))
        places (feature world (aget world "objects" "places"))
        ]
    (d3-timer
     (fn [e]
       (let [v 0.02
             projection (.rotate projection #js [(* v e) -15 0])]
         (-> ctx
             (clear! 0 0 width height)
            #_ (stroke! 3 "#002b36" path #js {:type "Sphere"})
             (fill! "#93a1a1" path #js {:type "Sphere"})
             (stroke! 0.5 "rgba(108,113,196,0.5)" path graticule)
             (fill! "#002b36" path land)
            #_ (stroke! 0.7 "#fdf6e3" path land)
             (stroke! 0.5 "#cb4b16" path boundaries)
             (stroke! 0.5 "rgba(211,54,130,0.7)" path states)
             (fill! "#fdf6e3" path
                    #js {:type "Feature"
                         :geometry #js {:type "Point" :coordinates #js [-96.936415 33.031545]}}
                    )
             (fill! "#fdf6e3" path
                    #js {:type "Feature"
                         :geometry #js {:type "Point" :coordinates #js [-122.415629 37.773963]}}
                    )
             (stroke! 0.5 "#fdf6e3" path
                    #js {:type "LineString"
                         :coordinates #js [ #js [-96.936415 33.031545] #js [-122.415629 37.773963]]}
                    )
             ))
       false))))

(defn rotate
  [v world]
  (d3-timer (fn [e]
              (draw-world (d3-select "#world") world (* v e))
              false)))

(d3-json "blah.json"
         (fn [err world]
           (when err
             (println err))
           (draw-world-canvas (d3-select "#wcanvas") world)
           ))





(comment

 (rotate 0.02)
 (d3-json "uk.json" (fn [err uk]
                     (if err
                       (println err)
                       (println (feature uk (.. uk -objects -subunits))))))
 )

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
