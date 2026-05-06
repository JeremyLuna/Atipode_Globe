(ns antipode-globe.core
  (:require ["three" :as three]
            ["topojson-client" :as topojson]))

(def earth-radius 2.6)
(def country-data-url "https://cdn.jsdelivr.net/npm/world-atlas@2/countries-110m.json")

(defonce state
  (atom {:scene nil
         :camera nil
         :renderer nil
         :globe nil
         :rotation {:x -0.32 :y -0.6}
         :zoom 8.1
         :dragging? false
         :last-pointer nil
         :target nil}))

(defn el [id]
  (.getElementById js/document id))

(defn set-status! [text]
  (set! (.-textContent (el "status")) text))

(defn deg->rad [deg]
  (* deg (/ js/Math.PI 180)))

(defn lon-lat->xyz
  ([lon lat] (lon-lat->xyz lon lat earth-radius))
  ([lon lat radius]
   (let [phi (deg->rad (- 90 lat))
         theta (deg->rad (+ lon 180))
         sin-phi (js/Math.sin phi)]
     (three/Vector3.
       (* (- radius) sin-phi (js/Math.cos theta))
       (* radius (js/Math.cos phi))
       (* radius sin-phi (js/Math.sin theta))))))

(defn antipode [[lon lat]]
  (let [lon2 (+ lon 180)
        wrapped (if (> lon2 180) (- lon2 360) lon2)]
    [wrapped (- lat)]))

(defn polygon-rings [geometry]
  (case (.-type geometry)
    "Polygon" (array-seq (.-coordinates geometry))
    "MultiPolygon" (mapcat array-seq (array-seq (.-coordinates geometry)))
    []))

(defn line-from-ring [ring material transform-fn radius]
  (let [points (->> (array-seq ring)
                    (map transform-fn)
                    (map (fn [[lon lat]] (lon-lat->xyz lon lat radius)))
                    (to-array))
        geometry (.setFromPoints (three/BufferGeometry.) points)]
    (three/Line. geometry material)))

(defn country-line-group [features material transform-fn radius]
  (let [group (three/Group.)]
    (doseq [feature features
            ring (polygon-rings (.-geometry feature))]
      (when (> (.-length ring) 3)
        (.add group (line-from-ring ring material transform-fn radius))))
    group))

(defn graticule-line [coords material]
  (let [points (->> coords
                    (map (fn [[lon lat]] (lon-lat->xyz lon lat (+ earth-radius 0.01))))
                    (to-array))
        geometry (.setFromPoints (three/BufferGeometry.) points)]
    (three/Line. geometry material)))

(defn make-graticule []
  (let [group (three/Group.)
        material (three/LineBasicMaterial. #js {:color 0x93aaa5
                                                :transparent true
                                                :opacity 0.18})]
    (doseq [lon (range -180 181 30)]
      (.add group (graticule-line (map (fn [lat] [lon lat]) (range -90 91 3)) material)))
    (doseq [lat (range -60 61 30)]
      (.add group (graticule-line (map (fn [lon] [lon lat]) (range -180 181 3)) material)))
    group))

(defn make-sphere []
  (let [geometry (three/SphereGeometry. earth-radius 96 64)
        material (three/MeshStandardMaterial.
                   #js {:color 0x10282d
                        :roughness 0.72
                        :metalness 0.08
                        :transparent true
                        :opacity 0.96})]
    (three/Mesh. geometry material)))

(defn apply-view! []
  (let [{:keys [globe camera rotation zoom]} @state
        ^js globe globe
        ^js camera camera]
    (when globe
      (set! (.. globe -rotation -x) (:x rotation))
      (set! (.. globe -rotation -y) (:y rotation)))
    (when camera
      (set! (.. camera -position -z) zoom))))

(defn resize! []
  (let [{:keys [camera renderer]} @state
        ^js camera camera
        ^js renderer renderer
        canvas (el "globe")
        width (.-clientWidth canvas)
        height (.-clientHeight canvas)]
    (when (and camera renderer (pos? height))
      (set! (.-aspect camera) (/ width height))
      (.updateProjectionMatrix camera)
      (.setSize renderer width height false))))

(defn animate! []
  (let [{:keys [renderer scene camera target]} @state]
    (when target
      (swap! state update :rotation
             (fn [{:keys [x y]}]
               {:x (+ x (* 0.08 (- (:x target) x)))
                :y (+ y (* 0.08 (- (:y target) y)))}))
      (let [{:keys [rotation]} @state]
        (when (and (< (js/Math.abs (- (:x rotation) (:x target))) 0.005)
                   (< (js/Math.abs (- (:y rotation) (:y target))) 0.005))
          (swap! state assoc :target nil))))
    (apply-view!)
    (when (and renderer scene camera)
      (.render renderer scene camera))
    (js/requestAnimationFrame animate!)))

(defn scene-setup! []
  (let [canvas (el "globe")
        scene (three/Scene.)
        camera (three/PerspectiveCamera. 40 1 0.1 100)
        renderer (three/WebGLRenderer. #js {:canvas canvas
                                            :antialias true
                                            :alpha true})
        globe (three/Group.)
        ambient (three/HemisphereLight. 0xf8f0cf 0x123e44 2.4)
        key (three/DirectionalLight. 0xffffff 2.2)]
    (.setPixelRatio renderer (min 2 (.-devicePixelRatio js/window)))
    (set! (.. key -position -x) -3)
    (set! (.. key -position -y) 4)
    (set! (.. key -position -z) 5)
    (.add globe (make-sphere))
    (.add globe (make-graticule))
    (.add scene ambient)
    (.add scene key)
    (.add scene globe)
    (swap! state assoc :scene scene :camera camera :renderer renderer :globe globe)
    (resize!)
    (apply-view!)))

(defn load-countries! []
  (-> (js/fetch country-data-url)
      (.then #(.json %))
      (.then (fn [topology]
               (let [^js objects (.-objects ^js topology)
                     countries (aget objects "countries")
                     feature-collection (topojson/feature topology countries)
                     features (array-seq (.-features feature-collection))
                     normal-material (three/LineBasicMaterial.
                                       #js {:color 0xf7efd9
                                            :transparent true
                                            :opacity 0.54})
                     antipode-material (three/LineBasicMaterial.
                                         #js {:color 0x4ff2e1
                                              :transparent true
                                              :opacity 0.9})
                     normal-lines (country-line-group features normal-material identity (+ earth-radius 0.018))
                     antipode-lines (country-line-group features antipode-material antipode (+ earth-radius 0.05))]
                 (set! (.-renderOrder antipode-lines) 2)
                 (.add (:globe @state) normal-lines)
                 (.add (:globe @state) antipode-lines)
                 (set-status! "Drag to rotate. Scroll to zoom."))))
      (.catch (fn [err]
                (js/console.error err)
                (set-status! "Could not load coastline data.")))))

(defn pointer-pos [event]
  {:x (.-clientX event) :y (.-clientY event)})

(defn on-pointer-down [event]
  (.setPointerCapture (el "globe") (.-pointerId event))
  (swap! state assoc :dragging? true :last-pointer (pointer-pos event) :target nil))

(defn on-pointer-move [event]
  (let [{:keys [dragging? last-pointer]} @state]
    (when (and dragging? last-pointer)
      (let [pos (pointer-pos event)
            dx (- (:x pos) (:x last-pointer))
            dy (- (:y pos) (:y last-pointer))]
        (swap! state
               (fn [s]
                 (-> s
                     (assoc :last-pointer pos)
                     (update :rotation
                             (fn [{:keys [x y]}]
                               {:x (-> (+ x (* dy 0.006)) (max -1.35) (min 1.35))
                                :y (+ y (* dx 0.006))})))))))))

(defn on-pointer-up [_]
  (swap! state assoc :dragging? false :last-pointer nil))

(defn on-wheel [event]
  (.preventDefault event)
  (let [delta (if (pos? (.-deltaY event)) 0.28 -0.28)]
    (swap! state update :zoom #(-> (+ % delta) (max 4.25) (min 9.5)))))

(defn focus! [lat lon]
  ;; In this sphere mapping, -90 longitude is the unrotated front center.
  (swap! state assoc :target {:x (deg->rad lat) :y (deg->rad (- (+ lon 90)))}))

(defn bind-events! []
  (let [canvas (el "globe")]
    (.addEventListener canvas "pointerdown" on-pointer-down)
    (.addEventListener canvas "pointermove" on-pointer-move)
    (.addEventListener canvas "pointerup" on-pointer-up)
    (.addEventListener canvas "pointercancel" on-pointer-up)
    (.addEventListener canvas "wheel" on-wheel #js {:passive false})
    (.addEventListener js/window "resize" resize!)
    (.addEventListener (el "argentina-button") "click" #(focus! -38 -64))
    (.addEventListener (el "china-button") "click" #(focus! 35 103))
    (.addEventListener (el "reset-button") "click"
                       #(swap! state assoc
                               :rotation {:x -0.32 :y -0.6}
                               :zoom 8.1
                               :target nil))))

(defn ^:export init []
  (scene-setup!)
  (bind-events!)
  (load-countries!)
  (animate!))
