(ns schlagly.io.core)

(enable-console-print!)

(defonce app-state (atom {:ticks 0  ; 
                          :speed 1
                          :spinning? false
                          :spin-handler nil
                          :brake-handler nil
                          :data []}))

(declare stop-wheel brake-wheel start-wheel)

(def pi js/Math.PI)
(def center-x 200)
(def center-y 200)
(def radius 150)

(defn get-ctx []
  (let [canvas (.getElementById js/document "canvas")]
    (.getContext canvas "2d")))


(defn elem [id]
   (.getElementById js/document id))

(defn deg->rad [d]
  (* 2 pi (/ d 360)))


(defn get-current-value []
  (let [data (:data @app-state)
        tick (get @app-state :ticks)
        translated-tick (+ tick 90)
        t (mod translated-tick 360)]
    (get data
         (- (count data)
            (inc
             (int (* (count data) (/ t 360))))))))


(def colors ["#87a96b"
             "#EEE8AA"
             "#FFFACD"
             "#B0C4DE"
             "#BDB76B"
             "#90EE90"
             "#F0FFF0"
             "#F4A460"
             "#E0FFFF"
             "#B0E0E6"
             "#87CEEB"
             "#D8BFD8"])

(defn get-color [idx num-items]
  (get colors (mod idx (count colors))))

(defn display-result []
  (set! (.-innerText (elem "result"))
        (str "You got " (get-current-value) "!")))

(defn clear-result []
    (set! (.-innerText (elem "result")) ""))

;;;
;;; Drawing functions
;;;

(defn draw-arrow [ctx]
  (.beginPath ctx)
  (.moveTo ctx (- center-x 15) (- center-y (+ radius 30)))
  (.lineTo ctx (+ center-x 15) (- center-y (+ radius 30)))
  (.lineTo ctx center-x (- center-y (+ radius 10)))
  (set! (.-fillStyle ctx) "black")
  (.fill ctx))


(defn draw-label [ctx label angle]
  (let [v (deg->rad angle)]
    (.save ctx)
    (.translate ctx center-x center-y)
    (.rotate ctx (+ v (* 0.5 pi)))
    (set! (.-fillStyle ctx) "black")
    (set! (.-font ctx) "11px Arial")
    (set! (.-textAlign ctx) "center")
    (.fillText ctx label 0 -120)
    (.translate ctx (- center-x) (- center-y))
    (.restore ctx)))


(defn draw-segment [ctx label start-angle end-angle color]
  (let [mid-angle (+ start-angle (* 0.5 (- end-angle start-angle)))
        start (deg->rad start-angle)
        end (deg->rad end-angle)]
    (.beginPath ctx)
    (set! (.-strokeStyle ctx) "#000")
    (.moveTo ctx center-x center-y)
    (.lineTo ctx
             (+ center-x (* radius (.cos js/Math start)))
             (+ center-y (* radius (.sin js/Math start))))
    (.arc ctx center-x center-y radius start end)
    (.moveTo ctx center-x center-y)
    (.lineTo ctx
             (+ center-x (* radius (.cos js/Math end)))
             (+ center-y (* radius (.sin js/Math end))))

    (set! (.-fillStyle ctx) color)
    (.fill ctx)
    (.stroke ctx)
    (draw-label ctx label mid-angle)))


(defn draw-wheel []
  (let [data (:data @app-state)
        num-items (count data)
        segment-size (/ 360 num-items)
        ctx (get-ctx)
        current-tick (get @app-state :ticks)]
    (draw-arrow ctx)
    (dotimes [i num-items]
      (let [start (+ current-tick (* i segment-size))
            end (+ start segment-size)
            color (get-color i num-items)]
        (draw-segment ctx (get data i) start end color)))))



;;;
;;; Helper functions for easily updating app state
;;;

(defn set-speed! [speed]
  (swap! app-state assoc :speed speed))

(defn set-tick! [tick]
  (swap! app-state assoc :ticks tick))

(defn set-data! [v]
  (when (vector? v)
    (swap! app-state assoc :data v)))


(defn enable-button []
  (set! (.-disabled (elem "btn")) false))

(defn disable-button []
  (set! (.-disabled (elem "btn")) true))


(defn ^:export button-handler []
  (start-wheel)
  (disable-button))


(defn decrease-speed []
  (let [new-speed (:speed (swap! app-state update-in [:speed] dec))]
    (when (zero? new-speed)
      (stop-wheel))))


(defn ticker-fn []
  (swap! app-state update-in [:ticks]
         (fn [tick] (+ tick (:speed @app-state))))
  (draw-wheel))


;;;
;;; Main control functions
;;;

(defn brake-wheel []
  (swap! app-state assoc
         :brake-handler (js/setInterval decrease-speed 100)))


(defn stop-wheel []
  (when-let [spin-handler (:spin-handler @app-state)]
    (js/clearInterval spin-handler))
  (when-let [brake-handler (:brake-handler @app-state)]
    (js/clearInterval brake-handler))
  (enable-button)
  (swap! app-state assoc
         :speed 0
         :spinning? false
         :spin-handler nil
         :brake-handler nil)
  (display-result))


(defn start-wheel []
  (when-not (:spinning? @app-state)
    (clear-result)
    (swap! app-state assoc
           :spin-handler (js/setInterval ticker-fn 30)
           :speed 20
           :spinning? true)
    (let [random-spin-time (+ 10 (rand-int 1500))]
      (js/setTimeout brake-wheel random-spin-time))))


(defn reset-app []
  (stop-wheel)
  (draw-wheel)
  (clear-result))

(defn ^:export start [data]
  (set-data! (vec data))
  (reset-app))

(defn on-js-reload []
  (reset-app))
