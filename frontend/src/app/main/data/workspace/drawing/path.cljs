;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020 UXBOX Labs SL

(ns app.main.data.workspace.drawing.path
  (:require
   [beicon.core :as rx]
   [potok.core :as ptk]
   [app.common.math :as mth]
   [app.common.geom.point :as gpt]
   [app.common.geom.shapes :as gsh]
   [app.util.data :as d]
   [app.util.geom.path :as ugp]
   [app.main.streams :as ms]
   [app.main.data.workspace.common :as dwc]
   [app.main.data.workspace.drawing.common :as common]
   [app.common.geom.shapes.path :as gsp]))

;;;;

(def close-path-distance 5)

(defn seek-start-path [content]
  (->> content
       reverse
       (d/seek (fn [{cmd :command}] (= cmd :move-to)))
       :params))

(defn next-node
  "Calculates the next-node to be inserted."
  [shape position prev-point prev-handler]
  (let [last-command (-> shape :content last :command)
        start-point  (-> shape :content seek-start-path)

        add-line?   (and prev-point (not prev-handler) (not= last-command :close-path))
        add-curve?  (and prev-point prev-handler (not= last-command :close-path))
        close-path? (and start-point
                         (< (mth/abs (gpt/distance (gpt/point start-point)
                                                   (gpt/point position)))
                            close-path-distance))]
    (cond
      close-path? {:command :close-path
                   :params []}
      add-line?   {:command :line-to
                   :params position}
      add-curve?  {:command :curve-to
                   :params (ugp/make-curve-params position prev-handler)}
      :else       {:command :move-to
                   :params position})))

(defn append-node
  "Creates a new node in the path. Usualy used when drawing."
  [shape position prev-point prev-handler]
  (let [command (next-node shape position prev-point prev-handler)]
    (as-> shape $
      (update $ :content (fnil conj []) command)
      (update $ :selrect (gsh/content->selrect (:content $))))))

(defn suffix-keyword [kw suffix]
  (let [strkw (if kw (name kw) "")]
    (keyword (str strkw suffix))))

;; handler-type => :prev :next
(defn move-handler [shape index handler-type match-opposite? position]
  (let [content (:content shape)
        [command next-command] (-> (d/with-next content) (nth index))

        update-command
        (fn [{cmd :command params :params :as command} param-prefix prev-command]
          (if (#{:line-to :curve-to} cmd)
            (let [command (if (= cmd :line-to)
                            {:command :curve-to
                             :params (ugp/make-curve-params params (:params prev-command))}
                            command)]
              (-> command
                  (update :params assoc
                          (suffix-keyword param-prefix "x") (:x position)
                          (suffix-keyword param-prefix "y") (:y position))))
            command))

        update-content
        (fn [shape index prefix]
          (if (contains? (:content shape) index)
            (let [prev-command (get-in shape [:content (dec index)])]
              (update-in shape [:content index] update-command prefix prev-command))

            shape))]

    (cond-> shape
      (= :prev handler-type)
      (update-content index :c2)

      (and (= :next handler-type) next-command)
      (update-content (inc index) :c1)

      match-opposite?
      (move-handler
       index
       (if (= handler-type :prev) :next :prev)
       false
       (ugp/opposite-handler (gpt/point (:params command))
                             (gpt/point position))))))


;;;;
(defn finish-event? [{:keys [type shift] :as event}]
  (or (= event ::end-path-drawing)
      (= event :interrupt)
      (and (ms/keyboard-event? event)
           (= type :down)
           (= 13 (:key event)))))

#_(defn init-path []
  (fn [state]
    (update-in state [:workspace-drawing :object]
               assoc :content []
               :initialized? true)))

#_(defn add-path-command [command]
  (fn [state]
    (update-in state [:workspace-drawing :object :content] conj command)))

#_(defn update-point-segment [state index point]
  (let [segments (count (get-in state [:workspace-drawing :object :segments]))
        exists? (< -1 index segments)]
    (cond-> state
      exists? (assoc-in [:workspace-drawing :object :segments index] point))))

#_(defn finish-drawing-path []
  (fn [state]
    (update-in
     state [:workspace-drawing :object]
     (fn [shape] (-> shape
                     (update :segments #(vec (butlast %)))
                     (gsh/update-path-selrect))))))


(defn calculate-selrect [shape]
  (assoc shape
         :points (gsh/content->points (:content shape))
         :selrect (gsh/content->selrect (:content shape))))

(defn init-path []
  (ptk/reify ::init-path
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (assoc-in [:workspace-drawing :object :initialized?] true)
          (assoc-in [:workspace-local :edit-path :last-point] nil)))))

(defn finish-path []
  (ptk/reify ::finish-path
    ptk/UpdateEvent
    (update [_ state]
      (-> state
          (update :workspace-local dissoc :edit-path)
          (update-in [:workspace-drawing :object] calculate-selrect)))))

(defn preview-next-point [{:keys [x y]}]
  (ptk/reify ::add-node
    ptk/UpdateEvent
    (update [_ state]
      (let [position (gpt/point x y)
            {:keys [last-point prev-handler] :as shape} (get-in state [:workspace-local :edit-path])
            command (next-node shape position last-point prev-handler)]
        (assoc-in state [:workspace-local :edit-path :preview] command)))))

(defn add-node [{:keys [x y]}]
  (ptk/reify ::add-node
    ptk/UpdateEvent
    (update [_ state]

      (let [position (gpt/point x y)
            {:keys [last-point prev-handler]} (get-in state [:workspace-local :edit-path])]
        (-> state
            (assoc-in  [:workspace-local :edit-path :last-point] position)
            (update-in [:workspace-local :edit-path] dissoc :prev-handler)
            (update-in [:workspace-drawing :object] append-node position last-point prev-handler))))))

(defn drag-handler [{:keys [x y]}]
  (ptk/reify ::drag-handler
    ptk/UpdateEvent
    (update [_ state]

      (let [position (gpt/point x y)
            shape (get-in state [:workspace-drawing :object])
            index (dec (count (:content shape)))]
        (-> state
            (update-in [:workspace-drawing :object] move-handler index :next true position)
            (assoc-in [:workspace-local :edit-path :drag-handler] position))))))

(defn finish-drag []
  (ptk/reify ::finish-drag
    ptk/UpdateEvent
    (update [_ state]
      (let [handler (get-in state [:workspace-local :edit-path :drag-handler])]
        (-> state
            (update-in [:workspace-local :edit-path] dissoc :drag-handler)
            (assoc-in [:workspace-local :edit-path :prev-handler] handler))))))

(defn make-click-stream
  [stream down-event]
  (->> stream
       (rx/filter ms/mouse-click?)
       (rx/debounce 200)
       (rx/first)
       (rx/map #(add-node down-event))))

(defn make-drag-stream
  [stream down-event]
  (let [mouse-up    (->> stream (rx/filter ms/mouse-up?))
        drag-events (->> ms/mouse-position
                         (rx/take-until mouse-up)
                         (rx/map #(drag-handler %)))]
    (->> (rx/timer 400)
         (rx/merge-map #(rx/concat
                         (rx/of (add-node down-event))
                         drag-events
                         (rx/of (finish-drag)))))))

(defn make-dbl-click-stream
  [stream down-event]
  (->> stream
       (rx/filter ms/mouse-double-click?)
       (rx/first)
       (rx/merge-map
        #(rx/of (add-node down-event)
                ::end-path-drawing))))

(defn handle-drawing-path []
  (ptk/reify ::handle-drawing-path
    ptk/WatchEvent
    (watch [_ state stream]

      (let [mouse-down    (->> stream (rx/filter ms/mouse-down?))
            finish-events (->> stream (rx/filter finish-event?))

            mousemove-events
            (->> ms/mouse-position
                 (rx/take-until finish-events)
                 (rx/throttle 100)
                 (rx/map #(preview-next-point %)))

            mousedown-events
            (->> mouse-down
                 (rx/take-until finish-events)
                 (rx/throttle 100)
                 (rx/with-latest merge ms/mouse-position)

                 ;; We change to the stream that emits the first event
                 (rx/switch-map
                  #(rx/race (make-click-stream stream %)
                            (make-drag-stream stream %)
                            (make-dbl-click-stream stream %))))]


        (rx/concat
         (rx/of (init-path))
         (rx/merge mousemove-events
                   mousedown-events)
         (rx/of (finish-path))
         (rx/of common/handle-finish-drawing))))))

#_(def handle-drawing-path
  (ptk/reify ::handle-drawing-path
    ptk/WatchEvent
    (watch [_ state stream]
      (let [{:keys [flags]} (:workspace-local state)

            last-point (volatile! @ms/mouse-position)

            stoper (->> (rx/filter stoper-event? stream)
                        (rx/share))

            mouse (rx/sample 10 ms/mouse-position)

            points (->> stream
                        (rx/filter ms/mouse-click?)
                        (rx/filter #(false? (:shift %)))
                        (rx/with-latest vector mouse)
                        (rx/map second))

            counter (rx/merge (rx/scan #(inc %) 1 points) (rx/of 1))

            stream' (->> mouse
                         (rx/with-latest vector ms/mouse-position-ctrl)
                         (rx/with-latest vector counter)
                         (rx/map flatten))

            imm-transform #(vector (- % 7) (+ % 7) %)
            immanted-zones (vec (concat
                                 (map imm-transform (range 0 181 15))
                                 (map (comp imm-transform -) (range 0 181 15))))

            align-position (fn [angle pos]
                             (reduce (fn [pos [a1 a2 v]]
                                       (if (< a1 angle a2)
                                         (reduced (gpt/update-angle pos v))
                                         pos))
                                     pos
                                     immanted-zones))]

        (rx/merge
         (rx/of #(initialize-drawing % @last-point))

         (->> points
              (rx/take-until stoper)
              (rx/map (fn [pt] #(insert-point-segment % pt))))

         (rx/concat
          (->> stream'
               (rx/take-until stoper)
               (rx/map (fn [[point ctrl? index :as xxx]]
                         (let [point (if ctrl?
                                       (as-> point $
                                         (gpt/subtract $ @last-point)
                                         (align-position (gpt/angle $) $)
                                         (gpt/add $ @last-point))
                                       point)]
                           #(update-point-segment % index point)))))
          (rx/of finish-drawing-path
                 common/handle-finish-drawing)))))))

#_(defn close-drawing-path []
  (ptk/reify ::close-drawing-path
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-drawing :object :close?] true))

    ptk/WatchEvent
    (watch [_ state stream]
      (rx/of ::end-path-drawing))))


(defn stop-path-edit []
  (ptk/reify ::stop-path-edit
    ptk/UpdateEvent
    (update [_ state]
      (update state :workspace-local dissoc :edit-path))))

(defn start-path-edit
  [id]
  (ptk/reify ::start-path-edit
    ptk/UpdateEvent
    (update [_ state]
      (assoc-in state [:workspace-local :edit-path] {}))

    ptk/WatchEvent
    (watch [_ state stream]
      (->> stream
           (rx/filter #(= % :interrupt))
           (rx/take 1)
           (rx/map #(stop-path-edit))))))

(defn modify-point [index dx dy]
  (ptk/reify ::modify-point

    ptk/UpdateEvent
    (update [_ state]

      (-> state
          (update-in [:workspace-local :edit-path :content-modifiers (inc index)] assoc
                     :c1x dx :c1y dy)
          (update-in [:workspace-local :edit-path :content-modifiers index] assoc
                     :x dx :y dy :c2x dx :c2y dy)
          ))))

(defn modify-handler [index type dx dy]
  (ptk/reify ::modify-point
    ptk/UpdateEvent
    (update [_ state]
      (let [s1 (if (= type :prev) -1 1)
            s2 (if (= type :prev) 1 -1)]
        (-> state
            (update-in [:workspace-local :edit-path :content-modifiers (inc index)] assoc
                       :c1x (* s1 dx) :c1y (* s1 dy))
            (update-in [:workspace-local :edit-path :content-modifiers index] assoc
                       :c2x (* s2 dx) :c2y (* s2 dy) ))
        ))))

(defn apply-content-modifiers []
  (ptk/reify ::apply-content-modifiers
    ;;ptk/UpdateEvent
    ;;(update [_ state]
    ;;  (update-in state [:workspace-local :edit-path] dissoc :content-modifiers))

    ptk/WatchEvent
    (watch [_ state stream]
      (let [id (get-in state [:workspace-local :edition])
            page-id (:current-page-id state)
            old-content (get-in state [:workspace-data :pages-index page-id :objects id :content])
            old-selrect (get-in state [:workspace-data :pages-index page-id :objects id :selrect])
            content-modifiers (get-in state [:workspace-local :edit-path :content-modifiers])
            new-content (gsp/apply-content-modifiers old-content content-modifiers)
            new-selrect (gsh/content->selrect new-content)
            rch [{:type :mod-obj
                  :id id
                  :page-id page-id
                  :operations [{:type :set :attr :content :val new-content}
                               {:type :set :attr :selrect :val new-selrect}]}]

            uch [{:type :mod-obj
                  :id id
                  :page-id page-id
                  :operations [{:type :set :attr :content :val old-content}
                               {:type :set :attr :selrect :val old-selrect}]}]]

        (rx/of (dwc/commit-changes rch uch {:commit-local? true})
               (fn [state] (update-in state [:workspace-local :edit-path] dissoc :content-modifiers)))))))

(defn start-move-path-point
  [index]
  (ptk/reify ::start-move-path-point
    ptk/WatchEvent
    (watch [_ state stream]
      (let [start-point @ms/mouse-position
            start-delta-x (get-in state [:workspace-local :edit-path :content-modifiers index :x] 0)
            start-delta-y (get-in state [:workspace-local :edit-path :content-modifiers index :y] 0)]
        (rx/concat
         (->> ms/mouse-position
              (rx/take-until (->> stream (rx/filter ms/mouse-up?)))
              (rx/map #(modify-point
                        index
                        (+ start-delta-x (- (:x %) (:x start-point)))
                        (+ start-delta-y (- (:y %) (:y start-point))))))
         (rx/concat (rx/of (apply-content-modifiers)))
         )))))

(defn start-move-handler
  [index type]
  (ptk/reify ::start-move-handler
    ptk/WatchEvent
    (watch [_ state stream]
      (let [[cx cy] (if (= :prev type) [:c2x :c2y] [:c1x :c1y])
            cidx (if (= :prev type) index (inc index))

            start-point @ms/mouse-position
            start-delta-x (get-in state [:workspace-local :edit-path :content-modifiers cidx cx] 0)
            start-delta-y (get-in state [:workspace-local :edit-path :content-modifiers cidx cy] 0)]

        (rx/concat
         (->> ms/mouse-position
              (rx/take-until (->> stream (rx/filter ms/mouse-up?)))
              (rx/map #(modify-handler
                        index
                        type
                        (+ start-delta-x (- (:x %) (:x start-point)))
                        (+ start-delta-y (- (:y %) (:y start-point)))))
              )
         (rx/concat (rx/of (apply-content-modifiers))))))))
