(ns entitas-clj.example
  (:require [entitas-clj.core :as cr]
            [entitas-clj.component :as c]
            [entitas-clj.entity :as e]
            [entitas-clj.repository :as r]
            [lanterna.screen :as s]))

(defn create-player [x y]
  (let [entity-id :player
        ctype :position
        position (c/create ctype {:x x :y y})]
    (e/add-component (e/create entity-id) position)))

(defn handle-input [{:keys [x y] :as position} input]
  (let [[new-x new-y] (case input
                        :left [(- x 1) y]
                        :right [(inc x) y]
                        :up [x (- y 1)]
                        :down [x (inc y)]
                        [x y])]
    (assoc position :x new-x :y new-y)))

(defn update-position [entity f input]
  (let [component (e/component-of-type entity :position)]
    (update-in component [:data] #(f % input))))

(defn render [screen data]
  (s/clear screen)
  (s/put-string screen (:x data) (:y data) "X")
  (s/redraw screen))

(defn start [width height]
  (let [screen (s/get-screen :swing {:cols width :rows height})
        player (create-player 0 0)
        repository0 (r/add-entity (r/create) player)]
    (s/start screen)
    (loop [input (s/get-key-blocking screen)
           repository repository0]
      (let [position-component (update-position player handle-input input)
            new-repository (cr/exchange-component repository player position-component)]
        (render screen (:data position-component))
        (recur (s/get-key-blocking screen) new-repository)))))
