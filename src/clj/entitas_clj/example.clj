(ns entitas-clj.example
  (:require [entitas-clj.core :as cr]
            [entitas-clj.component :as c]
            [entitas-clj.collection :as cl]
            [entitas-clj.entity :as e]
            [entitas-clj.repository :as r]
            [entitas-clj.system :as s]
            [lanterna.screen :as ls]
            [clojure.core.async :refer [chan go put! alts! <! >! timeout]]))

(def timeout-value 50)

(defn create-player [x y]
  (let [position-comp (c/create :position {:x x :y y :char "X"})
        player-comp (c/create :player)]
    (e/create :player position-comp player-comp)))

(defn create-enemy [x y]
  (let [position-comp (c/create :position {:x x :y y :char "E"})]
    (e/create :enemy position-comp)))

(defn handle-input [{:keys [x y] :as position} input]
  (let [[new-x new-y] (case input
                        :left [(- x 1) y]
                        :right [(inc x) y]
                        :up [x (- y 1)]
                        :down [x (inc y)]
                        [x y])]
    (assoc position :x new-x :y new-y)))

(defn update-position [entity input]
  (let [component (e/component-of-type entity :position)]
    (update-in component [:data] handle-input input)))

(defn input->entities [screen]
  (loop [input (ls/get-key screen)
         acc []]
    (if (not (nil? input))
      (let [comp (c/create :key-press {:input input})
            new-acc (conj acc (e/add-component (e/create nil) comp))]
        (recur (ls/get-key screen) new-acc))
      acc)))

(defn collect-input [repository screen]
  (let [input-entities (input->entities screen)]
    (reduce (fn [acc entity]
              (r/add-entity acc entity)) repository input-entities)))

(defn execute-enemy-move-system [repository]
  (let [[new-repository rc] (r/collection-for-types repository #{:enemy})]
    new-repository))

(defn get-player-entity [repository]
  (let [[r0 pc] (r/collection-for-types repository #{:player})
        player (first (cl/entities pc))]
    [r0 player]))

(defn execute-input-system [repository]
  (let [[r0 player] (get-player-entity repository)
        [new-repository rc] (r/collection-for-types r0 #{:key-press})]
    (reduce (fn [acc entity]
              (let [input (:data (e/component-of-type entity :key-press))
                    position-component (update-position player (:input input))
                    r1 (cr/exchange-component acc player position-component)
                    r2 (r/remove-entity r1 entity)]
                r2)) new-repository (cl/entities rc))))

;; screen should be in render component
(defn execute-render-system [screen repository]
  (let [[new-repository rc] (r/collection-for-types repository #{:render})]
    (ls/clear screen)
    (doseq [entity (cl/entities rc)]
      (let [component (e/component-of-type entity :position)
            {:keys [x y char]} (:data component)]
        (ls/put-string screen x y char)))
    (ls/redraw screen)
    new-repository))

(defn initial-state [width height]
  (let [player (-> (create-player 0 0) (e/add-component ,, (c/create :render)))
        enemy (-> (create-enemy 22 22) (e/add-component ,, (c/create :render)))
        repository (-> (r/create) (r/add-entity ,, player) (r/add-entity ,, enemy))
        input-system (s/create :input-system execute-input-system)
        enemy-system (s/create :enemy-system execute-enemy-move-system)
        screen (ls/get-screen :swing {:cols width :rows height})
        render-system (s/create :render-system #(execute-render-system screen %))
        systems [input-system enemy-system render-system]]
    (ls/start screen)
    {:systems systems :repository repository :screen screen}))

(defn start [width height]
  (let [command-chan (chan)]
    (go
     (loop [state (initial-state width height)
            timer (timeout timeout-value)]
       (let [[v c] (alts! [timer command-chan])]
         (condp = c
           command-chan (when v
                      (recur state timer))
           timer (let [repository0 (collect-input (:repository state) (:screen state))
                       repository1 (s/execute (:systems state) repository0)]
                   (recur (assoc state :repository repository1) (timeout timeout-value)))))))))

(start 40 40)
