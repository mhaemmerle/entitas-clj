(ns entitas-clj.example
  (:require [entitas-clj.core :as cr]
            [entitas-clj.component :as c]
            [entitas-clj.collection :as cl]
            [entitas-clj.entity :as e]
            [entitas-clj.repository :as r]
            [entitas-clj.system :as s]
            [lanterna.screen :as ls]
            [clojure.core.async :refer [chan go sliding-buffer put! alts! <! >!]]))

(defn create-player [x y]
  (let [entity-id :player
        position-comp (c/create :position {:x x :y y :char "X"})
        player-comp (c/create :player)]
    (e/create entity-id position-comp player-comp)))

(defn create-enemy [x y]
  (let [entity-id :enemy
        ctype :position
        position-comp (c/create ctype {:x x :y y :char "E"})]
    (e/create entity-id position-comp)))

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
      (let [ctype :key-press
            comp (c/create ctype {:input input})
            new-acc (conj acc (e/add-component (e/create nil) comp))]
        (recur (ls/get-key screen) new-acc))
      acc)))

(defn collect-input [repository screen]
  (reduce (fn [acc entity]
            (r/add-entity acc entity)) repository (input->entities screen)))

;; (defn create-input-emitter [screen ch]
;;   (go (while true
;;         (let [ctype :key-press
;;               input (ls/get-key screen)]
;;           (when-not (nil? input)
;;             (let [comp (c/create ctype {:input input})
;;                   ent (e/add-component (e/create nil) comp)]
;;               (>! ch ent)))))))

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

(defn start [width height]
  (let [screen (ls/get-screen :swing {:cols width :rows height})
        ;; player
        player0 (create-player 0 0)
        player (e/add-component player0 (c/create :render))
        ;; enemy
        enemy0 (create-enemy 22 22)
        enemy (e/add-component enemy0 (c/create :render))
        ;; repository
        repository (-> (r/create)
                       (r/add-entity ,, player)
                       (r/add-entity ,, enemy))
        input-system (s/create :input-system execute-input-system)
        enemy-system (s/create :enemy-system execute-enemy-move-system)
        render-system (s/create :render-system #(execute-render-system screen %))
        systems [input-system enemy-system render-system]]
    (ls/start screen)
    (loop [state {:systems systems :repository repository}]
      (let [repository0 (collect-input (:repository state) screen)
            repository1 (s/execute (:systems state) repository0)]
        (Thread/sleep 1000)
        (recur (assoc state :repository repository1))))))

(start 40 40)
