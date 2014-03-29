(ns entitas-clj.example
  (:require [entitas-clj.core :as cr]
            [entitas-clj.component :as c]
            [entitas-clj.collection :as cl]
            [entitas-clj.entity :as e]
            [entitas-clj.repository :as r]
            [entitas-clj.system :as s]
            [lanterna.screen :as ls]))

(defn create-player [x y]
  (let [entity-id :player
        ctype :position
        position (c/create ctype {:x x :y y :char "X"})]
    (e/add-component (e/create entity-id) position)))

(defn create-enemy [x y]
  (let [entity-id :enemy
        ctype :position
        position (c/create ctype {:x x :y y :char "E"})]
    (e/add-component (e/create entity-id) position)))

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

(defn create-input-emitter [screen]


  )

(defn execute-enemy-move-system [repository]
  (let [[new-repository rc] (r/collection-for-types repository #{:enemy})]
    (println "move enemy")
    repository
    )
  )

;; screen should be in render component
(defn execute-render-system [screen repository]
  (let [[new-repository rc] (r/collection-for-types repository #{:render})]
    (ls/clear screen)
    (doseq [entity (vals (cl/entities rc))]
      (let [component (e/component-of-type entity :position)
            {:keys [x y char]} (:data component)]
        (ls/put-string screen x y char)))
    (ls/redraw screen)
    new-repository))

(defn start [width height]
  (let [screen (ls/get-screen :swing {:cols width :rows height})
        ;; player
        player0 (create-player 0 0)
        player (e/add-component player0 (c/create :render nil))
        ;; enemy
        enemy0 (create-enemy 22 22)
        enemy (e/add-component enemy0 (c/create :render nil))
        ;; add
        repository (-> (r/create)
                       (r/add-entity ,, player)
                       (r/add-entity ,, enemy))
        ;; render system
        render-system (s/create :render-system #(execute-render-system screen %))
        enemy-system (s/create :enemy-system execute-enemy-move-system)
        systems [render-system enemy-system]]
    (ls/start screen)
    (loop [input (ls/get-key-blocking screen)
           state {:systems systems :repository repository}]
      (let [position-component (update-position player input)
            repository0 (cr/exchange-component (:repository state) player position-component)
            repository1 (s/execute (:systems state) repository0)]
        (recur (ls/get-key-blocking screen)
               (assoc state :repository repository1))))))

(start 40 40)
