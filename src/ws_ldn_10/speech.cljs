(ns ws-ldn-10.speech
  (:require-macros
   [reagent.ratom :refer [reaction]]
   [cljs-log.core :refer [debug info warn]])
  (:require
   [reagent.core :as reagent]))

(enable-console-print!)

(defonce state (reagent/atom {}))

(def has-speech?
  (not (nil? (aget js/window "webkitSpeechRecognition"))))

(defn start-recognizer
  [{:keys [result error start end lang continuous interim]
    :or   {interim true, continuous true, lang "en-US"}}]
  (let [rec (js/webkitSpeechRecognition.)]
    (set! (.-continuous rec) continuous)
    (set! (.-interimResults rec) interim)
    (set! (.-onresult rec) result)
    (when lang (set! (.-lang rec) lang))
    (when start (set! (.-onstart rec) start))
    (when end (set! (.-onend rec) end))
    (when error (set! (.-onerror rec) error))
    (.start rec)
    rec))

(defn stop-recognizer
  [rec] (.stop rec))

(defn result-handler
  [update-state]
  (fn [e]
    (let [len (-> e .-results .-length)]
      (loop [i 0, interim nil, final nil]
        (if (< i len)
          (let [res  (aget (.-results e) i)
                item (.-transcript (aget res 0))]
            (if (.-isFinal res)
              (recur (inc i) interim (str final item))
              (recur (inc i) (str interim item) final)))
          (update-state interim final))))))

(defn update-transcript!
  [interim final]
  (swap! state update :transcript assoc :interim interim :final final)
  (.log js/console "interim:" interim)
  (.log js/console "final:" final))

(defn clear-transcript!
  [] (swap! state dissoc :transcript))

(defn transcript-comp
  []
  (let [transcript (reaction (:transcript @state))]
    (fn []
      (if @transcript
        [:div
         [:label "Interim:"]
         [:h2 {:style {:color "#999"}} (:interim @transcript)]
         [:label "Final:"]
         [:h2 (:final @transcript)]
         [:button {:on-click clear-transcript!} "Clear!"]]
        [:div "Waiting..."]))))

(defn main
  []
  (let [rec (start-recognizer
             {:result (result-handler update-transcript!)})]
    (swap! state assoc :recognizer rec)
    (reagent/render-component
     [transcript-comp]
     (.getElementById js/document "app"))))

(main)
