(ns ws-ldn-10.speech
  (:require-macros
   [reagent.ratom :refer [reaction]]
   [cljs-log.core :refer [debug info warn]]
   [clojure.string :as str]
   [clojure.walk :as walk])
  (:require
   [reagent.core :as reagent]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [thi.ng.strf.core :as f]))

(declare clear-transcript!)

(enable-console-print!)

(defonce state (reagent/atom {}))

(def annotators
  {:number  {:match #"^(\-?\d+(\.\d+)?)|one$"
             :parse (fn [x] (if (= "one" x) 1 (f/parse-float x)))}
   :type    {:match #"^(background|point(s)?|particle(s)?|spring(s)?|cell(s)?|line(s)?|circle(s)?|dot(s)?|square(s)?|triangle(s)?)$"}
   :at      {:match #"^at$" :parse (constantly "@")}
   :keyword {:match #"^(define|random|range|from|to|distance|force|connect|avoid)$"}
   :color   {:match #"^(blue|red|green|yellow|pink|cyan|orange|purple|black|white)$"}
   :command {:match #"^(run|clear)$"
             :parse keyword}})

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

(defn analyze-token
  [token]
  (or (some
       (fn [[id {:keys [match parse]}]]
         (if (re-find match token)
           {:type id :value (if parse (parse token) token)}))
       annotators)
      {:type :unknown :value token}))

(defn format-token
  [token]
  (or (some
       (fn [[id regexp]]
         (if (re-find regexp token)
           [:span {:class (str "ann-" (name id) " ann-" (name id) "-" token)} token]))
       annotators)
      [:span.ann-unknown {:key (name (gensym))} token]))

(defn format-ast
  [ast]
  (walk/postwalk
   (fn [x]
     (if (map? x)
       (let [{:keys [type value]} x
             class (str "ann-" (name type))]
         (if (= :expr type)
           [:div {:key (name (gensym)) :class class} value]
           [:span {:key   (name (gensym))
                   :class (str class " " class "-" (:value x))}
            (:value x)]))
       x))
   ast))

(defn parse-phrase
  [phrase]
  {:type :expr :value (map analyze-token phrase)})

(defn ast-includes-command?
  [ast cmd]
  (->> ast
       (map :value)
       (mapcat identity)
       (some (fn [x] (and (= :command (:type x)) (= cmd (:value x)))))))

(def extract-phrases
  (comp (partition-by #(not (#{"ok" "okay"} %)))
        (take-nth 2)))

(defn parse-input
  [src]
  (when src
    (let [tokens  (str/split (str/lower-case src) #"\s+")
          phrases (sequence extract-phrases tokens)
          ast     (map parse-phrase phrases)]
      (if (ast-includes-command? ast :clear)
        (clear-transcript!)
        (do (swap! state assoc :ast ast)
            (swap! state assoc-in [:transcript :annotated] (format-ast ast)))))))

(defn update-transcript!
  [interim final]
  (swap! state update :transcript assoc :interim interim :final final)
  (parse-input final))

(def speech-supported?
  (not (nil? (or (aget js/window "SpeechRecognition")
                 (aget js/window "webkitSpeechRecognition")))))

(defn start-recognizer
  [{:keys [result error start end lang continuous interim]
    :or   {interim true, continuous true, lang "en-GB"}}]
  (let [rec (if (aget js/window "SpeechRecognition")
              (js/SpeechRecognition.)
              (js/webkitSpeechRecognition.))]
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

(defn init-regognizer
  []
  (swap! state assoc
         :recognizer (start-recognizer
                      {:result (result-handler update-transcript!)})
         :transcript nil))

(defn clear-transcript!
  []
  (stop-recognizer (:recognizer @state))
  (init-regognizer))

(defn transcript-comp
  []
  (let [transcript (reaction (:transcript @state))]
    (fn []
      [:div
       [:div.interim "> " (or (:interim @transcript) "listening...")]
       [:div.transcript (:annotated @transcript)]
       [:button {:on-click clear-transcript!} "Clear!"]])))

(defn no-speech-comp
  []
  [:div
   [:h1 "Sorry, speech recognition not supported!"]
   [:p "Please use Google Chrome (v25+)"]])

(defn main
  []
  (let [root (.getElementById js/document "app")]
    (if speech-supported?
      (do (init-regognizer)
          (reagent/render-component [transcript-comp] root))
      (reagent/render-component [no-speech-comp] root))))

(main)
