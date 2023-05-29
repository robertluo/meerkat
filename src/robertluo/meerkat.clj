(ns robertluo.meerkat
  "exprerimental LLM library"
  (:require 
   [sg.flybot.pullable :as pull]
   [wkok.openai-clojure.api :as ai]))

(defn ai-env
  "create an AI environment"
  [api-key]
  {:api-key (or api-key (System/getenv "OPENAI_API_KEY"))})

(defn chat-msg
  "construct a chat message
   https://platform.openai.com/docs/guides/chat"
  ([content]
   (chat-msg content :user))
  ([content role]
   (assert (#{:system :user :assistant} role) "illegal role")
   {:role (name role) :content content}))

^:rct/test
(comment
  (chat-msg "hello") ;=> {:role "user" :content "hello"}
  )

(def chat-models
  "models can be used in chat"
  #{:gpt-4 :gpt-4-32k :gpt-3.5-turbo})

(let [default {:model :gpt-3.5-turbo,
               :messages [(chat-msg "answer must in EDN format" :system)]}
      q-response (pull/qfn '{:choices [{:message ?msg}]} ?msg)
      fconj (fn [coll item] (if item (conj coll item) coll))]
  (defn chat-data
    "construct chat data"
    ([content]
     (chat-data content {}))
    ([content prev-data]
     (let [answer (-> (:response prev-data) q-response)]
       (-> (dissoc prev-data :response)
           (update :request
                   (fn [req]
                     (let [req (merge default req)]
                       (assert (chat-models (:model req)) "illegal model of chat")
                       (update req :messages #(-> % (fconj answer) (fconj (chat-msg content))))))))))))

(defn chat
  "execute chat specified in `data` and returns it with `:response` from the server"
  [{env :env request :request :as data}]
  (when-let [response (ai/create-chat-completion request env)]
    (assoc data :response response)))

^:rct/test
(comment
  (def init-data (chat-data "Who won the world series in 2020?" {:env (ai-env nil)}))
  init-data  ;=>> {:request {:model :gpt-3.5-turbo :messages #(= 2 (count %))}}
  (chat init-data)
  (chat (chat-data "Where was it played?" *1))
  (chat (chat-data "answer it using :field, :city :state" *1))
  )
