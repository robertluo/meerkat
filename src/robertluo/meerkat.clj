(ns robertluo.meerkat
  "exprerimental LLM library"
  (:require 
   [sg.flybot.pullable :as pull]
   [wkok.openai-clojure.api :as ai]))

;; ## Chat completion
;; [OpenAI guide](https://platform.openai.com/docs/guides/chat) contains 
;; a lot of additional information, the following code try to translate these suggestion
;; into code.

(defn ai-env
  "create an AI environment"
  ;;TODO support Azure api
  [api-key]
  {:api-key (or api-key (System/getenv "OPENAI_API_KEY"))})

(defn chat-msg
  "returns a chat message with string `content` and keyword `role`(default to `:user`)"
  ([content]
   (chat-msg content :user))
  ([content role]
   (assert (#{:system :user :assistant} role) "illegal role")
   {:role (name role) :content content}))

^:rct/test
(comment
  (chat-msg "hello") ;=> {:role "user" :content "hello"}
  )

(let [chat-models    #{:gpt-4 :gpt-4-32k :gpt-3.5-turbo}
      system-prompt  "answer must in EDN format"
      default-config {:model    :gpt-3.5-turbo,
                      :prompt-token-max  1024,
                      :messages [(chat-msg system-prompt :system)]}
      q-response     (pull/qfn '{:choices [{:message ?msg}]} ?msg)
      fconj          (fn [coll item] (if item (conj coll item) coll))]
  (defn chat-data
    "chat-data contains everything when we chat with ChatGPT, including the AI environment,
     returns a updated from `prev-data` and a `new-message`,
     
      - `prev-data` chating data possibly returned from previous chat session.
      - `new-message` may be constructed by `chat-msg`, new chat message request"
    ([new-message]
     (chat-data new-message {}))
    ([new-message prev-data]
     (chat-data new-message prev-data default-config))
    ([new-message prev-data config]
     (let [answer (-> (:response prev-data) q-response)]
       (-> (dissoc prev-data :response)
           (update :request
                   (fn [req]
                     (let [req (merge config req)]
                       (assert (chat-models (:model req)) "illegal model of chat")
                       (update req :messages #(-> % (fconj answer) (fconj new-message)))))))))))

(defn chat
  "execute chat specified in `data` and returns it with `:response` from the server"
  [{env :env request :request :as data}]
  (when-let [response (ai/create-chat-completion request env)]
    (assoc data :response response)))

^:rct/test
(comment
  (def init-data (chat-data (chat-msg "Who won the world series in 2020?") {:env (ai-env nil)}))
  init-data  ;=>> {:request {:model :gpt-3.5-turbo :messages #(= 2 (count %))}}
  (chat init-data)
  (chat (chat-data (chat-msg "Where was it played?") *1))
  (chat (chat-data (chat-msg "answer it using :field, :city :state") *1))
  (chat (chat-data (chat-msg "summarize current conversation in 40 words or less") *1))
  )
