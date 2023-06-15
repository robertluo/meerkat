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
  {:api-key    (or api-key (System/getenv "OPENAI_API_KEY"))
   :model/chat :gpt-3.5-turbo})

(defn- into-when
  ([coll item]
   (into-when coll item identity))
  ([coll item f-item]
   (into-when coll item conj f-item))
  ([coll item f-coll f-item] 
   (if item (f-coll coll (f-item item)) coll)))

(defn- chat-msg
  "returns a chat message with string `content` and keyword `role`(default to `:user`)"
  ([content]
   (chat-msg content :user))
  ([content role]
   (assert (#{:system :user :assistant} role) "illegal role")
   {:role (name role) :content content}))

(defn- prompt->msgs
  "returns a request from a `prompt`"
  [{:prompt/keys [user system history]}]
  {:messages 
   (-> []
       (into-when system #(chat-msg % :system))
       (into-when history into #(mapv (fn [{:prompt/keys [user assistant]}] (apply chat-msg (if user [user] [assistant :assistant]))) %))
       (into-when user chat-msg))})

^:rct/test
(comment
  (prompt->msgs #:prompt{:user "hello"}) ;=> {:messages [{:role "user" :content "hello"}]}
  (prompt->msgs #:prompt{:system "ok" :user "hello"}) ;=> {:messages [{:role "system" :content "ok"} {:role "user" :content "hello"}]}
  (prompt->msgs #:prompt{:system "ok" :user "hello" :history [#:prompt{:user "history"} #:prompt{:assistant "no comment"}]}) 
  ;=> {:messages [{:role "system", :content "ok"} {:role "user", :content "history"} {:role "assistant" :content "no comment"} {:role "user", :content "hello"}]}
  )

(defn gpt-of
  "returns a response of an `env` using chatGPT's chat completion"
  [{:keys [model/chat] :as env}]
  (fn [prompt]
    (ai/create-chat-completion (assoc prompt :model chat) env)))

(def ^:private q-response 
  "returns new prompt and other interesting information (token) in a gpt response"
  (pull/qfn 
   '{:usage {:prompt_tokens ?pt :completion_tokens ?ct :total_tokens ?tt}
     :choices [{:message {:role "assistant" :content ?content}, :finish_reason "stop"}]}
   {:token/prompt ?pt :token/completion ?ct :token/total ?tt
    :prompt/assistant ?content}))

(defn chat-completion
  ([f prompt]
   (when-let [response (-> (prompt->msgs prompt) f (q-response))]
     (-> prompt 
         (assoc :response response)
         (dissoc :prompt/user)
         (update :prompt/history 
                 (fn [cur]
                   (-> (or cur [])
                       (into-when (select-keys prompt [:prompt/user]))
                       (into-when response))))))))

(comment
  (def prog-instruction 
    "Please write Clojure code for my request, The response must start directly
     with the code, without human language opening. Any human language explanations must
     be embedded within comments.
      - parenthesis, brackets matches.
      - pass the `tests`")
  
  (def example-1
    {:code
     '(defn square "returns a number which is the squre of `x`" [x])
     :tests
     '[(= 9 (square 3))]
     :expected-assistant-response
     '(defn square "returns a number which is the squre of `x`" [x] (* x x))}) 
  
  (def example-2
    {:code
     '(defn my-odd? "predict if `x` is a odd number" [x])
     :tests
     '[(= false (my-odd? 2))
       (= true (my-odd? 1))]
     :expected-assistant-response
     '(defn my-odd? "predict if `x` is a odd number" [x] (= 1 (mod x 2)))})

  
  (def prog-sys-prompt (str prog-instruction "\nexample:" (pr-str example-1)
                            "\n" (pr-str example-2)))
  (def user-code-1
    {:code
     '(defn lcm "returns the least common multiple number of `a` and `b`" [a b])
     :tests
     '[(= 2 (lcm 1 2))
       (= 30 (lcm 5 6))
       (= 20 (lcm 10 4))]})
  
  (def user-code-2
    {:code
     '(defn read-all "returns a vector contains all clojure forms in `s`" [s])
     :tests
     '[(= [3] (read-all "3"))
       (= [:a 5] (read-all ":a 5"))]})
  
  (def rtn (chat-completion (gpt-of (ai-env nil)) 
                            #:prompt{:system prog-sys-prompt :user (pr-str user-code-2)})) 
  (-> rtn (get-in [:response :prompt/assistant]))
  )