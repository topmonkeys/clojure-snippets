(ns clojure-snippets.twitter
  (:use
   [twitter.oauth]
   [twitter.callbacks]
   [twitter.utils]
   [twitter.request]
   [twitter.callbacks.handlers]
   [twitter.api.restful])
  (:import
   (twitter.callbacks.protocols SyncSingleCallback)))

;;; References
;; Twitter-API: https://github.com/adamwynne/twitter-api
;; Get the keys from https://dev.twitter.com/

; The API keys need to be maintained in a file named api-keys.txt at the root of the project
; and should be a map with the keys :app-key, :app-secret, :user-token and :user-token-secret.
;(def *api-keys* 
;  {:app-key "",
;   :app-secret ""
;   :user-token ""
;   :user-token-secret ""})
;(spit "api-keys.txt" *api-keys*)

; Read API Keys from the api-keys.txt file.
(def *api-keys* (read-string (slurp "api-keys.txt")))

; Create the OAuth Credentials.
(def *credentials* (make-oauth-creds (:app-key *api-keys*)
                                     (:app-secret *api-keys*)
                                     (:user-token *api-keys*)
                                     (:user-token-secret *api-keys*)))

(defn print-timeline [screen-name credentials]
  "Prints the items in the timeline of a user based on the screen name. 
   Usage: (print-timeline ""TopMonkeys"" *credentials*)"
  (let [timeline (statuses-user-timeline :oauth-creds credentials
                        :params {:screen-name screen-name})
         body (:body timeline)]
    (doseq [item body]
           (prn (:text item)))))

; This doesn't seem to work yet as I have not provided write access to the app. To enable
; this, I need to add a mobile number to the test account, but twitter doesn't seem to like
; my mobile. :-P
(defn update-status [credentials status]
  "Updates the status to the user"
  (statuses-update :oauth-creds credentials
                   :body [(status-body-part status)]))
