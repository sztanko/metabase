(ns metabase.sync.util-test
  (:require [clj-time.coerce :as tcoerce]
            [expectations :refer :all]
            [metabase.sync :as sync]
            [metabase.sync.interface :as i]
            [metabase.models.database :as mdb]
            [metabase.sync.util :refer :all]))

(defn call-with-operation-info
  "Call `f` with `log-sync-summary` redef'd to intercept the step metadata before the information is logged. This is
  useful to validate that the metadata is correct as the message might not be logged at all (depending on the logging
  level)."
  [f]
  (let [step-info-atom (atom [])
        orig-fn (var-get #'metabase.sync.util/log-sync-summary)]
    (with-redefs [metabase.sync.util/log-sync-summary (fn [operation database {:keys [steps] :as operation-metadata}]
                                                        (swap! step-info-atom conj operation-metadata)
                                                        (orig-fn operation database operation-metadata))]
      (f))
    @step-info-atom))

(defn sync-database!
  "Calls `sync-database!` and returns the the metadata for `step` as the result. This function is useful for
  validating that each steps metadata correctly reflects the changes that were made via a test scenario."
  [step db]
  (let [operation-results (call-with-operation-info #(sync/sync-database! db))]
    (-> (into {} (mapcat :steps operation-results))
        (get step))))

(defn only-step-keys
  "This function removes the generic keys for the step metadata, returning only the step specific keypairs to make
  validating the results for the given step easier."
  [step-info]
  (dissoc step-info :start-time :end-time :duration :log-summary-fn))

(defn- date-string? [s]
  (-> s tcoerce/from-string boolean))

(defn- validate-times [m]
  (and (-> m :start-time date-string?)
       (-> m :end-time date-string?)
       (-> m :duration string?)))

(expect
  [
   ;; There should only be 1 operation info returned
   true
   ;; Validate that start/end/duration of the entire sync operation is included
   true
   ;; Each step should have a valid start/end/duration value
   [true true]
   ;; Each step name is included with the results, the order is preseverd
   ["step1" "step2"]]
  (let [sync-steps [(create-sync-step "step1" (fn [_] (Thread/sleep 10)))
                    (create-sync-step "step2" (fn [_] (Thread/sleep 10)))]
        mock-db    (mdb/map->DatabaseInstance {:name "test", :id  1, :engine :h2})
        [results & none]    (call-with-operation-info #(run-sync-operation "sync" mock-db sync-steps))]
    [(empty? none)
     (validate-times results)
     (map (comp validate-times second) (:steps results))
     (map first (:steps results))]))
