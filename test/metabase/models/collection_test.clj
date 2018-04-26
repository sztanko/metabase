(ns metabase.models.collection-test
  (:require [expectations :refer :all]
            [metabase.models
             [card :refer [Card]]
             [collection :as collection :refer [Collection]]]
            [metabase.util :as u]
            [toucan
             [db :as db]
             [hydrate :refer [hydrate]]]
            [metabase.test.util :as tu]
            [toucan.util.test :as tt]))

;; test that we can create a new Collection with valid inputs
(expect
  {:name        "My Favorite Cards"
   :slug        "my_favorite_cards"
   :description nil
   :color       "#ABCDEF"
   :archived    false
   :parent_id   nil}
  (tt/with-temp Collection [collection {:name "My Favorite Cards", :color "#ABCDEF"}]
    (dissoc collection :id)))

;; check that the color is validated
(expect Exception (db/insert! Collection {:name "My Favorite Cards"}))                    ; missing color
(expect Exception (db/insert! Collection {:name "My Favorite Cards", :color "#ABC"}))     ; too short
(expect Exception (db/insert! Collection {:name "My Favorite Cards", :color "#BCDEFG"}))  ; invalid chars
(expect Exception (db/insert! Collection {:name "My Favorite Cards", :color "#ABCDEFF"})) ; too long
(expect Exception (db/insert! Collection {:name "My Favorite Cards", :color "ABCDEF"}))   ; missing hash prefix

;; double-check that `with-temp-defaults` are working correctly for Collection
(expect
  :ok
  (tt/with-temp* [Collection [_]]
    :ok))

;; test that duplicate names aren't allowed
(expect
  Exception
  (tt/with-temp* [Collection [_ {:name "My Favorite Cards"}]
                  Collection [_ {:name "My Favorite Cards"}]]
    :ok))

;; things with different names that would cause the same slug shouldn't be allowed either
(expect
  Exception
  (tt/with-temp* [Collection [_ {:name "My Favorite Cards"}]
                  Collection [_ {:name "my_favorite Cards"}]]
    :ok))

;; check that archiving a collection archives its cards as well
(expect
  true
  (tt/with-temp* [Collection [collection]
                  Card       [card       {:collection_id (u/get-id collection)}]]
    (db/update! Collection (u/get-id collection)
      :archived true)
    (db/select-one-field :archived Card :id (u/get-id card))))

;; check that unarchiving a collection unarchives its cards as well
(expect
  false
  (tt/with-temp* [Collection [collection {:archived true}]
                  Card       [card       {:collection_id (u/get-id collection), :archived true}]]
    (db/update! Collection (u/get-id collection)
      :archived false)
    (db/select-one-field :archived Card :id (u/get-id card))))

;; check that collections' names cannot be blank
(expect
  Exception
  (tt/with-temp Collection [collection {:name ""}]
    collection))

;; check we can't change the name of a Collection to a blank string
(expect
  Exception
  (tt/with-temp Collection [collection]
    (db/update! Collection (u/get-id collection)
      :name "")))


;;; +----------------------------------------------------------------------------------------------------------------+
;;; |                                               Nested Collections                                               |
;;; +----------------------------------------------------------------------------------------------------------------+

;; Does our handy utility function for working with `location` paths work as expected?
(expect "/1/2/3/" (collection/location-path 1 2 3))
(expect "/"       (collection/location-path))
(expect "/1/"     (collection/location-path {:id 1}))
(expect "/1/2/3/" (collection/location-path {:id 1} {:id 2} {:id 3}))
(expect "/1/337/" (collection/location-path 1 {:id 337}))
(expect Exception (collection/location-path "1"))
(expect Exception (collection/location-path nil))
(expect Exception (collection/location-path -1))
(expect Exception (collection/location-path 1 2 1)) ; shouldn't allow duplicates

(expect [1 2 3]   (collection/location-path->ids "/1/2/3/"))
(expect []        (collection/location-path->ids "/"))
(expect [1]       (collection/location-path->ids "/1/"))
(expect [1 337]   (collection/location-path->ids "/1/337/"))
(expect Exception (collection/location-path->ids "/a/"))
(expect Exception (collection/location-path->ids nil))
(expect Exception (collection/location-path->ids "/-1/"))
(expect Exception (collection/location-path->ids "/1/2/1/"))

(expect 3         (collection/location-path->parent-id "/1/2/3/"))
(expect nil       (collection/location-path->parent-id "/"))
(expect 1         (collection/location-path->parent-id "/1/"))
(expect 337       (collection/location-path->parent-id "/1/337/"))
(expect Exception (collection/location-path->parent-id "/a/"))
(expect Exception (collection/location-path->parent-id nil))
(expect Exception (collection/location-path->parent-id "/-1/"))
(expect Exception (collection/location-path->parent-id "/1/2/1/"))

(expect "/1/2/3/1000/" (collection/children-location {:id 1000, :location "/1/2/3/"}))
(expect "/1000/"       (collection/children-location {:id 1000, :location "/"}))
(expect "/1/1000/"     (collection/children-location {:id 1000, :location "/1/"}))
(expect "/1/337/1000/" (collection/children-location {:id 1000, :location "/1/337/"}))
(expect Exception      (collection/children-location {:id 1000, :location "/a/"}))
(expect Exception      (collection/children-location {:id 1000, :location nil}))
(expect Exception      (collection/children-location {:id 1000, :location "/-1/"}))
(expect Exception      (collection/children-location {:id nil,  :location "/1/"}))
(expect Exception      (collection/children-location {:id "a",  :location "/1/"}))
(expect Exception      (collection/children-location {:id 1,    :location "/1/2/"}))

;; Can we INSERT a Collection with a valid path?
(defn- insert-collection-with-location! [location]
  (tu/with-model-cleanup [Collection]
    (-> (db/insert! Collection :name (tu/random-name), :color "#ABCDEF", :location location)
        :location
        (= location))))

(expect
  (tt/with-temp Collection [parent]
    (insert-collection-with-location! (collection/location-path parent))))

;; Make sure we can't INSERT a Collection with an invalid path
(defn- nonexistent-collection-id []
  (inc (or (:max (db/select-one [Collection [:%max.id :max]]))
           0)))

(expect
  Exception
  (insert-collection-with-location! "/a/"))

;; Make sure we can't INSERT a Collection with an non-existent ancestors
(expect
  Exception
  (insert-collection-with-location! (collection/location-path (nonexistent-collection-id))))

;; MAae sure we can UPDATE a Collection and give it a new, *valid* location
(expect
  (tt/with-temp* [Collection [collection-1]
                  Collection [collection-2]]
    (db/update! Collection (u/get-id collection-1) :location (collection/location-path collection-2))))

;; Make sure we can't UPDATE a Collection to give it an valid path
(expect
  Exception
  (tt/with-temp Collection [collection]
    (db/update! Collection (u/get-id collection) :location "/a/")))

;; Make sure we can't UPDATE a Collection to give it a non-existent ancestors
(expect
  Exception
  (tt/with-temp Collection [collection]
    (db/update! Collection (u/get-id collection) :location (collection/location-path (nonexistent-collection-id)))))


;;; --------------------------------------- Related Collection Hydration Tests ---------------------------------------

(defmacro ^:private with-a-family-of-collections
  {:style/indent 1}
  [[grandparent-binding parent-binding child-binding] & body]
  `(tt/with-temp* [Collection [grandparent# {:name "Grandparent"}]
                   Collection [parent#      {:name "Parent", :location (collection/location-path grandparent#)}]
                   Collection [child#       {:name "Child",  :location (collection/location-path grandparent# parent#)}]]
     (let [~grandparent-binding grandparent#
           ~parent-binding      parent#
           ~child-binding       child#]
       ~@body)))

;; Can we hydrate `ancestors` the way we'd hope?
(expect
  [{:id Integer, :name "Grandparent"}
   {:id Integer, :name "Parent"}]
  (with-a-family-of-collections [_ _ collection]
    (for [ancestor (collection/ancestors collection)]
      (update ancestor :id class))))

;; Can we hydrate `children` Collections the way we'd hope?
(expect
  [{:id Integer, :name "Another Child"}
   {:id Integer, :name "Child"}]
  (with-a-family-of-collections [_ parent child]
    (tt/with-temp Collection [_ {:name "Another Child", :location (:location child)}]
      (for [child (collection/children parent)]
        (update child :id class)))))


;; When we delete a Collection do its descendants get deleted as well?
(expect
  0
  (with-a-family-of-collections [grandparent parent child]
    (db/delete! Collection :id (u/get-id grandparent))
    (db/count Collection :id [:in (map u/get-id [grandparent parent child])])))

;; ...put parents should be untouched
(expect
  1
  (with-a-family-of-collections [grandparent parent child]
    (db/delete! Collection :id (u/get-id parent))
    (db/count Collection :id [:in (map u/get-id [grandparent parent child])])))
