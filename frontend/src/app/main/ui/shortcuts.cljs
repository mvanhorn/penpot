;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC Sucursal en España SL

(ns app.main.ui.shortcuts
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.config :as cf]
   [app.main.data.shortcuts :as ds]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :as i :refer [icon*]]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [app.util.strings :refer [matches-search]]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(defn translation-keyname
  [type keyname]
  (let [translat-pre (case type
                       :sc      "shortcuts."
                       :sec     "shortcut-section."
                       :sub-sec "shortcut-subsection.")]
    (tr (str translat-pre (d/name keyname)))))

(defn add-translation
  [type item]
  (map (fn [[k v]] [k (assoc v :translation (translation-keyname type k))]) item))

(defn shortcuts->subsections
  [shortcuts]
  (let [subsections (into #{} (mapcat :subsections) (vals shortcuts))
        get-sc-by-subsection
        (fn [subsection [k v]]
          (when (some #(= subsection %) (:subsections v)) {k v}))
        reduce-sc
        (fn [acc subsection]
          (let [shortcuts-by-subsection (into {} (keep (partial get-sc-by-subsection subsection) shortcuts))]
            (assoc acc subsection {:children shortcuts-by-subsection})))]
    (reduce reduce-sc {} subsections)))

(defn build-all-shortcuts
  [workspace-sc dashboard-sc viewer-sc]
  (let [walk (fn walk [element parent-id]
               (if (nil? element)
                 element
                 (let [rec-fn (fn [index [k item]]
                                (let [item-id (if (nil? parent-id)
                                                [index]
                                                (conj parent-id index))]
                                  [k (assoc item :id item-id :children (walk (:children item) item-id))]))]
                   (into {} (map-indexed (partial rec-fn) element)))))
        ws-subs (->> (shortcuts->subsections workspace-sc)
                     (add-translation :sub-sec)
                     (into {}))
        db-subs (->> (shortcuts->subsections dashboard-sc)
                     (add-translation :sub-sec)
                     (into {}))
        vw-subs (->> (shortcuts->subsections viewer-sc)
                     (add-translation :sub-sec)
                     (into {}))
        basics  (into {} (concat (:children (:basics ws-subs))
                                 (:children (:basics db-subs))
                                 (:children (:basics vw-subs))))
        ws-subs (dissoc ws-subs :basics)
        db-subs (dissoc db-subs :basics)
        vw-subs (dissoc vw-subs :basics)
        all     {:basics    {:id [1]
                             :children {:none {:children basics}}
                             :translation (tr "shortcut-section.basics")}
                 :workspace {:id [2]
                             :children ws-subs
                             :translation (tr "shortcut-section.workspace")}
                 :dashboard {:id [3]
                             :children db-subs
                             :translation (tr "shortcut-section.dashboard")}
                 :viewer    {:id [4]
                             :children vw-subs
                             :translation (tr "shortcut-section.viewer")}}
        all     (walk all nil)
        all-sc-names (map #(translation-keyname :sc %)
                          (concat (keys workspace-sc)
                                  (keys dashboard-sc)
                                  (keys viewer-sc)))
        all-sub-names (map #(translation-keyname :sub-sec %)
                           (concat (keys ws-subs)
                                   (keys db-subs)
                                   (keys vw-subs)))
        all-section-names (map #(translation-keyname :sec %) (keys all))]
    {:all-shortcuts all
     :all-sc-names all-sc-names
     :all-sub-names all-sub-names
     :all-section-names all-section-names}))

(mf/defc converted-chars*
  [{:keys [char command]}]
  (let [modified-keys {:up    ds/up-arrow
                       :down  ds/down-arrow
                       :left  ds/left-arrow
                       :right ds/right-arrow
                       :plus "+"}
        macos-keys    {:command "\u2318"
                       :option  "\u2325"
                       :alt     "\u2325"
                       :delete  "\u232B"
                       :del     "\u232B"
                       :shift   "\u21E7"
                       :control "\u2303"
                       :esc     "\u238B"
                       :enter   "\u23CE"}
        is-macos?     (cf/check-platform? :macos)
        char          (if (contains? modified-keys (keyword char)) ((keyword char) modified-keys) char)
        char          (if (and is-macos? (contains? macos-keys (keyword char))) ((keyword char) macos-keys) char)
        unique-key    (str (d/name command) "-" char)]
    [:span {:class (stl/css :key)
            :key unique-key} char]))

(mf/defc shortcuts-keys*
  [{:keys [content command]}]
  (let [managed-list    (if (coll? content)
                          content
                          (conj () content))
        chars-list      (map ds/split-sc managed-list)
        last-element    (last chars-list)
        short-char-list (if (= 1 (count chars-list))
                          chars-list
                          (drop-last chars-list))
        penultimate     (last short-char-list)]
    [:span {:class (stl/css :keys)}
     (for [chars short-char-list]
       [:* {:key (str/join chars)}
        (for [char chars]
          [:> converted-chars* {:key (dm/str char "-" (name command))
                                :char char
                                :command command}])
        (when (not= chars penultimate) [:span {:class (stl/css :space)} ","])])
     (when (not= last-element penultimate)
       [:*
        [:span {:class (stl/css :space)} (tr "shortcuts.or")]
        (for [char last-element]
          [:> converted-chars* {:key (dm/str char "-" (name command))
                                :char char
                                :command command}])])]))

(mf/defc shortcut-row*
  [{:keys [elements filter-term is-match-section is-match-subsection
           editable? custom-shortcuts on-edit on-reset]}]
  (let [shortcut-name         (keys elements)
        shortcut-translations (map #(translation-keyname :sc %) shortcut-name)
        match-shortcut?       (some #(matches-search % @filter-term) shortcut-translations)
        filtered              (if (and (or is-match-section is-match-subsection) (not match-shortcut?))
                                shortcut-translations
                                (filter #(matches-search % @filter-term) shortcut-translations))
        sorted-filtered       (sort filtered)]

    [:ul {:class (stl/css :sub-menu)}
     (for [command-translate sorted-filtered]
       (let [sc-by-translate  (first (filter #(= (:translation (second %)) command-translate) elements))
             [command  comand-info] sc-by-translate
             content                (or (:show-command comand-info) (:command comand-info))
             customized?            (and editable? (contains? custom-shortcuts command))]
         [:li {:class (stl/css-case :shortcuts-name true
                                    :customized customized?)
               :key command-translate}
          [:span {:class (stl/css :command-name)}
           command-translate]
          [:div {:class (stl/css :shortcut-actions)}
           [:> shortcuts-keys* {:content content
                                :command command}]
           (when editable?
             [:div {:class (stl/css :edit-buttons)}
              (when customized?
                [:> icon-button* {:variant "ghost"
                                  :aria-label (tr "shortcuts.reset")
                                  :on-click (fn [e]
                                              (dom/stop-propagation e)
                                              (on-reset command))
                                  :icon i/reload
                                  :icon-size "s"}])
              [:> icon-button* {:variant "ghost"
                                :aria-label (tr "shortcuts.edit")
                                :on-click (fn [e]
                                            (dom/stop-propagation e)
                                            (on-edit command))
                                :icon i/curve
                                :icon-size "s"}]])]]))]))

(mf/defc section-title*
  [{:keys [name is-visible is-sub]}]
  [:div {:class (if is-sub
                  (stl/css :subsection-title)
                  (stl/css :section-title))}
   [:> icon* {:icon-id (if is-visible i/arrow-down i/arrow-right)
              :size "s"}]
   [:span {:class (if is-sub
                    (stl/css :subsection-name)
                    (stl/css :section-name))} name]])

(mf/defc shortcut-subsection*
  [{:keys [subsections manage-sections filter-term is-match-section open-sections
           editable? custom-shortcuts on-edit on-reset]}]
  (let [subsections-names       (keys subsections)
        subsection-translations (if (= :none (first subsections-names))
                                  (map #(translation-keyname :sc %) subsections-names)
                                  (map #(translation-keyname :sub-sec %) subsections-names))
        sorted-translations     (sort subsection-translations)]
    (if (= :none (first subsections-names))
      (let [basic-shortcuts (:none subsections)]
        [:> shortcut-row* {:elements (:children basic-shortcuts)
                           :filter-term filter-term
                           :is-match-section is-match-section
                           :is-match-subsection true
                           :editable? editable?
                           :custom-shortcuts custom-shortcuts
                           :on-edit on-edit
                           :on-reset on-reset}])

      [:ul {:class (stl/css :subsection-menu)}
       (for [sub-translated sorted-translations]
         (let [sub-by-translate    (first (filter #(= (:translation (second %)) sub-translated) subsections))
               [sub-name sub-info] sub-by-translate
               visible?            (some  #(= % (:id sub-info)) @open-sections)
               match-subsection?   (matches-search (translation-keyname :sub-sec sub-name) @filter-term)
               shortcut-names      (map #(translation-keyname :sc %) (keys (:children sub-info)))
               match-shortcuts?    (some #(matches-search % @filter-term) shortcut-names)]
           (when (or match-subsection? match-shortcuts? is-match-section)
             [:li {:key sub-translated
                   :on-click (manage-sections (:id sub-info))}
              [:> section-title* {:name sub-translated
                                  :is-visible visible?
                                  :is-sub true}]

              [:div {:style {:display (if visible? "initial" "none")}}
               [:> shortcut-row* {:elements (:children sub-info)
                                  :filter-term filter-term
                                  :is-match-section is-match-section
                                  :is-match-subsection match-subsection?
                                  :editable? editable?
                                  :custom-shortcuts custom-shortcuts
                                  :on-edit on-edit
                                  :on-reset on-reset}]]])))])))

(mf/defc shortcut-section*
  [{:keys [section manage-sections open-sections filter-term
           editable? custom-shortcuts on-edit on-reset]}]
  (let [[section-key section-info] section
        section-id          (:id section-info)
        section-translation (translation-keyname :sec section-key)
        match-section?      (matches-search section-translation @filter-term)
        subsections         (:children section-info)
        subs-names          (keys subsections)
        subs-bodys          (reduce #(conj %1 (:children (%2 subsections))) {} subs-names)
        sub-trans           (map #(if (= "none" (d/name %))
                                    nil
                                    (translation-keyname :sub-sec %)) subs-names)
        match-subsection?   (some #(matches-search % @filter-term) sub-trans)
        translations        (map #(translation-keyname :sc %) (keys subs-bodys))
        match-shortcut?     (some #(matches-search % @filter-term) translations)
        visible?            (some  #(= % section-id) @open-sections)]

    (when (or match-section? match-subsection? match-shortcut?)
      [:div {:class (stl/css :section)
             :on-click (manage-sections section-id)}
       [:> section-title* {:name section-translation
                           :is-visible visible?
                           :is-sub false}]

       [:div {:style {:display (if visible? "initial" "none")}}
        [:> shortcut-subsection* {:subsections subsections
                                  :open-sections open-sections
                                  :manage-sections manage-sections
                                  :is-match-section match-section?
                                  :filter-term filter-term
                                  :editable? editable?
                                  :custom-shortcuts custom-shortcuts
                                  :on-edit on-edit
                                  :on-reset on-reset}]]])))
