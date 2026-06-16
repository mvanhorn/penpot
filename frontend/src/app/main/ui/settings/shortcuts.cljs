(ns app.main.ui.settings.shortcuts
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.main.data.dashboard.shortcuts]
   [app.main.data.modal :as modal]
   [app.main.data.shortcuts :as ds]
   [app.main.data.viewer.shortcuts]
   [app.main.data.workspace.path.shortcuts]
   [app.main.data.workspace.shortcuts]
   [app.main.data.workspace.shortcuts.customize :as customize]
   [app.main.store :as st]
   [app.main.ui.ds.foundations.typography :as t]
   [app.main.ui.ds.foundations.typography.heading :refer [heading*]]
   [app.main.ui.ds.layout.tab-switcher :refer [tab-switcher*]]
   [app.main.ui.shortcuts :as ss]
   [app.util.dom :as dom]
   [rumext.v2 :as mf]))

(def ^:private workspace-shortcuts-raw
  (d/deep-merge app.main.data.workspace.path.shortcuts/shortcuts
                app.main.data.workspace.shortcuts/shortcuts))

(mf/defc shortcuts-list*
  [{:keys [profile workspace-sc dashboard-sc viewer-sc]}]
  (let [open-sections*                (mf/use-state [[1]])
        open-sections                  (deref open-sections*)
        custom-shortcuts             (get-in profile [:props :custom-shortcuts])

        workspace-translated         (->> workspace-sc
                                          (ss/add-translation :sc)
                                          (into {}))
        dashboard-translated         (->> dashboard-sc
                                          (ss/add-translation :sc)
                                          (into {}))
        viewer-translated            (->> viewer-sc
                                          (ss/add-translation :sc)
                                          (into {}))

        filter-term                  (mf/use-state "")

        {:keys [all-shortcuts]}
        (ss/build-all-shortcuts workspace-translated dashboard-translated viewer-translated)

        section-has-content?
        (fn [section]
          (let [children (:children section)]
            (if (and (= (count children) 1) (contains? children :none))
              (seq (:children (:none children)))
              (seq children))))

        all-shortcuts (into {} (filter (fn [[_ v]] (section-has-content? v)) all-shortcuts))

        on-edit-shortcut
        (mf/use-callback
         (mf/deps custom-shortcuts workspace-sc)
         (fn [shortcut-key]
           (let [default-command (:command (get workspace-shortcuts-raw shortcut-key))
                 current-command (or (get custom-shortcuts shortcut-key) default-command)]
             (st/emit! (modal/show :shortcut-edit
                                   {:shortcut-key     shortcut-key
                                    :shortcut-name    (ss/translation-keyname :sc shortcut-key)
                                    :current-command  current-command
                                    :default-command  default-command
                                    :all-shortcuts    all-shortcuts})))))

        on-reset-shortcut
        (mf/use-callback
         (fn [shortcut-key]
           (st/emit! (customize/reset-custom-shortcut shortcut-key))))
        manage-sections
        (fn [item]
          (fn [event]
            (dom/stop-propagation event)
            (let [is-present? (some #(= % item) open-sections)
                  new-value (if is-present?
                              (filterv (fn [element] (not= element item)) open-sections)
                              (conj open-sections item))]
              (reset! open-sections* new-value))))]

    (for [section all-shortcuts]
      (let [[section-key _] section]
        [:> ss/shortcut-section* {:key (name section-key)
                                  :section section
                                  :manage-sections manage-sections
                                  :open-sections open-sections*
                                  :filter-term filter-term
                                  :editable? true
                                  :custom-shortcuts custom-shortcuts
                                  :on-edit on-edit-shortcut
                                  :on-reset on-reset-shortcut}]))))

(mf/defc all-shortcuts-section*
  [{:keys [profile]}]
  (let [custom-shortcuts           (get-in profile [:props :custom-shortcuts])
        workspace-shortcuts-custom (ds/apply-custom-overrides workspace-shortcuts-raw custom-shortcuts)]
    [:div {:class (stl/css :shortcuts-section)}
     [:> shortcuts-list* {:profile profile
                          :workspace-sc workspace-shortcuts-custom
                          :dashboard-sc app.main.data.dashboard.shortcuts/shortcuts
                          :viewer-sc app.main.data.viewer.shortcuts/shortcuts}]]))

(mf/defc personalized-shortcuts-section*
  [{:keys [profile]}]
  (let [custom-shortcuts           (get-in profile [:props :custom-shortcuts])
        workspace-shortcuts-custom (ds/apply-custom-overrides workspace-shortcuts-raw custom-shortcuts)
        personalized               (select-keys workspace-shortcuts-custom (keys custom-shortcuts))]
    [:div {:class (stl/css :shortcuts-section)}
     (if (seq personalized)
       [:> shortcuts-list* {:profile profile
                            :workspace-sc personalized
                            :dashboard-sc {}
                            :viewer-sc {}}]
       [:p "There are no personalized shortcuts."])]))

(mf/defc not-assigned-shortcuts-section*
  [{:keys [profile]}]
  (let [custom-shortcuts           (get-in profile [:props :custom-shortcuts])
        workspace-shortcuts-custom (ds/apply-custom-overrides workspace-shortcuts-raw custom-shortcuts)
        not-assigned               (into {} (filter (fn [[_ v]]
                                                       (empty? (:command v)))
                                                     workspace-shortcuts-custom))]
    [:div {:class (stl/css :shortcuts-section)}
     (if (seq not-assigned)
       [:> shortcuts-list* {:profile profile
                            :workspace-sc not-assigned
                            :dashboard-sc {}
                            :viewer-sc {}}]
       [:p "There are no not-assigned shortcuts."])]))



(mf/defc shortcuts-page*
  [{:keys [profile]}]
  (let [section*        (mf/use-state :all)
        section         (deref section*)

        tabs
        (mf/with-memo []
          [{:label "All shortcuts"
            :id "all"}
           {:label "Personalized shortcuts"
            :data-testid "personalized"
            :id "personalized"}
           {:label "Not assigned shortcuts"
            :data-testid "not-assigned"
            :id "not-assigned"}])

        handle-change-tab
        (mf/use-fn
         (mf/deps)
         (fn [new-section]
           (reset! section* (keyword new-section))))]


    [:section {:class (stl/css :shortcuts-page)
               :aria-label "Shortcuts page"}
     [:> heading* {:level 1
                   :typography t/title-large
                   :class (stl/css :color-primary)}
      "Shortcuts page"]

     [:div {:class (stl/css :shortcuts-content)}
      [:p "search-bar"]
      [:button "restore all"]
      [:p "This is the shortcuts page content."]
      [:> tab-switcher* {:tabs tabs
                         :selected (name section)
                         :on-change handle-change-tab
                         :class (stl/css :viewer-tab-switcher)}
       (case section
         :all
         [:> all-shortcuts-section* {:profile profile}]

         :personalized
         [:> personalized-shortcuts-section* {:profile profile}]

         :not-assigned
         [:> not-assigned-shortcuts-section* {:profile profile}])]]]))