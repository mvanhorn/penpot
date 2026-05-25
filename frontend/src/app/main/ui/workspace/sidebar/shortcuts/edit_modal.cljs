;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS INC

(ns app.main.ui.workspace.sidebar.shortcuts.edit-modal
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.config :as cf]
   [app.main.data.modal :as modal]
   [app.main.data.shortcuts :as ds]
   [app.main.data.workspace.shortcuts.customize :as customize]
   [app.main.store :as st]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.foundations.assets.icon :as i]
   [app.main.ui.ds.notifications.context-notification :refer [context-notification*]]
   [app.util.dom :as dom]
   [app.util.i18n :refer [tr]]
   [cuerdas.core :as str]
   [goog.events :as events]
   [rumext.v2 :as mf])
  (:import
   goog.events.EventType))

(def ^:private modifier-keys
  #{"Control" "Shift" "Alt" "Meta"})

(def ^:private key-name-map
  {"ArrowUp"    "up"
   "ArrowDown"  "down"
   "ArrowLeft"  "left"
   "ArrowRight" "right"
   "Escape"     "escape"
   "Enter"      "enter"
   "Backspace"  "backspace"
   "Delete"     "del"
   "Tab"        "tab"
   " "          "space"})

(defn- shortcut-translation
  [keyname]
  (tr (str "shortcuts." (d/name keyname))))

(defn- keyboard-event->mousetrap
  [^js event]
  (let [parts (cond-> []
                (and (.-ctrlKey event)
                     (not (cf/check-platform? :macos)))
                (conj "ctrl")

                (and (.-metaKey event)
                     (cf/check-platform? :macos))
                (conj "command")

                (.-altKey event)
                (conj "alt")

                (.-shiftKey event)
                (conj "shift"))
        key   (.-key event)
        key   (if (contains? modifier-keys key)
                nil
                (or (get key-name-map key)
                    (.toLowerCase key)))]
    (when key
      (str/join "+" (conj parts key)))))

(defn- keyboard-event->display-parts
  [^js event]
  (let [parts (cond-> []
                (and (.-ctrlKey event)
                     (not (cf/check-platform? :macos)))
                (conj "ctrl")

                (and (.-metaKey event)
                     (cf/check-platform? :macos))
                (conj "command")

                (.-altKey event)
                (conj "alt")

                (.-shiftKey event)
                (conj "shift"))
        key   (.-key event)]
    (if (contains? modifier-keys key)
      {:modifiers parts :finalized? false}
      {:modifiers parts
       :final-key (or (get key-name-map key) (.toLowerCase key))
       :finalized? true})))

(defn- find-conflict
  [new-command all-shortcuts current-key]
  (let [command-index (ds/build-command-index all-shortcuts)]
    (when-let [conflicting-key (get command-index new-command)]
      (when (not= conflicting-key current-key)
        {:key  conflicting-key
         :name (shortcut-translation conflicting-key)}))))

(mf/defc shortcut-edit-dialog
  {::mf/register modal/components
   ::mf/register-as :shortcut-edit}
  [{:keys [shortcut-key shortcut-name current-command default-command all-shortcuts]}]
  (let [recorded-command (mf/use-state nil)
        display-parts    (mf/use-state nil)
        conflict         (mf/use-state nil)

        has-recording?   (some? @recorded-command)

        cancel-fn
        (mf/use-callback
         (fn [event]
           (dom/prevent-default event)
           (st/emit! (modal/hide))))

        save-fn
        (mf/use-callback
         (mf/deps @recorded-command @conflict shortcut-key)
         (fn [event]
           (dom/prevent-default event)
           (when @recorded-command
             (st/emit! (modal/hide)
                       (customize/set-custom-shortcut
                        shortcut-key
                        @recorded-command
                        (:key @conflict))))))]

    (mf/with-effect []
      (letfn [(on-keydown [^js event]
                (.preventDefault event)
                (.stopPropagation event)
                (let [command (keyboard-event->mousetrap event)
                      parts   (keyboard-event->display-parts event)]
                  (reset! display-parts parts)
                  (when command
                    (reset! recorded-command command)
                    (reset! conflict (find-conflict command all-shortcuts shortcut-key)))))]
        (->> (events/listen js/document EventType.KEYDOWN on-keydown)
             (partial events/unlistenByKey))))

    [:div {:class (stl/css :modal-overlay)}
     [:div {:class (stl/css :modal-container)}
      [:div {:class (stl/css :modal-header)}
       [:h2 {:class (stl/css :modal-title)} (tr "shortcuts.edit-modal.title")]
       [:div {:class (stl/css :modal-close-btn)}
        [:> icon-button* {:variant "ghost"
                          :aria-label (tr "labels.close")
                          :on-click cancel-fn
                          :icon i/close}]]]

      [:div {:class (stl/css :modal-content)}
       [:div {:class (stl/css :shortcut-info)}
        [:span {:class (stl/css :shortcut-label)} shortcut-name]
        [:div {:class (stl/css :current-keys)}
         [:span {:class (stl/css :current-label)} (tr "shortcuts.edit-modal.current")]
         [:span {:class (stl/css :current-value)}
          (if (vector? current-command)
            (str/join " / " current-command)
            current-command)]]]

       [:div {:class (stl/css :recording-area)}
        (if (nil? @display-parts)
          [:span {:class (stl/css :recording-hint)}
           (tr "shortcuts.edit-modal.press-keys")]

          [:div {:class (stl/css :recorded-keys)}
           (for [mod (:modifiers @display-parts)]
             [:span {:class (stl/css :key) :key mod} mod])
           (when (:final-key @display-parts)
             [:span {:class (stl/css :key)} (:final-key @display-parts)])
           (when-not (:finalized? @display-parts)
             [:span {:class (stl/css :recording-ellipsis)}
              (tr "shortcuts.edit-modal.recording")])])]

       (when @conflict
         [:> context-notification* {:level :warning
                                    :appearance :ghost}
          (tr "shortcuts.edit-modal.conflict" (:name @conflict))])]

      [:div {:class (stl/css :modal-footer)}
       [:div {:class (stl/css :action-buttons)}
        [:> button* {:variant "secondary"
                     :on-click cancel-fn}
         (tr "shortcuts.edit-modal.cancel")]
        [:> button* {:variant "primary"
                     :disabled (not has-recording?)
                     :on-click save-fn}
         (tr "shortcuts.edit-modal.save")]]]]]))
