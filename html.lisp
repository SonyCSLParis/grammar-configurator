;; Copyright 2021-present Sony Computer Science Laboratories Paris

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

(in-package :fcg)

;; This file contains prototype code that was developed for research purposes and should not be used in production environments.
;; No warranties are provided.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                           ;;
;; This file contains all code for generating the grammar configurator form in HTML          ;;
;;                                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Abstractions ;;
;;;;;;;;;;;;;;;;;;

(defun top-level-section (&key title value expanded?)
  (let ((top-level-label-id (lisp->camel-case (mkstr (make-id "top-level-label"))))
        (top-level-value-id (lisp->camel-case (mkstr (make-id "top-level-value"))))
        (title-expanded (format nil "- ~a:" title))
        (title-collapsed (format nil "+ ~a" title)))
    `((span)
      ((label :class "top-level-label"
              :id ,top-level-label-id
              :onclick ,(format nil "toggle_inner_text_and_visibility('~a','~a','~a','~a');"
                                top-level-label-id title-expanded title-collapsed top-level-value-id))
       ,(if expanded?
          title-expanded
          title-collapsed))
      ((div :class "top-level-value"
            :id ,top-level-value-id
            :style ,(if expanded?
                      "display: block;"
                      "display: none;"))
       ,value))))

(defun text-field (&key value pattern title required id disabled other-properties)
  `((input :type "text"
           :accept "text/plain"
           :value ,value
           ,@(when disabled '(:disabled "disabled"))
           :pattern ,pattern
           :title ,title
           ,@(when required '(:required "required"))
           :id ,id
           ,@other-properties)))


(defun table (properties &rest table-rows)
  `((table :class "gc-inner-table" ,@properties)
    ((tbody)
     ,@table-rows)))

(defun span (properties &rest span-data)
  `((span ,@properties)
    ,@span-data))

(defun table-row (&rest table-data)
  `((tr)
    ,@table-data))

(defun table-data (&rest contents)
  `((td)
    ,@contents))

(defun html-label (label &key properties)
  `((label ,@properties)
    ,label))

(defun checkbox (&key value id checked other-properties)
  `((input :type "checkbox"
           :value ,value
           :id ,id
           ,@(when checked '(:checked "checked"))
           ,@other-properties)))

(defun radio-button (&key value id name checked other-properties)
  `((input :type "radio"
           :value ,value
           :id ,id
           :name ,name
           ,@(when checked '(:checked "checked"))
           ,@other-properties)))

(defun warning-message (string)
  `((p) ((i) ,string)))

(defun vspacer ()
  '((div :class "vspacer")))


;; Form Content ;;
;;;;;;;;;;;;;;;;;;

(defun grammar-name (cxn-inventory-symbol)
  "For the grammar name textbox."
  (top-level-section :expanded? t
                  :title "Grammar name and access variable"
                  :value (text-field :value (if cxn-inventory-symbol
                                              (downcase (symbol-name cxn-inventory-symbol))
                                              "*fcg-constructions*")
                                     :pattern "([a-zA-Z0-9]|-|_|&#92;*)+"
                                     :title "No spaces or characters other than [a-z], [A-Z], 0-9, _, - and * allowed"

                                     :disabled (if cxn-inventory-symbol t)
                                     :id "grammar_name"
                                     :required t)))

(defun custom-feature-types (cxn-inventory)
  "For specifying custom feature types."
  (top-level-section :expanded? nil
                     :title "Custom feature types"
                     :value (table nil
                                   (table-row (table-data (html-label "Set:" :properties '(:for "ft_set")))
                                              (table-data (text-field :value (if cxn-inventory
                                                                               (format nil "~{~(~a~)~^ ~}" (features-of-type 'set cxn-inventory))
                                                                               "subunits args footprints")
                                                                      :pattern "[^,;]*"
                                                                      :title "Separate feature names by spaces"
                                                                      :id "ft_set"
                                                                      :other-properties  '(:class "ft_textbox" ))))
                                   (table-row (table-data (html-label "Sequence:" :properties '(:for "ft_sequence")))
                                              (table-data (text-field :value (if cxn-inventory
                                                                               (format nil "~{~(~a~)~^ ~}"
                                                                                       (features-of-type 'sequence cxn-inventory))
                                                                               "")
                                                                      :pattern "[^,;]*"
                                                                      :title "Separate feature names by spaces"
                                                                      :id "ft_sequence"
                                                                      :other-properties  '(:class "ft_textbox" ))))
                                   (table-row (table-data (html-label "Set of predicates:" :properties '(:for "ft_set_of_predicates")))
                                              (table-data (text-field :value (if cxn-inventory
                                                                               (format nil "~{~(~a~)~^ ~}"
                                                                                       (features-of-type 'set-of-predicates cxn-inventory))
                                                                               "form meaning")
                                                                      :pattern "[^,;]*"
                                                                      :title "Separate feature names by spaces"
                                                                      :id "ft_set_of_predicates"
                                                                      :other-properties  '(:class "ft_textbox" ))))
                                   (table-row (table-data (html-label "Sequence of predicates:" :properties '(:for "ft_sequence_of_predicates")))
                                              (table-data (text-field :value (if cxn-inventory
                                                                               (format nil "~{~(~a~)~^ ~}"
                                                                                       (features-of-type 'sequence-of-predicates cxn-inventory))
                                                                               "")
                                                                      :pattern "[^,;]*"
                                                                      :title "Separate feature names by spaces"
                                                                      :id "ft_sequence_of_predicates"
                                                                      :other-properties  '(:class "ft_textbox" )))))))

(defun goal-tests (cxn-inventory)
  "Specifying the goal tests."
  (top-level-section
   :expanded? nil
   :title "Goal tests"
   :value
   (table nil
          (table-row (table-data (html-label "Comprehension:"))
                     (table-row (table-data
                                 (checkbox :value ":no-applicable-cxns"
                                           :id "no-applicable-cxns-comprehension"
                                           :checked (or (not cxn-inventory)
                                                        (and cxn-inventory
                                                             (member :no-applicable-cxns (get-configuration cxn-inventory :parse-goal-tests)))))
                                 (html-label "no more applicable constructions (<i>:no-applicable-cxns</i>)" :properties '(:for "no-applicable-cxns-comprehension"
                                                                                                                      :class "checkbox-label"))))
                     (table-row (table-data
                                 (checkbox :value ":no-strings-in-root"
                                           :id "no-strings-in-root"
                                           :checked (and cxn-inventory
                                                         (member :no-strings-in-root (get-configuration cxn-inventory :parse-goal-tests))))
                                 (html-label "no more strings in root (<i>:no-strings-in-root</i>)" :properties '(:for "no-strings-in-root"
                                                                                                             :class "checkbox-label"))))
                     (table-row (table-data
                                 (checkbox :value ":connected-semantic-network"
                                           :id "connected-semantic-network"
                                           :checked (and cxn-inventory
                                                         (member :connected-semantic-network (get-configuration cxn-inventory :parse-goal-tests))))
                                 (html-label "semantic network is fully connected (<i>:connected-semantic-network</i>)"
                                        :properties '(:class "checkbox-label" :for "connected-semantic-network"))))
                     (table-row (table-data
                                 (checkbox :value ":always-fail"
                                           :id "always-fail-comprehension"
                                           :checked (and cxn-inventory
                                                         (member :always-fail (get-configuration cxn-inventory :parse-goal-tests))))
                                 (html-label "always fail (<i>:always-fail</i>)"
                                        :properties '(:class "checkbox-label" :for "always-fail-comprehension"))))
                     (table-row (table-data
                                 (checkbox :value "other-goal-tests-comprehension"
                                           :id "other-goal-tests-comprehension"
                                           :checked (and cxn-inventory
                                                         (set-difference (get-configuration cxn-inventory :parse-goal-tests)
                                                                         '(:no-applicable-cxns :no-strings-in-root :connected-semantic-network :always-fail))))
                                 (text-field :value (if cxn-inventory
                                                      (loop for gt in (get-configuration cxn-inventory :parse-goal-tests)
                                                            unless (member gt '(:no-applicable-cxns :no-strings-in-root :connected-semantic-network :always-fail))
                                                            collect gt into other-goal-tests
                                                            finally (return (format nil "~{:~(~a~)~^ ~}" other-goal-tests)))
                                                      "")
                                             :pattern "[^,;]*"
                                             :title "Separate goal test names by spaces"
                                             :id "other_parse_goal_tests"
                                             :other-properties '(:class "ft_textbox checkbox-label")))))
          (table-row (table-data (vspacer)))
          (table-row (table-data (html-label "Production:"))
                     (table-row (table-data
                                 (checkbox :value ":no-applicable-cxns"
                                           :id "no-applicable-cxns-production"
                                           :checked (or (not cxn-inventory)
                                                        (and cxn-inventory
                                                             (member :no-applicable-cxns (get-configuration cxn-inventory :production-goal-tests)))))
                                 (html-label "no more applicable constructions (<i>:no-applicable-cxns</i>)"
                                        :properties '(:class "checkbox-label" :for "no-applicable-cxns-production"))))
                     (table-row (table-data
                                 (checkbox :value ":no-meaning-in-root"
                                           :id "no-meaning-in-root"
                                           :checked (and cxn-inventory
                                                         (member :no-meaning-in-root (get-configuration cxn-inventory :production-goal-tests))))
                                 (html-label "no more meaning predicates in root (<i>:no-meaning-in-root</i>)"
                                        :properties '(:class "checkbox-label" :for "no-meaning-in-root"))))
                     (table-row (table-data
                                 (checkbox :value ":connected-structure"
                                           :id "connected-structure"
                                           :checked (and cxn-inventory
                                                         (member :connected-structure (get-configuration cxn-inventory :production-goal-tests))))
                                 (html-label "unit structure is fully connected (<i>:connected-structure</i>)"
                                        :properties '(:class "checkbox-label" :for "connected-structure"))))
                     (table-row (table-data
                                 (checkbox :value ":always-fail"
                                           :id "always-fail-production"
                                           :checked (and cxn-inventory
                                                         (member :always-fail (get-configuration cxn-inventory :production-goal-tests))))
                                 (html-label "always fail (<i>:always-fail</i>)"
                                        :properties '(:class "checkbox-label" :for "always-fail-production"))))
                     (table-row (table-data
                                 (checkbox :value "other-goal-tests-production"
                                           :id "other-goal-tests-production"
                                           :checked (and cxn-inventory
                                                         (set-difference (get-configuration cxn-inventory :production-goal-tests)
                                                                         '(:no-applicable-cxns :no-meaning-in-root :connected-structure :always-fail))))
                                 (text-field :value (if cxn-inventory
                                                      (loop for gt in (get-configuration cxn-inventory :production-goal-tests)
                                                            unless (member gt '(:no-applicable-cxns :no-meaning-in-root :connected-structure :always-fail))
                                                            collect gt into other-goal-tests
                                                            finally (return (format nil "~{:~(~a~)~^ ~}" other-goal-tests)))
                                                      "")
                                             :pattern "[^,;]*"
                                             :title "Separate goal test names by spaces"
                                             :id "other_production_goal_tests"
                                             :other-properties '(:class "ft_textbox checkbox-label"))))))))



(defun node-tests (cxn-inventory)
  "Specifying the goal tests."
  (top-level-section
   :expanded? nil
   :title "Node tests"
   :value
   (span nil
         (table nil
                (table-row (table-data
                            (checkbox :value ":check-duplicate"
                                      :id "check_duplicate"
                                      :checked (or (not cxn-inventory)
                                                   (and cxn-inventory
                                                        (member :check-duplicate (get-configuration cxn-inventory :node-tests)))))
                            (html-label "discard duplicate nodes (<i>:check-duplicate</i>)" :properties '(:for "check_duplicate"
                                                                                                     :class "checkbox-label"))))
                (table-row (table-data
                            (checkbox :value ":restrict-nr-of-nodes"
                                      :id "restrict_nr_of_nodes"
                                      :checked (or (not cxn-inventory)
                                                   (and cxn-inventory
                                                        (member :restrict-nr-of-nodes (get-configuration cxn-inventory :node-tests))))
                                      :other-properties '(:onclick "toggleVisibilityCheckbox('restrict_nr_of_nodes', 'max_nr_of_nodes_field');"))
                            (html-label "limit the number of nodes in the search tree (<i>:restrict-nr-of-nodes</i>)" :properties '(:for "restrict_nr_of_nodes"
                                                                                                                               :class "checkbox-label"))))
                (table-row (table-data
                            (checkbox :value ":restrict-search-depth"
                                      :id "restrict_search_depth"
                                      :checked (or (not cxn-inventory)
                                                   (and cxn-inventory
                                                        (member :restrict-search-depth (get-configuration cxn-inventory :node-tests))))
                                      :other-properties '(:onclick "toggleVisibilityCheckbox('restrict_search_depth', 'max_search_depth_field');"))
                            (html-label "limit the depth of the search tree (<i>:restrict-search-depth</i>)" :properties '(:for "restrict_search_depth"
                                                                                                                      :class "checkbox-label"))))
                (table-row (table-data
                            (checkbox :value "true"
                                      :id "other-node-tests-button"
                                      :checked (and cxn-inventory
                                                    (set-difference (get-configuration cxn-inventory :node-tests)
                                                                    '(:check-duplicate :restrict-nr-of-nodes :restrict-search-depth))))
                            (text-field :value (if cxn-inventory
                                                 (loop for nt in (get-configuration cxn-inventory :node-tests)
                                                       unless (member nt '(:check-duplicate :restrict-nr-of-nodes :restrict-search-depth))
                                                       collect nt into other-node-tests
                                                       finally (return (format nil "~{:~(~a~)~^ ~}" other-node-tests)))
                                                 "")
                                        :pattern "[^,;]*"
                                        :title "Separate node test names by spaces"
                                        :id "other_node_tests"
                                        :other-properties '(:class "ft_textbox checkbox-label")))))
         (vspacer)
         (table '(:id "max_nr_of_nodes_field")
                (table-row
                 (table-data
                  (html-label "Maximum number of nodes:" :properties '(:for "max_nr_of_nodes")))
                 (table-data
                  (text-field :value (if cxn-inventory
                                       (mkstr (get-configuration cxn-inventory :max-nr-of-nodes))
                                       "500")
                              :pattern "[0-9]*"
                              :title "Enter an integer"
                              :id "max_nr_of_nodes"
                              :other-properties '(:class "ft_textbox")))))
         (table '(:id "max_search_depth_field")
                (table-row
                 (table-data
                  (html-label "Maximum search depth:" :properties '(:for "max_search_depth")))
                 (table-data
                  (text-field :value (if cxn-inventory
                                       (mkstr (get-configuration cxn-inventory :max-search-depth))
                                       "50")
                              :pattern "[0-9]*"
                              :title "Enter an integer"
                              :id "max_search_depth"
                              :other-properties '(:class "ft_textbox"))))))))


(defun hashing (cxn-inventory)
  "For hashing."
  (top-level-section :title "Hashing"
                     :expanded? nil
                     :value
                     (span nil
                           (table nil
                                  (table-row (table-data
                                              (checkbox :value "t"
                                                        :id "hashed"
                                                        :checked (and cxn-inventory (hashed-cxn-inventory-p cxn-inventory))
                                                        :other-properties '(:onclick "toggleVisibilityCheckbox('hashed', 'hashed_table');"))
                                              (html-label "use hashed construction inventory"
                                                     :properties '(:for "hashed" :class "checkbox-label")))))
                           (table `(:id "hashed_table" ,@(if (and cxn-inventory (hashed-cxn-inventory-p cxn-inventory))
                                                           '(:style "display:block;")
                                                           '(:style "display:none;")))
                                  (table-row (table-data (vspacer)))
                                  (table-row
                                   (table-data (html-label "Hash mode:"))
                                   (table-data
                                    (table-row
                                     (table-data
                                      (radio-button :value ":hash-word-entity-root-one-pole"
                                                    :id "hash-word-entity-root-one-pole"
                                                    :name "hash-mode"
                                                    :checked (or (not cxn-inventory)
                                                                 (not (hashed-cxn-inventory-p cxn-inventory))
                                                                 (eql :hash-word-entity-root-one-pole (get-configuration cxn-inventory :hash-mode))))
                                      (html-label "hash on string and meaning (<i>:hash-word-entity-root-one-pole</i>)"
                                             :properties '(:for "hash-word-entity-root-one-pole" :class "checkbox-label") )))
                                    (table-row 
                                     (table-data
                                      (radio-button :name "hash-mode"
                                                    :value "custom-hash-mode-button"
                                                    :id "custom-hash-mode-button"
                                                    :checked (and cxn-inventory
                                                                  (hashed-cxn-inventory-p cxn-inventory)
                                                                  (not (eql :hash-word-entity-root-one-pole (get-configuration cxn-inventory :hash-mode)))))
                                      (text-field :value (if (and cxn-inventory
                                                                  (hashed-cxn-inventory-p cxn-inventory)
                                                                  (not (member (get-configuration cxn-inventory :hash-mode) '(:hash-word-entity-root-one-pole))))
                                                           (format nil ":~(~a~)"  (get-configuration cxn-inventory :hash-mode))
                                                           "")
                                                  :pattern "[^ ]*"
                                                  :title "Specify a single mode"
                                                  :id "custom-hash-mode"
                                                  :other-properties '(:class "ft_textbox checkbox-label"))))))))))

(defun construction-sets (cxn-inventory)
  "For specifying construction sets."
  (top-level-section :title "Construction sets"
                     :expanded? nil
                     :value (span nil
                                  (warning-message "Warning: this only declares construction sets. How they are used is the responsability of the construction supplier and heuristics.")
                                  (table nil
                                         (table-row
                                          (table-data (html-label "Comprehension:" :properties '(:for "parse_order")))
                                          (table-data
                                           (text-field :value (if cxn-inventory
                                                                (format nil "~{~(~a~)~^ ~}" (get-configuration cxn-inventory :parse-order))
                                                                "cxn")
                                                       :pattern "[^,;]*"
                                                       :title "Separate by spaces"
                                                       :id "parse_order"
                                                       :other-properties '(:class "ft_textbox"))))
                                         (table-row
                                          (table-data (html-label "Production:" :properties '(:for "production_order")))
                                          (table-data
                                           (text-field :value (if cxn-inventory
                                                                (format nil "~{~(~a~)~^ ~}" (get-configuration cxn-inventory :production-order))
                                                                "cxn")
                                                       :pattern "[^,;]*"
                                                       :title "Separate  by spaces"
                                                       :id "production_order"
                                                       :other-properties '(:class "ft_textbox"))))))))


(defun rendering (cxn-inventory)
  "Render and de-render mode"
  (top-level-section :title "Rendering and de-rendering"
                     :expanded? nil
                     :value
                     (table nil
                            (table-row
                             (table-data
                              (html-label "Rendering mode:"))
                             (table-row
                              (table-data
                               (radio-button :value ":generate-and-test"
                                             :id "generate-and-test"
                                             :name "render-mode"
                                             :checked (or (not cxn-inventory)
                                                          (when (eql :generate-and-test (get-configuration cxn-inventory :render-mode)))))
                               (html-label "extensible render mode for constraints including 'string', 'meets', 'precedes' and 'first' (<i>:generate-and-test</i>)"
                                      :properties '(:for "generate-and-test" :class "checkbox-label") )))
                             (table-row
                              (table-data
                               (radio-button :name "render-mode"
                                             :value "custom-render-mode-button"
                                             :id "custom-render-mode-button"
                                             :checked (and cxn-inventory
                                                           (not (member (get-configuration cxn-inventory :render-mode)
                                                                        '(:generate-and-test)))))
                               (text-field :value (if (and cxn-inventory
                                                           (not (member (get-configuration cxn-inventory :render-mode) '(:generate-and-test))))
                                                    (format nil ":~(~a~)"  (get-configuration cxn-inventory :render-mode))
                                                    "")
                                           :pattern "[^ ]*"
                                           :title "Specify a single mode"
                                           :id "custom-render-mode"
                                           :other-properties '(:class "ft_textbox checkbox-label")))))
                            (table-row (table-data (vspacer)))
                            (table-row
                             (table-data (html-label "De-rendering mode:"))
                             (table-row (table-data
                                         (radio-button :value ":de-render-string-meets"
                                                       :id "de-render-string-meets"
                                                       :name "de-render-mode"
                                                       :checked (or (not cxn-inventory)
                                                                    (and cxn-inventory
                                                                         (eql :de-render-string-meets (get-configuration cxn-inventory :de-render-mode)))))
                                         (html-label "de-render into 'string' and 'meets' constraints (<i>:de-render-string-meets</i>)"
                                                :properties '(:for "de-render-string-meets" :class "checkbox-label"))))
                             (table-row (table-data
                                         (radio-button :name "de-render-mode"
                                                       :value ":de-render-string-meets-precedes"
                                                       :id "de-render-string-meets-precedes"
                                                       :checked (and cxn-inventory
                                                                     (eql :de-render-string-meets-precedes (get-configuration cxn-inventory :de-render-mode))))
                                         (html-label "de-render into 'string', 'meets' and 'precedes' constraints (<i>:de-render-string-meets-precedes</i>)"
                                                :properties '(:for "de-render-string-meets-precedes" :class "checkbox-label") )))
                             (table-row
                              (table-data
                               (radio-button :name "de-render-mode"
                                             :value ":de-render-string-meets-precedes-first"
                                             :id "de-render-string-meets-precedes-first"
                                             :checked (and cxn-inventory
                                                           (eql :de-render-string-meets-precedes-first (get-configuration cxn-inventory :de-render-mode))))
                               (html-label "de-render into 'string', 'meets', 'precedes' and 'first' constraints (<i>:de-render-string-meets-precedes-first</i>)"
                                      :properties '(:for "de-render-string-meets-precedes-first" :class "checkbox-label"))))
                             (table-row
                              (table-data
                               (radio-button :name "de-render-mode"
                                             :value "custom-de-render-mode"
                                             :id "custom-de-render-mode-button"
                                             :checked (and cxn-inventory
                                                           (not (member (get-configuration cxn-inventory :de-render-mode)
                                                                        '(:de-render-string-meets :de-render-string-meets-precedes
                                                                          :de-render-string-meets-precedes-first)))))
                               (text-field :value (if (and cxn-inventory
                                                           (not (member (get-configuration cxn-inventory :de-render-mode)
                                                                        '(:de-render-string-meets :de-render-string-meets-precedes
                                                                          :de-render-string-meets-precedes-first))))
                                                    (format nil ":~(~a~)"  (get-configuration cxn-inventory :de-render-mode))
                                                    "")
                                           :pattern "[^ ]*"
                                           :title "Specify a single mode"
                                           :id "custom-de-render-mode"
                                           :other-properties '(:class "ft_textbox checkbox-label"))))))))



(defun search-configuration (cxn-inventory)
  (top-level-section
   :title "Search strategies and heuristics"
   :expanded? nil
   :value
   (span nil
         (table nil
                (table-row (table-data
                            (checkbox :value "t"
                                      :id "shuffle_cxns"
                                      :checked (or (not cxn-inventory)
                                                   (get-configuration cxn-inventory :shuffle-cxns-before-application)))
                            (html-label "shuffle constructions before application (<i>:shuffle-cxns-before-application</i>)" :properties '(:for "shuffle_cxns" :class "checkbox-label")))))
         (table nil
                (table-row
                 (table-data (html-label "Construction inventory processor mode:"))
                 (table-row (table-data
                             (radio-button :value ":heuristic-search"
                                           :id "heuristic_search"
                                           :name "cip_mode"
                                           :other-properties '(:onchange "toggleVisibilityCheckbox('heuristic_search', 'heuristic_search_properties');toggleVisibilityCheckbox('legacy', 'default_search_properties');")
                                           :checked (or (not cxn-inventory)
                                                        (and cxn-inventory
                                                             (eql :heuristic-search
                                                                  (get-configuration cxn-inventory :construction-inventory-processor-mode)))))
                        
                             (html-label "cip-mode for heuristic search (<i>:heuristic-search</i>)"
                                    :properties '(:for "heuristic_search" :class "checkbox-label"))))
                 (table-row (table-data
                             (radio-button :name "cip_mode"
                                           :value ":default"
                                           :id "legacy"
                                           :other-properties '(:onchange "toggleVisibilityCheckbox('heuristic_search', 'heuristic_search_properties');toggleVisibilityCheckbox('legacy', 'default_search_properties');")
                                           :checked (and cxn-inventory
                                                         (or
                                                          (eql :default
                                                               (get-configuration cxn-inventory :construction-inventory-processor-mode))
                                                          (eql nil
                                                               (get-configuration cxn-inventory :construction-inventory-processor-mode)))))
                             (html-label "legacy cip-mode (<i>:default</i>)"
                                    :properties '(:for "legacy" :class "checkbox-label") )))
                 (table-row
                  (table-data
                   (radio-button :name "cip_mode"
                                 :value "custom-cip-mode"
                                 :id "custom-cip-mode-button"
                                 :other-properties '(:onchange "toggleVisibilityCheckbox('heuristic_search', 'heuristic_search_properties');toggleVisibilityCheckbox('legacy', 'default_search_properties');")
                                 :checked (and cxn-inventory
                                               (not (member (get-configuration cxn-inventory :construction-inventory-processor-mode)
                                                            '(:default :heuristic-search nil)))))
                   (text-field :value (if (and cxn-inventory
                                               (not (member (get-configuration cxn-inventory :construction-inventory-processor-mode)
                                                            '(:default :heuristic-search nil))))
                                        (format nil ":~(~a~)"  (get-configuration cxn-inventory :de-render-mode))
                                        "")
                               :pattern "[^ ]*"
                               :title "Specify a single mode"
                               :id "custom-cip-mode"
                               :other-properties '(:class "ft_textbox checkbox-label")))))

                (table-row (table-data (vspacer)))

                (table-row
                 (table-data (html-label "Node expansion mode:"))
                 (table-row (table-data
                             (radio-button :value ":default"
                                           :id "single_cxn"
                                           :name "node_expansion_mode"
                                           :checked (and cxn-inventory
                                                         (eql :default (get-configuration cxn-inventory :node-expansion-mode))))
                             (html-label "expand using one construction at a time, not to be used in combination with heuristic search (<i>:default</i>)"
                                    :properties '(:for "single_cxn" :class "checkbox-label"))))
                 (table-row (table-data
                             (radio-button :name "node_expansion_mode"
                                           :value ":multiple-cxns"
                                           :id "multiple_cxns"
                                           :checked (and cxn-inventory
                                                         (eql :multiple-cxns
                                                              (get-configuration cxn-inventory :node-expansion-mode))))
                             (html-label "expand  with the constructions returned by the construction supplier (:multiple-cxns)"
                                    :properties '(:for "multiple_cxns" :class "checkbox-label"))))
                 (table-row (table-data
                             (radio-button :name "node_expansion_mode"
                                           :value ":full-expansion"
                                           :id "full_expansion"
                                           :checked (or (not cxn-inventory)
                                                        (eql :full-expansion
                                                             (get-configuration cxn-inventory :node-expansion-mode))))
                             (html-label "expand immediately with all constructions returned by the construction supplier (:full-expansion)"
                                    :properties '(:for "full_expansion" :class "checkbox-label"))))
                 (table-row
                  (table-data
                   (radio-button :name "node_expansion_mode"
                                 :value "custom_node_expansion_mode"
                                 :id "custom-node-expansion-radio"
                                 :checked (and cxn-inventory
                                               (not (member (get-configuration cxn-inventory :node-expansion-mode)
                                                            '(:default :full-expansion)))))
                   (text-field :value (if (and cxn-inventory
                                               (not (member (get-configuration cxn-inventory :node-expansion-mode)
                                                            '(:default :full-expansion))))
                                        (format nil ":~(~a~)"  (get-configuration cxn-inventory :node-expansion-mode))
                                        "")
                               :pattern "[^ ]*"
                               :title "Specify a single mode"
                               :id "custom-node-expansion-mode"
                               :other-properties '(:class "ft_textbox checkbox-label"))))))
    
         (table `(:id "heuristic_search_properties" ,@(if (or (not cxn-inventory)
                                                              (and cxn-inventory
                                                                   (eql :heuristic-search (get-configuration cxn-inventory :construction-inventory-processor-mode))))
                                                        '(:style "display:block;")
                                                        '(:style "display:none;")))
                (table-row (table-data (vspacer)))
                (table-row
                 (table-data (html-label "Search algorithm:"))
                 (table-data
                  (table-row
                   (table-data
                    (radio-button :value ":best-first"
                                  :id "best_first"
                                  :name "search_algorithm"
                                  :checked (or (not cxn-inventory)
                                               (not (get-configuration cxn-inventory :search-algorithm))
                                               (eql :best-first (get-configuration cxn-inventory :search-algorithm))))
                    (html-label "best-first search (<i>:best-first</i>)"
                           :properties '(:for "best_first" :class "checkbox-label") )))
                  (table-row
                   (table-data
                    (radio-button :value ":depth-first"
                                  :id "depth_first"
                                  :name "search_algorithm"
                                  :checked (eql :depth-first (get-configuration cxn-inventory :search-algorithm)))
                    (html-label "depth-first search (<i>:depth-first</i>)"
                           :properties '(:for "depth_first" :class "checkbox-label") )))
                  (table-row
                   (table-data
                    (radio-button :value ":breadth-first"
                                  :id "breadth_first"
                                  :name "search_algorithm"
                                  :checked (eql :breadth-first (get-configuration cxn-inventory :search-algorithm)))
                    (html-label "breadth-first search (<i>:breadth-first</i>)"
                           :properties '(:for "breadth_first" :class "checkbox-label") )))
                  (table-row
                   (table-data
                    (radio-button :value ":random"
                                  :id "random"
                                  :name "search_algorithm"
                                  :checked (eql :random (get-configuration cxn-inventory :search-algorithm)))
                    (html-label "random search (<i>:random</i>)"
                           :properties '(:for "random" :class "checkbox-label") )))
                  (table-row 
                   (table-data
                    (radio-button :name "search_algorithm"
                                  :value "custom-search-algorithm"
                                  :id "custom-search-algorithm-radio"
                                  :checked (and cxn-inventory
                                                (get-configuration cxn-inventory :search-algorithm)
                                                (not (member (get-configuration cxn-inventory :search-algorithm) '(:best-first :depth-first :breadth-first :random)))))
                    (text-field :value (if (and cxn-inventory
                                                (get-configuration cxn-inventory :search-algorithm)
                                                (not (member (get-configuration cxn-inventory :search-algorithm) '(:best-first :depth-first :breadth-first :random))))
                                         (format nil ":~(~a~)"  (get-configuration cxn-inventory :hash-mode))
                                         "")
                                :pattern "[^ ]*"
                                :title "Specify a single mode"
                                :id "custom-search-algorithm"
                                :other-properties '(:class "ft_textbox checkbox-label"))))))
           
                (table-row (table-data (vspacer)))
           
                (table-row (table-data (html-label "Heuristics"))
                           (table-row (table-data
                                       (checkbox :value ":nr-of-applied-cxns"
                                                 :id "nr-of-applied-cxns"
                                                 :checked (or (not cxn-inventory)
                                                              (not (get-configuration cxn-inventory :heuristics))
                                                              (and cxn-inventory
                                                                   (member :nr-of-applied-cxns (get-configuration cxn-inventory :heuristics)))))
                                       (html-label "number of constructions applied so far (<i>:nr-of-applied-cxns</i>)"
                                              :properties '(:class "checkbox-label" :for "nr-of-applied-cxns"))))
                           (table-row (table-data
                                       (checkbox :value ":nr-of-units-matched"
                                                 :id "nr-of-units-matched"
                                                 :checked (and cxn-inventory
                                                               (member :nr-of-units-matched (get-configuration cxn-inventory :heuristics))))
                                       (html-label "number of units that were matched (<i>:nr-of-units-matched</i>)"
                                              :properties '(:class "checkbox-label" :for "nr-of-units-matched"))))
                           (table-row (table-data
                                       (checkbox :value ":cxn-sets"
                                                 :id "cxn-sets"
                                                 :checked (and cxn-inventory
                                                               (member :cxn-sets (get-configuration cxn-inventory :heuristics))))
                                       (html-label "distance between the construction set of the applied construction and the current construction set (<i>:cxn-sets</i>)"
                                              :properties '(:class "checkbox-label" :for "cxn-sets"))))
                           (table-row (table-data
                                       (checkbox :value ":prefer-local-bindings"
                                                 :id "prefer-local-bindings"
                                                 :checked (and cxn-inventory
                                                               (member :prefer-local-bindings (get-configuration cxn-inventory :heuristics))))
                                       (html-label "prefer binding units that are sequentially closer to each other in the utterance (<i>:prefer-local-bindings</i>)"
                                              :properties '(:class "checkbox-label" :for "prefer-local-bindings"))))
                           (table-row (table-data
                                       (checkbox :value "other-heuristics"
                                                 :id "other_heuristics_checkbox"
                                                 :checked (and cxn-inventory
                                                               (set-difference (get-configuration cxn-inventory :heuristics)
                                                                               '(:nr-of-applied-cxns :nr-of-units-matched :cxn-sets :prefer-local-bindings))))
                                       (text-field :value (if cxn-inventory
                                                            (loop for h in (get-configuration cxn-inventory :heuristics)
                                                                  unless (member h '(:nr-of-applied-cxns :nr-of-units-matched :cxn-sets :prefer-local-bindings))
                                                                  collect h into other-h
                                                                  finally (return (format nil "~{:~(~a~)~^ ~}" other-h)))
                                                            "")
                                                   :pattern "[^,;]*"
                                                   :title "Separate goal test names by spaces"
                                                   :id "other_heuristics"
                                                   :other-properties '(:class "ft_textbox checkbox-label")))))
                (table-row (table-data (vspacer)))
                (table-row
                 (table-data (html-label "Heuristic value mode:"))
                 (table-data
                  (table-row
                   (table-data
                    (radio-button :value ":sum-heuristics-and-parent"
                                  :id "sum_heuristics_and_parent"
                                  :name "heuristic_value_mode"
                                  :checked (or (not cxn-inventory)
                                               (not (get-configuration cxn-inventory :heuristic-value-mode))
                                               (eql :sum-heuristics-and-parent (get-configuration cxn-inventory :heuristic-value-mode))))
                    (html-label "sum of all heuristics and heuristic value of parent (<i>:sum-heuristics-and-parent</i>)"
                           :properties '(:for "sum_heuristics_and_parent" :class "checkbox-label") )))
                  (table-row 
                   (table-data
                    (radio-button :name "heuristic_value_mode"
                                  :value "custom-heuristic-value-mode"
                                  :id "custom-heuristic-value-mode-radio"
                                  :checked (and cxn-inventory
                                                (get-configuration cxn-inventory :heuristic-value-mode)
                                                (not (member (get-configuration cxn-inventory :heuristic-value-mode) '(:sum-heuristics-and-parent)))))
                    (text-field :value (if (and cxn-inventory
                                                (get-configuration cxn-inventory :heuristic-value-mode)
                                                (not (member (get-configuration cxn-inventory :heuristic-value-mode) '(:sum-heuristics-and-parent))))
                                         (format nil ":~(~a~)"  (get-configuration cxn-inventory :heuristic-value-mode))
                                         "")
                                :pattern "[^ ]*"
                                :title "Specify a single mode"
                                :id "custom-heuristic-value-mode"
                                :other-properties '(:class "ft_textbox checkbox-label"))))))
                (table-row (table-data (vspacer)))
           
                (table-row
                 (table-data (html-label "Construction supplier:"))
                 (table-data
                  (table-row
                   (table-data
                    (radio-button :value ":all-cxns"
                                  :id "all_cxns"
                                  :name "cxn_supplier"
                                  :checked (or (not cxn-inventory)
                                               (eql :all-cxns (get-configuration cxn-inventory :cxn-supplier-mode))))
                    (html-label "apply all applicable constructions (<i>:all-cxns</i>)"
                           :properties '(:for "all_cxns" :class "checkbox-label") )))
                  (table-row
                   (table-data
                    (radio-button :value ":hashed"
                                  :id "hashed_cxn_supplier"
                                  :name "cxn_supplier"
                                  :checked (eql :hashed (get-configuration cxn-inventory :cxn-supplier-mode)))
                    (html-label "apply all relevant hashed constructions (depending on hash-mode) (<i>:hashed</i>)"
                           :properties '(:for "hashed_cxn_supplier" :class "checkbox-label") )))
                  (table-row
                   (table-data
                    (radio-button :value ":cxn-sets"
                                  :id "cxn_sets"
                                  :name "cxn_supplier"
                                  :checked (eql :cxn-sets (get-configuration cxn-inventory :cxn-supplier-mode)))
                    (html-label "apply all constructions of the same or later construction sets (<i>:cxn-sets</i>)"
                           :properties '(:for "cxn_sets" :class "checkbox-label") )))
                  (table-row
                   (table-data
                    (radio-button :value ":cxn-sets-hashed"
                                  :id "cxn_sets_hashed"
                                  :name "cxn_supplier"
                                  :checked (eql :cxn-sets-hashed (get-configuration cxn-inventory :cxn-supplier-mode)))
                    (html-label "apply all relevant hashed constructions of the same or later construction sets (<i>:cxn-sets-hashed</i>)"
                           :properties '(:for "cxn_sets_hashed" :class "checkbox-label") )))
                  (table-row 
                   (table-data
                    (radio-button :name "cxn_supplier"
                                  :value "custom-cxn-supplier"
                                  :id "custom-cxn-supplier-radio"
                                  :checked (and cxn-inventory
                                                (not (member (get-configuration cxn-inventory :cxn-supplier-mode) '(:all-cxns :cxn-sets :hashed :cxn-sets-hashed)))))
                    (text-field :value (if (and cxn-inventory
                                                (not (member (get-configuration cxn-inventory :cxn-supplier-mode) '(:all-cxns :cxn-sets :hashed :cxn-sets-hashed))))
                                         (format nil ":~(~a~)"  (get-configuration cxn-inventory :cxn-supplier-mode))
                                         "")
                                :pattern "[^ ]*"
                                :title "Specify a single mode"
                                :id "custom_cxn_supplier"
                                :other-properties '(:class "ft_textbox checkbox-label")))))))
    
         (table `(:id "default_search_properties" ,@(if (and cxn-inventory
                                                             (or
                                                              (eql :default
                                                                   (get-configuration cxn-inventory :construction-inventory-processor-mode))
                                                              (eql nil
                                                                   (get-configuration cxn-inventory :construction-inventory-processor-mode))))
                                                      '(:style "display:block;")
                                                      '(:style "display:none;")))
                (table-row (table-data (vspacer)))
                (table-row
                 (table-data (html-label "Priority mode:"))
                 (table-data
                  (table-row
                   (table-data
                    (radio-button :value ":nr-of-applied-cxns"
                                  :id "priority_mode_nr_of_applied_cxns"
                                  :name "priority_mode"
                                  :checked (or (not cxn-inventory)
                                               (not (get-configuration cxn-inventory :priority-mode))
                                               (or (eql :nr-of-applied-cxns (get-configuration cxn-inventory :priority-mode))
                                                   (eql :depth-first (get-configuration cxn-inventory :priority-mode)))))
                    (html-label "prefer nodes with more applied constructions (<i>:nr-of-applied-cxns</i> or <i>:depth-first</i>)"
                           :properties '(:for "priority_mode_nr_of_applied_cxns" :class "checkbox-label") )))
             
                  (table-row
                   (table-data
                    (radio-button :value ":nr-of-units-matched"
                                  :id "priority_mode_nr_of_units_matched"
                                  :name "priority_mode"
                                  :checked (and cxn-inventory (eql :nr-of-units-matched (get-configuration cxn-inventory :priority-mode))))
                    (html-label "prefer nodes in which more units were matched by the applied cxn (<i>:nr-of-units-matched</i>)"
                           :properties '(:for "priority_mode_nr_of_units_matched" :class "checkbox-label") )))
                  (table-row
                   (table-data
                    (radio-button :value ":priming"
                                  :id "priority_mode_priming"
                                  :name "priority_mode"
                                  :checked (and cxn-inventory (eql :priming (get-configuration cxn-inventory :priority-mode))))
                    (html-label "rely on priming data (<i>:priming</i>)"
                           :properties '(:for "priority_mode_priming" :class "checkbox-label") )))
             
                  (table-row 
                   (table-data
                    (radio-button :name "priority_mode"
                                  :value "custom-priority-mode"
                                  :id "custom-priority-mode-radio"
                                  :checked (and cxn-inventory
                                                (get-configuration cxn-inventory :priority-mode)
                                                (not (member (get-configuration cxn-inventory :priority-mode) '(:nr-of-applied-cxns :depth-first :nr-of-units-matched :priming)))))
                    (text-field :value (if (and cxn-inventory
                                                (get-configuration cxn-inventory :priority-mode)
                                                (not (member (get-configuration cxn-inventory :priority-mode) '(:nr-of-applied-cxns :depth-first :nr-of-units-matched :priming))))
                                         (format nil ":~(~a~)"  (get-configuration cxn-inventory :hash-mode))
                                         "")
                                :pattern "[^ ]*"
                                :title "Specify a single mode"
                                :id "custom-priority-mode"
                                :other-properties '(:class "ft_textbox checkbox-label"))))))
                (table-row (table-data (vspacer)))
                (table-row
                 (table-data (html-label "Queue mode:"))
                 (table-data

                  (table-row
                   (table-data
                    (radio-button :value ":depth-first"
                                  :id "depth_first_legacy"
                                  :name "queue_mode"
                                  :checked (or (not cxn-inventory)
                                               (not (get-configuration cxn-inventory :queue-mode))
                                               (eql :depth-first (get-configuration cxn-inventory :queue-mode))))
                    (html-label "queue constructions at the beginning of the queue (<i>:depth-first</i>)"
                           :properties '(:for "depth_first_legacy" :class "checkbox-label") )))

                  (table-row
                   (table-data
                    (radio-button :value ":breadth-first"
                                  :id "breadth_first_legacy"
                                  :name "queue_mode"
                                  :checked (or (not cxn-inventory)
                                               (not (get-configuration cxn-inventory :queue-mode))
                                               (eql :breadth-first (get-configuration cxn-inventory :queue-mode))))
                    (html-label "queue constructions at the end of the queue (<i>:breadth-first</i>)"
                           :properties '(:for "breadth_first_legacy" :class "checkbox-label") )))

                  (table-row
                   (table-data
                    (radio-button :value ":greedy-best-first"
                                  :id "best_first_legacy"
                                  :name "queue_mode"
                                  :checked (or (not cxn-inventory)
                                               (not (get-configuration cxn-inventory :queue-mode))
                                               (or (eql :greedy-best-first (get-configuration cxn-inventory :queue-mode))
                                                   (eql :by-priority (get-configuration cxn-inventory :queue-mode)))))
                    (html-label "queue constructions based on their priority score (<i>:greedy-best-first</i> or <i>:by-priority</i>)"
                           :properties '(:for "best_first_legacy" :class "checkbox-label") )))

                  (table-row
                   (table-data
                    (radio-button :value ":random-walk"
                                  :id "random_legacy"
                                  :name "queue_mode"
                                  :checked (or (not cxn-inventory)
                                               (not (get-configuration cxn-inventory :queue-mode))
                                               (eql :random-walk (get-configuration cxn-inventory :queue-mode))))
                    (html-label "queue constructions in a random fashion (<i>:random-walk</i>)"
                           :properties '(:for "random_legacy" :class "checkbox-label") )))
                         
                  (table-row 
                   (table-data
                    (radio-button :name "queue_mode"
                                  :value "custom-queue-mode"
                                  :id "custom-queue-mode-radio"
                                  :checked (and cxn-inventory
                                                (get-configuration cxn-inventory :queue-mode)
                                                (not (member (get-configuration cxn-inventory :queue-mode) '(:by-priority :random-walk :greedy-best-first :breadth-first :depth-first)))))
                    (text-field :value (if (and cxn-inventory
                                                (get-configuration cxn-inventory :queue-mode)
                                                (not (member (get-configuration cxn-inventory :queue-mode) '(:by-priority :random-walk :greedy-best-first :breadth-first :depth-first))))
                                         (format nil ":~(~a~)"  (get-configuration cxn-inventory :queue-mode))
                                         "")
                                :pattern "[^ ]*"
                                :title "Specify a single mode"
                                :id "custom-queue-mode"
                                :other-properties '(:class "ft_textbox checkbox-label"))))))
           
                (table-row (table-data (vspacer)))

                (table-row
                 (table-data (html-label "Construction supplier:"))
                 (table-data
             
                  (table-row
                   (table-data
                    (radio-button :value ":simple-queue"
                                  :id "simple_queue_legacy"
                                  :name "cxn_supplier_legacy"
                                  :checked (or (not cxn-inventory)
                                               (eql :simple-queue (get-configuration cxn-inventory :cxn-supplier-mode))))
                    (html-label "choose next cxn from unordered list (<i>:simple-queue</i>)"
                           :properties '(:for "simple_queue_legacy" :class "checkbox-label") )))

                  (table-row
                   (table-data
                    (radio-button :value ":ordered-by-score"
                                  :id "ordered_by_score_legacy"
                                  :name "cxn_supplier_legacy"
                                  :checked (eql :scores (get-configuration cxn-inventory :cxn-supplier-mode)))
                    (html-label "choose next cxn according to its score (higher is better) (<i>:scores</i>)"
                           :properties '(:for "ordered_by_score_legacy" :class "checkbox-label") )))
             
                  (table-row
                   (table-data
                    (radio-button :value ":ordered-by-label"
                                  :id "ordered_by_label_legacy"
                                  :name "cxn_supplier_legacy"
                                  :checked (eql :ordered-by-label (get-configuration cxn-inventory :cxn-supplier-mode)))
                    (html-label "choose next cxn according to its label (<i>:ordered-by-label</i>)"
                           :properties '(:for "ordered_by_label_legacy" :class "checkbox-label") )))
             
                  (table-row
                   (table-data
                    (radio-button :value ":ordered-by-label-and-score"
                                  :id "ordered_by_label_and_score_legacy"
                                  :name "cxn_supplier_legacy"
                                  :checked (eql :ordered-by-label-and-score (get-configuration cxn-inventory :cxn-supplier-mode)))
                    (html-label "choose next cxn according to its label and score (<i>:ordered-by-label-and-score</i>)"
                           :properties '(:for "ordered_by_label_and_score_legacy" :class "checkbox-label"))))

                  (table-row
                   (table-data
                    (radio-button :value ":hashed-simple-queue"
                                  :id "hashed_simple_queue_legacy"
                                  :name "cxn_supplier_legacy"
                                  :checked (or (not cxn-inventory)
                                               (eql :hashed-simple-queue (get-configuration cxn-inventory :cxn-supplier-mode))))
                    (html-label "choose next cxn based on hashing mode (<i>:hashed-simple-queue</i>)"
                           :properties '(:for "hashed_simple_queue_legacy" :class "checkbox-label") )))


                  (table-row
                   (table-data
                    (radio-button :value ":hashed-ordered-by-label"
                                  :id "hashed_ordered_by_label_legacy"
                                  :name "cxn_supplier_legacy"
                                  :checked (or (not cxn-inventory)
                                               (eql :hashed-ordered-by-label (get-configuration cxn-inventory :cxn-supplier-mode))))
                    (html-label "choose next cxn based on hashing mode and label (<i>:hashed-ordered-by-label</i>)"
                           :properties '(:for "hashed_ordered_by_label_legacy" :class "checkbox-label") )))
             
                  (table-row 
                   (table-data
                    (radio-button :name "cxn_supplier_legacy"
                                  :value "custom-cxn-supplier"
                                  :id "custom-cxn-supplier-radio-legacy"
                                  :checked (and cxn-inventory
                                                (not (member (get-configuration cxn-inventory :cxn-supplier-mode) '(:simple-queue :ordered-by-label :ordered-by-label-and-score :hashed-simple-queue :hashed-ordered-by-label)))))
                    (text-field :value (if (and cxn-inventory
                                                (not (member (get-configuration cxn-inventory :cxn-supplier-mode) '(:simple-queue :ordered-by-label :ordered-by-label-and-score :hashed-simple-queue :hashed-ordered-by-label))))
                                         (format nil ":~(~a~)"  (get-configuration cxn-inventory :cxn-supplier-mode))
                                         "")
                                :pattern "[^ ]*"
                                :title "Specify a single mode"
                                :id "custom_cxn_supplier_legacy"
                                :other-properties '(:class "ft_textbox checkbox-label"))))))))))
                     
  

(defun meta-layer (cxn-inventory)
  "For meta-layer."
  (top-level-section :title "Meta-layer"
                     :expanded? nil
                     :value
                     (span nil
                           (table nil
                                  (table-row (table-data
                                              (checkbox :value "t"
                                                        :id "use_meta_layer"
                                                        :checked (and cxn-inventory
                                                                      (get-configuration cxn-inventory :use-meta-layer))
                                                        :other-properties '(:onclick "toggleVisibilityCheckbox('use_meta_layer', 'diagnostics_and_repairs');"))
                                              (html-label "use meta-layer" :properties '(:for "use_meta_layer" :class "checkbox-label"))))
                                  (table-row (table-data
                                              (checkbox :value "t"
                                                        :id "consolidate_repairs"
                                                        :checked (and cxn-inventory (get-configuration cxn-inventory :consolidate-repairs)))
                                              (html-label "consolidate repairs" :properties '(:for "consolidate_repairs" :class "checkbox-label")))))
                           (table `(:id "diagnostics_and_repairs" ,@(if (and cxn-inventory (get-configuration cxn-inventory :use-meta-layer))
                                                                      '(:style "display:block;")
                                                                      '(:style "display:none;")))
                                  (table-row (table-data (vspacer)))
                                  (table-row (table-data
                                              (html-label "Diagnostics:" :properties '(:for "diagnostics")))
                                             (table-data
                                              (text-field :value (if cxn-inventory
                                                                   (format nil "~{~(~a~)~^ ~}" (mapcar #'type-of (diagnostics cxn-inventory)))
                                                                   "")
                                                          :pattern "[^,;]*"
                                                          :title "Separate by spaces"
                                                          :id "diagnostics"
                                                          :other-properties '(:class "ft_textbox"))))
                                  (table-row (table-data
                                              (html-label "Repairs:" :properties '(:for "repairs")))
                                             (table-data
                                              (text-field :value (if cxn-inventory
                                                                   (format nil "~{~(~a~)~^ ~}" (mapcar #'type-of (repairs cxn-inventory)))
                                                                   "")
                                                          :pattern "[^,;]*"
                                                          :title "Separate  by spaces"
                                                          :id "repairs"
                                                          :other-properties '(:class "ft_textbox"))))))))


(defun visualization (cxn-inventory)
  "Configurations for visualization."
  (top-level-section :expanded? nil
                     :title "Visualization"
                     :value (span nil
                                  (table nil
                                         (table-row
                                          (table-data (html-label "Features for hierarchy visualization (<i>:hierarchy-features</i>)" :properties '(:for "hierarchy_features")))
                                          (table-data
                                           (text-field :value (if cxn-inventory
                                                                (format nil "~{~(~a~)~^ ~}" (get-configuration (visualization-configuration cxn-inventory) :hierarchy-features))
                                                                "subunits")
                                                       :pattern "[^,;]*"
                                                       :title "Separate by spaces"
                                                       :id "hierarchy_features"
                                                       :other-properties '(:class "ft_textbox"))))
                                         (table-row
                                          (table-data (html-label "Feature for hierarchy visualization selected by default (<i>:selected-hierarchy</i>)" :properties '(:for "selected_hierarchy")))
                                          (table-data
                                           (text-field :value (if cxn-inventory
                                                                (format nil "~(~a~)" (get-configuration (visualization-configuration cxn-inventory) :selected-hierarchy))
                                                                "subunits")
                                                       :pattern "[^,; ]*"
                                                       :title "Enter a single feature name"
                                                       :id "selected_hierarchy"
                                                       :other-properties '(:class "ft_textbox"))))
                                         (table-row
                                          (table-data (html-label "Features to hide in the web interface (<i>:hide-features</i>)" :properties '(:for "hide_features")))
                                          (table-data
                                           (text-field :value (if cxn-inventory
                                                                (format nil "~{~(~a~)~^ ~}" (get-configuration (visualization-configuration cxn-inventory) :hide-features))
                                                                "footprints")
                                                       :pattern "[^,;]*"
                                                       :title "Separate by spaces"
                                                       :id "hide_features"
                                                       :other-properties '(:class "ft_textbox"))))
                                         (table-row
                                          (table-data (html-label "Restrict visualization in the web interface to these features (<i>:select-subfeatures</i>)" :properties '(:for "select_subfeatures")))
                                          (table-data
                                           (text-field :value (if cxn-inventory
                                                                (format nil "~{~(~a~)~^ ~}" (get-configuration (visualization-configuration cxn-inventory) :select-subfeatures))
                                                                "")
                                                       :pattern "[^,;]*"
                                                       :title "Separate by spaces"
                                                       :id "select_subfeatures"
                                                       :other-properties '(:class "ft_textbox")))))
                                  (vspacer)
                                  (table nil
                                         (table-row (table-data
                                                     (checkbox :value "t"
                                                               :id "with_search_debug_data"
                                                               :checked (or (not cxn-inventory)
                                                                            (get-configuration (visualization-configuration cxn-inventory) :with-search-debug-data)))
                                                     (html-label "show heuristic values of nodes in the web visualization (<i>:with-search-debug-data</i>)" :properties '(:for "with_search_debug_data" :class "checkbox-label"))))
                                         (table-row (table-data
                                                     (checkbox :value "t"
                                                               :id "remove_empty_units"
                                                               :checked (and cxn-inventory
                                                                             (get-configuration (visualization-configuration cxn-inventory) :remove-empty-units)))
                                                     (html-label "do not show empty units in the web interface  (<i>:remove-empty-units</i>)" :properties '(:for "remove_empty_units" :class "checkbox-label"))))
                                         (table-row (table-data
                                                     (checkbox :value "t"
                                                               :id "draw_meaning_as_network"
                                                               :checked (or (not cxn-inventory)
                                                                            (get-configuration cxn-inventory :draw-meaning-as-network)))
                                                     (html-label "draw meaning representations as predicate networks (<i>:draw-meaning-as-network</i>)" :properties '(:for "draw_meaning_as_network" :class "checkbox-label"))))
                                         (table-row (table-data
                                                     (checkbox :value "t"
                                                               :id "show_constructional_dependencies"
                                                               :checked (or (not cxn-inventory)
                                                                            (get-configuration (visualization-configuration cxn-inventory) :show-constructional-dependencies)))
                                                     (html-label "show the dependency network between the applied constructions (<i>:show-constructional-dependencies</i>)"
                                                            :properties '(:for "show_constructional_dependencies" :class "checkbox-label"))))
                                         (table-row (table-data
                                                     (checkbox :value "t"
                                                               :id "labeled_paths"
                                                               :checked (and cxn-inventory
                                                                             (get-configuration (visualization-configuration cxn-inventory) :labeled-paths)))
                                                     (html-label "show labels on edges in the constructional dependency network (<i>:labeled-paths</i>)"
                                                            :properties '(:for "labeled_paths" :class "checkbox-label"))))
                                         (table-row (table-data
                                                     (checkbox :value "t"
                                                               :id "colored_paths"
                                                               :checked (and cxn-inventory
                                                                             (get-configuration (visualization-configuration cxn-inventory) :colored-paths)))
                                                     (html-label "visualize the constructional dependency network using color (<i>:colored-paths</i>)"
                                                            :properties '(:for "colored_paths" :class "checkbox-label"))))
                                         )
                                  )))

                   

(defun other-configuration-settings (cxn-inventory)
  "For all other configuration settings"
  (top-level-section :title "Other configuration settings"
                     :value
                     `((textarea
                        :class "other-configurations"
                        :rows "5"
                        :cols "80"
                        :title "Enter an alist with configuration values."
                        :id "other_configuration_settings"
                        :placeholder "(:key . attribute)\&#10;(:key . attribute)\&#10;..."
                        )
                       ,(if cxn-inventory
                          (loop for conf in (entries (configuration cxn-inventory))
                                unless (member (car conf) '(:parse-goal-tests :production-goal-tests :feature-types :heuristic-value-mode
                                                            :heuristics :search-algorithm :construction-inventory-processor-mode :hash-mode
                                                            :create-initial-structure-mode :de-render-mode :render-mode
                                                            :parse-order :production-order :max-nr-of-nodes :max-search-depth :node-tests
                                                            :use-meta-layer :consolidate-repairs :node-expansion-mode :shuffle-cxns-before-application
                                                            :priority-mode :queue-mode :cxn-supplier-mode :draw-meaning-as-network))
                                collect conf into remaining-configurations
                                finally (return (format nil "~{~(~a~)~^ ~%~}" (loop for rc in remaining-configurations
                                                                                   for key = (if (keywordp (car rc))
                                                                                               (format nil ":~a" (car rc))
                                                                                               (format nil "~a" (car rc)))
                                                                                   for value = (loop for v in (listify (cdr rc))
                                                                                                     if (keywordp v)
                                                                                                     collect (format nil ":~a" v)
                                                                                                     else if (functionp v)
                                                                                                     collect (format nil "~a" (third (multiple-value-list
                                                                                                                     (function-lambda-expression v))))
                                                                                                     else collect (format nil "~a" v))
                                                                                   collect (if (listp (cdr rc))
                                                                                             (cons key value)
                                                                                             (cons key (car value)))))))
                          ""))))


(defun other-visualisation-settings (cxn-inventory)
  "For all other visualisation settings"
  (top-level-section :title "Other visualization settings"
                     :value
                     `((textarea
                        :class "other-configurations"
                        :rows "5"
                        :cols "80"
                        :title "Enter an alist with configuration values."
                        :id "other_visualization_settings"
                        :placeholder "(:key . attribute)\&#10;(:key . attribute)\&#10;...")
                       ,(if cxn-inventory
                          (loop for conf in (entries (visualization-configuration cxn-inventory))
                                unless (member (car conf) '(:hierarchy-features :colored-paths :labeled-paths :show-constructional-dependencies
                                                            :with-search-debug-data :select-subfeatures :hide-features :selected-hierarchy
                                                            :remove-empty-units))
                                collect conf into remaining-configurations
                                finally (return (format nil "~{~(~a~)~^ ~%~}" (loop for rc in remaining-configurations
                                                                                   for key = (if (keywordp (car rc))
                                                                                               (format nil ":~a" (car rc))
                                                                                               (format nil "~a" (car rc)))
                                                                                   for value = (loop for v in (listify (cdr rc))
                                                                                                     if (keywordp v)
                                                                                                     collect (format nil ":~a" v)
                                                                                                     else if (functionp v)
                                                                                                     collect (format nil "~a" (third (multiple-value-list
                                                                                                                     (function-lambda-expression v))))
                                                                                                     else collect (format nil "~a" v))
                                                                                   collect (if (listp (cdr rc))
                                                                                             (cons key value)
                                                                                             (cons key (car value)))))))
                          ""))))


(defun finish-configuration ()
  "Adding the reset and configure buttons."
  '((span)
    ((label :class "top-level-label") "Finish configuration:")
    ((div :class "top-level-value")
     ((table :class "gc-inner-table")
      ((tbody)
       ((tr)
        ((td)
         ((a :href "javascript:resetGrammarConfigurator();" :class "gc-button reset" :type "button") "Reset"))
        ((td)
         ((a :href "javascript:configureGrammar()"
             :class "gc-button configure") "Configure"))))))))



