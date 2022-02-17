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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                             ;;       
;; This file contains the main code implementing the functionality of the grammar configurator ;;
;;                                                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Calling the grammar configurator... ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro configure-grammar (&optional cxn-inventory)
  "A macro is needed in order to be able to access the variable in which the cxn-inventory is stored."
  `(do-configure-grammar ',cxn-inventory))

(defun do-configure-grammar (cxn-inventory-symbol)
  "Launches the grammar configurator in the web interface."
  (let ((cxn-inventory (cond ((and cxn-inventory-symbol
                                   (boundp cxn-inventory-symbol)
                                   (or (eql (type-of (symbol-value cxn-inventory-symbol)) 'fcg-construction-set)
                                       (eql (type-of (symbol-value cxn-inventory-symbol)) 'hashed-fcg-construction-set)))
                              (symbol-value cxn-inventory-symbol))
                             (t nil)))) 
    ;; Empty page
    (clear-page)
    ;; Add header
    (add-element '((hr)))
    (add-element '((h1) "FCG Grammar Configurator"))
    (add-element '((hr)))
    ;; Add form
    (add-element
     `((form :class "grammar-configurator" :name "gc" :id "gc")
       ,(grammar-name cxn-inventory-symbol)
       ,(custom-feature-types cxn-inventory)
       ,(hashing cxn-inventory)
       ,(construction-sets cxn-inventory)
       ,(rendering cxn-inventory)
       ,(goal-tests cxn-inventory)
       ,(node-tests cxn-inventory)
       ,(search-configuration cxn-inventory)
       ,(meta-layer cxn-inventory)
       ,(visualization cxn-inventory)
       ,(other-configuration-settings cxn-inventory)
       ,(other-visualisation-settings cxn-inventory)
       ,(finish-configuration)))))


;; When the 'configure' button is hit... ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ht-simple-ajax:defun-ajax make-grammar-configuration
                           (grammar-configuration) (wi::*ajax-processor*)
  "Ajax function redirecting input to grammar-configuration function."
  (do-make-grammar-configuration grammar-configuration)
  nil)

(defun do-make-grammar-configuration (grammar-configuration)
  "Function doing the grammar configuration."
  (let* ((package :fcg)
         (form-contents (cl-json:decode-json-from-string grammar-configuration))
         (grammar-information (cdr (assoc :grammar-information form-contents)))
         (fcg-configurations (parse-and-intern-fcg-configurations (cdr (assoc :fcg-configurations form-contents)) package))
         (visualization-configurations (parse-and-intern-fcg-configurations (cdr (assoc :visualization-configurations form-contents)) :fcg))
         (grammar-name (make-id (intern (upcase (cdr (assoc :grammar-name grammar-information))) package)))
         (cxn-inventory (intern (upcase (cdr (assoc :grammar-name grammar-information))) package))
         (hashed  (cdr (assoc :hashed grammar-information)))
         (hierarchy-features  (mapcar #'(lambda (string) (intern (upcase string) package))
                                      (cdr (assoc :hierarchy-features grammar-information))))
         (feature-types (parse-and-intern-feature-types (cdr (assoc :feature-types grammar-information)) package))
         (diagnostics (mapcar #'(lambda (string) (intern (upcase string) package))
                              (cdr (assoc :diagnostics grammar-information))))
         (repairs (mapcar #'(lambda (string) (intern (upcase string) package))
                              (cdr (assoc :repairs grammar-information))))
         (other-fcg-configurations (read-from-string (cdr (assoc :other-fcg-configurations form-contents))))
         (other-visualization-configurations (read-from-string (cdr (assoc :other-visualization-configurations form-contents)))))

    (format t "~%~%~%
(def-fcg-constructions ~(~a~)
  :cxn-inventory ~(~a~)
  :hashed ~(~a~)
  :feature-types (~{(~{~(~a~)~^ ~})~^~%                  ~})
  :hierarchy-features (~{~(~a~)~^ ~})
  :diagnostics (~{~(~a~)~^~%                ~})
  :repairs (~{~(~a~)~^~%            ~})
  :fcg-configurations (~{~a~^~%                       ~})
  :visualization-configurations (~{~a~^~%                                 ~})

;; Add your constructions here...


)~%~%~%"
            grammar-name
            cxn-inventory
            hashed
            feature-types
            hierarchy-features
            diagnostics
            repairs
            (mapcar #'(lambda (conf-item) (format-configuration-item conf-item)) (append fcg-configurations other-fcg-configurations))
            (mapcar #'(lambda (conf-item) (format-configuration-item conf-item)) (append visualization-configurations other-visualization-configurations))
           )))

(defun format-configuration-item (configuration-item)
  (let* ((key (car configuration-item))
         (value (cdr configuration-item))
         (dotted? (atom value))
         (function? (unless dotted? (eql 'function (first value)))))
    (if (or dotted? function?)
      (format nil "(~a . ~a)"
              (format-keyword-or-symbol key)
              (format-keyword-or-symbol value))
      (format nil "(~a ~{~a~^ ~})"
              (format-keyword-or-symbol key)
              (mapcar #'(lambda (atom) (format-keyword-or-symbol atom)) value)))))

(defun format-keyword-or-symbol (atom)
  (cond ((and (listp atom) (eql 'function (first atom)))
         (format nil "~(~a~)" (second atom)))
        ((keywordp atom)
         (format nil ":~(~a~)" atom))
        ((functionp atom)
         (format nil "~(~a~)" atom))
        (t
         (format nil "~(~a~)" atom))))

(defun parse-and-intern-feature-types (feature-types package)
  (loop for (type . features) in feature-types
        append (loop for feature in features collect (list (intern (upcase feature) package) (intern (upcase type) package)))))  

(defun parse-and-intern-fcg-configurations (fcg-configurations package)
  (loop for (key . value) in fcg-configurations
        collect (cons key (parse-and-intern-symbol value package))))


(defun parse-and-intern-symbol (symbol-or-list package)
  (cond ((null symbol-or-list)
         nil)
        ((symbolp symbol-or-list)
         symbol-or-list)
        ((stringp symbol-or-list)
         (cond ((equalp ":" (subseq symbol-or-list 0 1))
                (make-kw (subseq symbol-or-list 1)))
               ((numberp (read-from-string symbol-or-list))
                (parse-integer symbol-or-list))
               (t
                (intern (upcase symbol-or-list) package))))
        (t
         (loop for element in symbol-or-list
               collect (parse-and-intern-symbol element package)))))


;; Utilities ;;
;;;;;;;;;;;;;;;

(defun features-of-type (feature-type cxn-inventory)
  "Returns the features declared as a particular type in a cxn-inventory"
  (loop for feature in (feature-types cxn-inventory)
        when (eql (second feature) feature-type)
        collect (first feature)))

