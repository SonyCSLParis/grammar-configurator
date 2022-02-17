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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Grammar Configurator Demonstration                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file contains prototype code that was developed for research purposes and should not be used in production environments.
;; No warranties are provided.

;; Loading ... ;;
;;;;;;;;;;;;;;;;;

(ql:quickload '(:cl-json :fcg))
(in-package :fcg)

;; About the grammar configurator        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The grammar configurator is a graphical tool to design the configuration of an FCG grammar.
;; It takes the shape of an interactive form embedded in FCG's web interface.

;; The grammar configurator can be used to create a new configuration from scratch, or to visualize and
;; potentially alter the configuration of an existing grammar.

;; The output of the grammar configurator is a ready-to-evaluate def-fcg-constructions macro call.
;; This macro call is printed to the output buffer of your Common Lisp editor.


;; Launching the grammar configurator    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The grammar configuration can be used to create a new configuration from scratch, or to visualize
;; and potentially alter the configuration of an existing grammar.

;; Let us first consider the case where you would like to create a grammar from scratch. The grammar
;; configurator can be launched by evaluating the following macro call:

(configure-grammar)

;; The grammar configurator interface should now have appeared in the FCG web interface (http://localhost:8000 by default).

;; The grammar configurator interface consists of a number of collapsable/expandable elements (denoted by + and -).
;; The form is initialised by sensible default values. Hitting the 'Reset' button at the bottom will reset these values.
;; Hitting the 'Configure' button will write the configuration to the output browser of your Common Lisp editor. You can
;; then copy/paste this configuration to your grammar file and add constructions to the construction inventory.

;; If the aim is to inspect or alter the configuration of an existing construction inventory, the configure-grammar macro
;; can be called with construction inventory as argument.
                  
(load-demo-grammar) ;; loads a grammar into *fcg-constructions*
(configure-grammar *fcg-constructions*)

;; The grammar configurator now reflects the current state of the configuration of the chosen grammar and can be used
;; to browse through it in a graphical way. The configuration can also be adapted using the form. Upon hitting the
;; 'Configure' button, the adapted configuration is printed to the output browser of your Common Lisp editor.


