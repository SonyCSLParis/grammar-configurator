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
;; This file contains all css code that is related to the grammar configurator.              ;;
;;                                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-css 'grammar-configurator "

.grammar-configurator {
    margin-top: 15px;
    border: 1px solid grey;
    border-radius: 15px;
    width: auto;
    display: inline-block;
    padding: 10px;
}


.grammar-configurator td {
    padding: 10px;
}

.gc-button {
  border: none;
  color: white;
  padding: 10px 32px;
  width: auto;
  text-align: center;
  text-decoration: none;
  display: inline-block;
  border-radius: 4px;
}

.reset {
  background-color: #f44336; 
}

.configure {
  background-color: #4CAF50; 
}

.grammar-configurator a:link, a:visited, a:hover, a:active {

  text-decoration: none;

}

.reset:hover {

  background-color: #aa1409;

}

.reset:active {

 background-color: #f44336; 

}

.configure:hover {

  background-color: #2f6a31;

}

.configure:active {

 background-color: #4CAF50;
}

.feature-type-label  {

 display: inline-block;
margin-right: 10px;

}

.gc-inner-table td {

 padding-top: 0px;
 padding-bottom: 0px;
 padding-right: 15px;
 padding-left: 0px;

}

.top-level-label {

 display: block;
 font-weight: bold;
 margin-bottom:10px;

}

.top-level-value {

margin-bottom:15px;
margin-left:25px;

}

.checkbox-label {

margin-left:5px;

}

.vspacer {

 height:15px;

}

.ft_textbox {

  width: 250px;

}

.other-configurations {

  font-family: Helvetica Neue, Helvetica, Arial;
  font-size: 9pt;

}

input {
  font-family: Helvetica Neue, Helvetica, Arial;
  font-size: 9pt;
}

")