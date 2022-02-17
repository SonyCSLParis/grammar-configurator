# grammar-configurator
A library for Fluid Construction Grammar that allows to inspect and modify the configuration of a grammar through FCG's interactive web interface.

## Introduction

The grammar conﬁgurator is a graphical tool to design the conﬁguration of an FCG grammar. It takes the shape of an interactive form embedded in FCG’s web interface.
The grammar conﬁgurator can be used to create a new conﬁguration from scratch, or to visualize and potentially alter the conﬁguration of an existing grammar.
The output of the grammar conﬁgurator is a ready-to-evaluate def-fcg-constructions macro call. This macro call is printed to the output buﬀer of your Common Lisp editor.

## Installation and Loading

In order to use the grammar conﬁgurator, the folder grammar-configurator should be moved to Babel’s folder systems/fcg, and a reference to the ﬁles should be added to systems/fcg/fcg.asd after the module :constructional-dependencies, as follows:

````
(:module grammar-configurator
  :serial t
  :components ((:file ”grammar-configurator”)
               (:file ”html”)
               (:file ”css”)
               (:file ”js”)))
````

Additionally, :cl-json should be added to the :depends-on list of the :fcg system, just after :s-dot. The grammar conﬁgurator is automatically loaded upon loading the :fcg system, into the :fcg package.
The code was designed for compatibility with the current version of babel-core (https: //gitlab.ai.vub.ac.be/ehai/babel-core – commit 451f0bfe99306bc8d4d2114f478bc8a2f2d42658 and has been tested using Clozure Com-mon Lisp version 1.12.
A demonstration of the possibilities is included in the ﬁle demo.lisp.

## General Architecture

The grammar conﬁgurator consists of a combination of Common Lisp code for interacting with the FCG platform (grammar-configurator.lisp), Lisp-generated html code for the structure of the web form (html.lisp), Lisp-embedded css code for the visual appearance of the form (css.lisp), and Lisp-embedded JavaScript code for the facilitating interaction with the form (js.lisp).

The web form is created from the macro configure-grammar. Via the function do-configure-grammar, the html code for the form is generated through calls to the functions in html.lisp. These functions receive the construction inventory as argument and should return html code to be pushed to the web interface.
Upon hitting the Conﬁgure button, the JavaScript function configureGrammar() is called. This function extracts all information from the form, structures it, and calls the Lisp function do-make-grammar-configuration with the information from the form as argument. The information is then parsed and printed to the output buﬀer of the Lisp editor of the form of a def-fcg-constructions macro call.

## Usage of the grammar configurator

### Loading

If properly installed as explained above, the grammar conﬁgurator is loaded with the :fcg system, into the :fcg package.

(ql:quickload :fcg)
(in-package :fcg)

### Launching and Configuring

The grammar conﬁguration can be used to create a new conﬁguration from scratch, or to visualize and potentially alter the conﬁguration of an existing grammar.
Let us ﬁrst consider the case where you would like to create a grammar from scratch. The grammar conﬁgurator can be launched by evaluating the following macro call:

````
(configure-grammar)
````

The grammar conﬁgurator interface should now have appeared in the FCG web interface (http://localhost:8000 by default).

The grammar conﬁgurator interface consists of a number of collapsible/expandable elements (denoted by ‘+’ and ‘-’). The form is initialised by sensible default values. Hitting the Reset button at the bottom will reset these values.

Hitting the Conﬁgure button will write the conﬁguration to the output browser of your Common Lisp editor. You can then copy/paste this conﬁguration to your grammar ﬁle and add constructions to the construction inventory.

If the aim is to inspect or alter the conﬁguration of an existing construction inventory, the conﬁgure-grammar macro can be called with construction inventory as argument.

````
(load-demo-grammar) ;; loads a grammar into *fcg-constructions* 
(configure-grammar *fcg-constructions*)
````

The grammar conﬁgurator now reﬂects the current state of the conﬁguration of the chosen grammar and can be used to browse through it in a graphical way. The conﬁguration can alsobe adapted using the form. Upon hitting the Conﬁgure button, the adapted conﬁguration is printed to the output browser of your Common Lisp editor.
