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
;; This file contains all javascript functions that are related to the grammar configurator. ;;
;;                                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reset Form ;;
;;;;;;;;;;;;;;;;

(define-js 'resetGrammarConfigurator "
function resetGrammarConfigurator() {

    document.getElementById('gc').reset();
    toggleVisibilityCheckbox('use_meta_layer', 'diagnostics_and_repairs');
    toggleVisibilityCheckbox('restrict_nr_of_nodes', 'max_nr_of_nodes_field');
    toggleVisibilityCheckbox('restrict_search_depth', 'max_search_depth_field');

}
")

;; Extract information from form ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-js 'configureGrammar "
function configureGrammar() {

    // bind grammar configurator form to gc variable
    var gc = document.getElementById('gc');

    // Report validity of form entries (e.g. fill this field)
    gc.reportValidity();

    var grammar_name = grammarName();
    var feature_types = featureTypes();
    var parse_goal_tests = parseGoalTests();
    var production_goal_tests = productionGoalTests();
    var node_tests = nodeTests();
    var parse_order = cxnSetsComprehension();
    var production_order = cxnSetsProduction();
    var render_mode = renderMode();
    var de_render_mode = deRenderMode();
    var use_meta_layer = useMetaLayer();
    var consolidate_repairs = consolidateRepairs();
    var diagnostics = getDiagnostics();
    var draw_meaning_as_network = drawMeaningAsNetwork();
    var repairs = getRepairs();
    var shuffle = shuffleCxns();
    var visualization_checkboxes = visualizationCheckboxes();
    var visualization_textfields = visualizationTextfields();
    var heuristic_search_configuration = searchHeuristic();
    var legacy_search_configuration = searchLegacy();
    var hashed_cxn_set = hashedCxnSet();
    var hierarchy_features = hierarchyFeatures();
    var hash_mode = hashMode();


    var other_configurations = otherConfigurations();
    var other_visualization_configurations = otherVisualizationConfigurations();
  
    if (gc.checkValidity()) {
    ajax_make_grammar_configuration(JSON.stringify({'grammarInformation':
                                                    {...grammar_name,
                                                     ...hashed_cxn_set,
                                                     ...feature_types,
                                                     ...diagnostics,
                                                     ...repairs,
                                                     ...hierarchy_features},
                                                   'otherFcgConfigurations': other_configurations,
                                                   'otherVisualizationConfigurations': other_visualization_configurations,
                                                   'fcgConfigurations':
                                                    {...parse_goal_tests,
                                                     ...production_goal_tests,
                                                     ...node_tests,
                                                     ...parse_order,
                                                     ...production_order,
                                                     ...render_mode,
                                                     ...de_render_mode,
                                                     ...use_meta_layer,
                                                     ...consolidate_repairs,
                                                     ...shuffle,
                                                     ...draw_meaning_as_network,
                                                     ...heuristic_search_configuration,
                                                     ...hash_mode,
                                                     ...legacy_search_configuration
                                                    },
                                                    'visualizationConfigurations':
                                                    {...visualization_checkboxes,
                                                     ...visualization_textfields
                                                     }}));
    alert('Your new grammar configuration has been successfully printed to the output buffer of your editor!');
    }
}
")

(define-js 'grammarName "
function grammarName() {

    var gc = document.getElementById('gc');

    var grammar_name_dict = {'grammarName': gc.grammar_name.value};

    return grammar_name_dict
}
")

(define-js 'cxnSetsComprehension "
function cxnSetsComprehension() {

    var gc = document.getElementById('gc');

    var cxn_sets = gc.parse_order.value.split(' ').filter(Boolean);
    var parse_order = {'parseOrder': cxn_sets};
    
    return parse_order
}
")

(define-js 'cxnSetsProduction "
function cxnSetsProduction() {

    var gc = document.getElementById('gc');

    var cxn_sets = gc.production_order.value.split(' ').filter(Boolean);
    var production_order = {'productionOrder': cxn_sets};
    
    return production_order
}
")

(define-js 'useMetaLayer "
function useMetaLayer() {

    var uml = document.getElementById('use_meta_layer').checked;
    var use_meta_layer = {'useMetaLayer': uml};
    
    return use_meta_layer
}
")

(define-js 'hashedCxnSet "
function hashedCxnSet() {

    var hashed = document.getElementById('hashed').checked;
    var hashed_return = {'hashed': hashed};
    
    return hashed_return
}
")


(define-js 'hashMode "
function hashMode() {

    var gc = document.getElementById('gc');

    if (document.getElementById('hashed').checked) {

    var hash_mode_buttons = ['hash-word-entity-root-one-pole'];
    var hash_mode_checked = '';
    for (r of hash_mode_buttons) {
        if  (document.getElementById(r).checked) {
            hash_mode_checked = document.getElementById(r).value;
        }
    }

    if (hash_mode_checked === '') {
       hash_mode_checked = document.getElementById('custom-hash-mode').value.split(' ').filter(Boolean)[0];
    }

    var hash_mode  = {hashMode: hash_mode_checked};

    return hash_mode
}
}
")

(define-js 'consolidateRepairs "
function consolidateRepairs() {

    var cr = document.getElementById('consolidate_repairs').checked;
    var consolidate_repairs = {'consolidateRepairs': cr};
    
    return consolidate_repairs
}
")

(define-js 'getDiagnostics "
function getDiagnostics() {

    var d = null;
    if (document.getElementById('use_meta_layer').checked) {
        d = document.getElementById('diagnostics').value.split(' ').filter(Boolean);
    }

    diagnostics = {'diagnostics': d}

    return diagnostics
}
")

(define-js 'getRepairs "
function getRepairs() {

    var r = null;
    if (document.getElementById('use_meta_layer').checked) {
        r = document.getElementById('repairs').value.split(' ').filter(Boolean);
    }

    repairs = {'repairs': r}

    return repairs
}
")

(define-js 'featureTypes "
function featureTypes() {

    var gc = document.getElementById('gc');

    var feature_types = {featureTypes: {'set': gc.ft_set.value.split(' ').filter(Boolean),
                                        'sequence': gc.ft_sequence.value.split(' ').filter(Boolean),
                                        'setOfPredicates': gc.ft_set_of_predicates.value.split(' ').filter(Boolean),
                                        'sequenceOfPredicates': gc.ft_sequence_of_predicates.value.split(' ').filter(Boolean)}};
    
    return feature_types
}
")

(define-js 'renderMode "
function renderMode() {

    var gc = document.getElementById('gc');

    var render_mode_buttons = ['generate-and-test'];
    var render_mode_checked = '';
    for (r of render_mode_buttons) {
        if  (document.getElementById(r).checked) {
            render_mode_checked = document.getElementById(r).value;
        }
    }

    if (render_mode_checked === '') {
       render_mode_checked = document.getElementById('custom-render-mode').value.split(' ').filter(Boolean)[0];
    }

    var render_mode  = {renderMode: render_mode_checked};

    return render_mode
}
")

(define-js 'deRenderMode "
function deRenderMode() {

    var gc = document.getElementById('gc');

    var de_render_mode_buttons = ['de-render-string-meets', 'de-render-string-meets-precedes', 'de-render-string-meets-precedes-first'];
    var de_render_mode_checked = '';
    for (r of de_render_mode_buttons) {
        if  (document.getElementById(r).checked) {
            de_render_mode_checked = document.getElementById(r).value;
        }
    }

    if (de_render_mode_checked === '') {
       de_render_mode_checked = document.getElementById('custom-de-render-mode').value.split(' ').filter(Boolean)[0];
    }

    var de_render_mode  = {deRenderMode: de_render_mode_checked};

    return de_render_mode
}
")

(define-js 'otherConfigurations "
function otherConfigurations() {

    var other_configurations = document.getElementById('other_configuration_settings').value;
    return '(' + other_configurations + ')'
}
")

(define-js 'otherVisualizationConfigurations "
function otherVisualizationConfigurations() {

    var other_visualization_configurations = document.getElementById('other_visualization_settings').value;
    return  '(' + other_visualization_configurations + ')'
}
")


(define-js 'parseGoalTests "
function parseGoalTests() {

    var gc = document.getElementById('gc');

    var parse_goal_test_checkboxes = ['no-applicable-cxns-comprehension','no-strings-in-root','connected-semantic-network','always-fail-comprehension'];
    var parse_goal_tests_checked = [];
    for (pgtc of parse_goal_test_checkboxes) {
        if  (document.getElementById(pgtc).checked) {
            parse_goal_tests_checked.push(document.getElementById(pgtc).value);
        }
    }

    var other_goal_tests = [];
    if (document.getElementById('other-goal-tests-comprehension').checked) {
         other_goal_tests = gc.other_parse_goal_tests.value.split(' ').filter(Boolean);
    }

    var all_goal_tests = parse_goal_tests_checked.concat(other_goal_tests);
    var parse_goal_tests  = {parseGoalTests: all_goal_tests};

    return parse_goal_tests
}
")

(define-js 'productionGoalTests "
function productionGoalTests() {

    var gc = document.getElementById('gc');

    var production_goal_test_checkboxes = ['no-applicable-cxns-production','no-meaning-in-root','connected-structure','always-fail-production'];
    var production_goal_tests_checked = [];
    for (pgtc of production_goal_test_checkboxes) {
        if  (document.getElementById(pgtc).checked) {
            production_goal_tests_checked.push(document.getElementById(pgtc).value);
        }
    }

    var other_goal_tests = [];
    if (document.getElementById('other-goal-tests-production').checked) {
         other_goal_tests = gc.other_production_goal_tests.value.split(' ').filter(Boolean);
    }

    var all_goal_tests = production_goal_tests_checked.concat(other_goal_tests);
    var production_goal_tests  = {productionGoalTests: all_goal_tests};

    return production_goal_tests
}
")

(define-js 'nodeTests "
function nodeTests() {

    var gc = document.getElementById('gc');

    var node_test_checkboxes = ['check_duplicate','restrict_nr_of_nodes','restrict_search_depth'];
    var node_tests_checked = [];
    for (nt of node_test_checkboxes) {
        if  (document.getElementById(nt).checked) {
            node_tests_checked.push(document.getElementById(nt).value);
        }
    }

    var other_node_tests = [];
    if (document.getElementById('other-node-tests-button').checked) {
         other_node_tests = gc.other_node_tests.value.split(' ').filter(Boolean);
    }

    var all_node_tests = node_tests_checked.concat(other_node_tests);


    var max_nr_of_nodes = null;
    var max_search_depth = null;

    if (document.getElementById('restrict_nr_of_nodes').checked) {
       max_nr_of_nodes = document.getElementById('max_nr_of_nodes').value.split(' ').filter(Boolean)[0];
    }

    if (document.getElementById('restrict_search_depth').checked) {
       max_search_depth = document.getElementById('max_search_depth').value.split(' ').filter(Boolean)[0];
    }

    var node_tests  = {nodeTests: all_node_tests,
                       maxNumberOfNodes: max_nr_of_nodes,
                       maxSearchDepth: max_search_depth};

    return node_tests
}
")

(define-js 'shuffleCxns "
function shuffleCxns() {

    var gc = document.getElementById('gc');

    var shuffle = document.getElementById('shuffle_cxns').checked;
   
    var shuffle_cxns_before_application  = {shuffleCxnsBeforeApplication: shuffle};

    return shuffle_cxns_before_application
}
")

(define-js 'drawMeaningAsNetwork "
function drawMeaningAsNetwork() {

    var gc = document.getElementById('gc');

    var draw_meaning = document.getElementById('draw_meaning_as_network').checked;
   
    var draw_meaning_as_network  = {drawMeaningAsNetwork: draw_meaning};

    return draw_meaning_as_network
}
")

(define-js 'visualizationCheckboxes "
function visualizationCheckboxes() {

    var gc = document.getElementById('gc');

    var search_debug = document.getElementById('with_search_debug_data').checked;
    var remove_empty_units =  document.getElementById('remove_empty_units').checked;
    var cxn_dependencies = document.getElementById('show_constructional_dependencies').checked;
    var labeled_paths =  document.getElementById('labeled_paths').checked;
    var colored_paths =  document.getElementById('colored_paths').checked;
    
    var visualization_checkboxes  = {withSearchDebugData: search_debug,
                                     removeEmptyUnits: remove_empty_units,
                                     showConstructionalDependencies: cxn_dependencies,
                                     labeledPaths: labeled_paths,
                                     coloredPaths: colored_paths};

    return visualization_checkboxes
}
")

(define-js 'hierarchyFeatures "
function hierarchyFeatures() {

    var gc = document.getElementById('gc');

    var hierarchy_features = gc.hierarchy_features.value.split(' ').filter(Boolean);

    var hf = {hierarchyFeatures: hierarchy_features};
    
    return hf
}
")


(define-js 'visualizationTextfields "
function visualizationTextfields() {

    var gc = document.getElementById('gc');

    var hierarchy_features = gc.hierarchy_features.value.split(' ').filter(Boolean);
    var selected_hierarchy = gc.selected_hierarchy.value.split(' ').filter(Boolean)[0];
    var hide_features = gc.hide_features.value.split(' ').filter(Boolean);
    var select_subfeatures = gc.select_subfeatures.value.split(' ').filter(Boolean);

    var visualization_textfields = {hierarchyFeatures: hierarchy_features,
                                    selectedHierarchy: selected_hierarchy,
                                    hideFeatures: hide_features,
                                    selectSubfeatures: select_subfeatures};
    
    return visualization_textfields
}
")

(define-js 'searchHeuristic "
function searchHeuristic() {

  var gc = document.getElementById('gc');

  if (document.getElementById('heuristic_search').checked) {

    var cip_mode_checked = document.getElementById('heuristic_search').value;

    var node_expansion_mode_buttons = ['single_cxn', 'multiple_cxns', 'full_expansion'];
    var node_expansion_mode_checked = '';
    for (r of node_expansion_mode_buttons) {
      if  (document.getElementById(r).checked) {
        node_expansion_mode_checked = document.getElementById(r).value;
      }
    }

    if (node_expansion_mode_checked === '') {
      node_expansion_mode_checked = document.getElementById('custom-node-expansion-mode').value.split(' ').filter(Boolean)[0];
    }

    var search_algorithm_buttons = ['depth_first', 'best_first', 'breadth_first', 'random'];
    var search_algorithm_checked = '';
    for (r of search_algorithm_buttons) {
      if  (document.getElementById(r).checked) {
        search_algorithm_checked = document.getElementById(r).value;
      }
    }

    if (search_algorithm_checked === '') {
      search_algorithm_checked = document.getElementById('custom-search-algorithm').value.split(' ').filter(Boolean)[0];
    }


    var heuristic_value_buttons = ['sum_heuristics_and_parent'];
    var heuristic_value_checked = '';
    for (r of heuristic_value_buttons) {
      if  (document.getElementById(r).checked) {
        heuristic_value_checked = document.getElementById(r).value;
      }
    }

    if (heuristic_value_checked === '') {
      heuristic_value_checked = document.getElementById('custom-heuristic-value-mode').value.split(' ').filter(Boolean)[0];
    }

    var cxn_supplier_buttons = ['all_cxns', 'hashed_cxn_supplier', 'cxn_sets', 'cxn_sets_hashed'];
    var cxn_supplier_checked = '';
    for (r of cxn_supplier_buttons) {
      if  (document.getElementById(r).checked) {
        cxn_supplier_checked = document.getElementById(r).value;
      }
    }

    if (cxn_supplier_checked === '') {
      cxn_supplier_checked = document.getElementById('custom_cxn_supplier').value.split(' ').filter(Boolean)[0];
    }


    var heuristics_checkboxes = ['nr-of-applied-cxns','nr-of-units-matched','cxn-sets','prefer-local-bindings'];
    var heuristics_checked = [];
    for (r of heuristics_checkboxes) {
        if  (document.getElementById(r).checked) {
            heuristics_checked.push(document.getElementById(r).value);
        }
    }

    var other_heuristics = [];
    if (document.getElementById('other_heuristics_checkbox').checked) {
         other_heuristics = gc.other_heuristics.value.split(' ').filter(Boolean);
    }

    var all_heuristics = heuristics_checked.concat(other_heuristics);

    var heuristic_search_configuration  = {constructionInventoryProcessorMode: cip_mode_checked,
                                           nodeExpansionMode: node_expansion_mode_checked,
                                           searchAlgorithm: search_algorithm_checked,
                                           heuristicValueMode: heuristic_value_checked,
                                           cxnSupplierMode: cxn_supplier_checked,
                                           heuristics: all_heuristics};

    return heuristic_search_configuration
  }
}
")

(define-js 'searchLegacy "
function searchLegacy() {

  var gc = document.getElementById('gc');

  if (document.getElementById('legacy').checked) {

    var cip_mode_checked = document.getElementById('legacy').value;

    var node_expansion_mode_buttons = ['single_cxn', 'multiple_cxns','full_expansion'];
    var node_expansion_mode_checked = '';
    for (r of node_expansion_mode_buttons) {
      if  (document.getElementById(r).checked) {
        node_expansion_mode_checked = document.getElementById(r).value;
      }
    }

    if (node_expansion_mode_checked === '') {
      node_expansion_mode_checked = document.getElementById('custom-node-expansion-mode').value.split(' ').filter(Boolean)[0];
    }

    var priority_mode = ['priority_mode_nr_of_applied_cxns', 'priority_mode_nr_of_units_matched', 'priority_mode_priming'];
    var priority_mode_checked = '';
    for (r of priority_mode) {
      if  (document.getElementById(r).checked) {
        priority_mode_checked = document.getElementById(r).value;
      }
    }

    if (priority_mode_checked === '') {
      priority_mode_checked = document.getElementById('custom-priority-mode').value.split(' ').filter(Boolean)[0];
    }

    var queue_mode = ['depth_first_legacy', 'breadth_first_legacy', 'best_first_legacy', 'random_legacy'];
    var queue_mode_checked = '';
    for (r of queue_mode) {
      if  (document.getElementById(r).checked) {
        queue_mode_checked = document.getElementById(r).value;
      }
    }

    if (queue_mode_checked === '') {
      queue_mode_checked = document.getElementById('custom-queue-mode').value.split(' ').filter(Boolean)[0];
    }

    var cxn_supplier = ['simple_queue_legacy', 'ordered_by_score_legacy', 'ordered_by_label_legacy', 'ordered_by_label_and_score_legacy', 'hashed_simple_queue_legacy',
  'hashed_ordered_by_label_legacy'];
    var cxn_supplier_checked = '';
    for (r of cxn_supplier) {
      if  (document.getElementById(r).checked) {
        cxn_supplier_checked = document.getElementById(r).value;
      }
    }

    if (cxn_supplier_checked === '') {
      cxn_supplier_checked = document.getElementById('custom_cxn_supplier_legacy').value.split(' ').filter(Boolean)[0];
    }


    var legacy_search_configuration  = {constructionInventoryProcessorMode: cip_mode_checked,
                                           nodeExpansionMode: node_expansion_mode_checked,
                                           priorityMode: priority_mode_checked,
                                           queueMode: queue_mode_checked,
                                           cxnSupplierMode: cxn_supplier_checked};

    return legacy_search_configuration
  }
}
")


;; Conditional visibility of form elements ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-js 'toggleInnerTextAndVisiblity "
function toggle_inner_text_and_visibility(labelid, text1, text2, valueid) {
       toggle_inner_text(labelid,text1,text2);
       toggle_visibility(valueid);
    }
")

(define-js 'toggleVisibility "
function toggle_visibility(id) {
       var e = document.getElementById(id);
       if(e.style.display == 'none')
          e.style.display = 'block';
       else
          e.style.display = 'none';
    }
")

(define-js 'toggleVisibilityCheckbox "
function toggleVisibilityCheckbox(checkboxid,elementid) {
  // Get the checkbox
  var checkBox = document.getElementById(checkboxid);
  var e = document.getElementById(elementid);

  // If the checkbox is checked, display the output text
  if (checkBox.checked == true){
    e.style.display = 'block';
  } else {
    e.style.display = 'none';
  }
}
")

(define-js 'toggleInnerText "
function toggle_inner_text(id,text1,text2) {
       var e = document.getElementById(id);
       if(e.innerText == text1)
          e.innerText = text2;
       else
           e.innerText = text1;
    }
")
