/**
 * jspsych-html-button-response-catact
 * Josh de Leeuw
 *
 * plugin for displaying a stimulus and getting a button response
 *
 * documentation: docs.jspsych.org
 *
 **/

 jsPsych.plugins["html-button-response-catact"] = (function() {

  var plugin = {};

  plugin.info = {
    name: 'html-button-response',
    description: '',
    parameters: {
      stimulus: {
        type: jsPsych.plugins.parameterType.HTML_STRING,
        pretty_name: 'Stimulus',
        default: undefined,
        description: 'The HTML string to be displayed'
      },
      choices: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Choices',
        default: undefined,
        array: true,
        description: 'The labels for the buttons.'
      },
      images: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Images',
        default: undefined,
        array: true,
        description: 'The images used in the buttons.'
      },
      button_html: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Button HTML',
        default: '<button class="jspsych-btn">%choice%</button>',
        array: true,
        description: 'The html of the button. Can create own style.'
      },
      prompt: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Prompt',
        default: null,
        description: 'Any content here will be displayed under the button.'
      },
      stimulus_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Stimulus duration',
        default: null,
        description: 'How long to hide the stimulus.'
      },
      trial_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Trial duration',
        default: null,
        description: 'How long to show the trial.'
      },
      margin_vertical: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Margin vertical',
        default: '0px',
        description: 'The vertical margin of the button.'
      },
      margin_horizontal: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Margin horizontal',
        default: '8px',
        description: 'The horizontal margin of the button.'
      },
      button_label: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Button label',
        default:  'Submit',
        description: 'Label of the submit button.'
      },
      response_ends_trial: {
        type: jsPsych.plugins.parameterType.BOOL,
        pretty_name: 'Response ends trial',
        default: true,
        description: 'If true, then trial will end when user responds.'
      },
    }
  }

  plugin.trial = function(display_element, trial) {

    // display stimulus
    var html = '<div id="jspsych-html-button-response-stimulus">'+trial.stimulus+'</div>';

    //display buttons
    var buttons = [];
    if (Array.isArray(trial.button_html)) {
      if (trial.button_html.length == trial.choices.length) {
        buttons = trial.button_html;
      } else {
        console.error('Error in html-button-response plugin. The length of the button_html array does not equal the length of the choices array');
      }
    } else {
      for (var i = 0; i < trial.choices.length; i++) {
        buttons.push(trial.button_html);
      }
    }
    html += '<div id="jspsych-html-button-response-btngroup">';
    for (var i = 0; i < trial.choices.length; i++) {
      var str = buttons[i].replace(/%choice%/g, trial.choices[i]);
      html += '<div class="jspsych-html-button-response-button" style="display: inline-block; margin:'+trial.margin_vertical+' '+trial.margin_horizontal+'" id="jspsych-html-button-response-button-' + i +'" data-choice="'+i+'">'+str+'</div>';
    }
    html += '</div>';

    // add submit button
    html += '<input type="submit" id="html-button-response-catact-submit" class="html-button-response-catact jspsych-btn" value="'+trial.button_label+'"></input>';

    //show prompt if there is one
    if (trial.prompt !== null) {
      html += trial.prompt;
    }
    display_element.innerHTML = html;

    // start time
    var start_time = performance.now();

    // add event listeners to buttons
    for (var i = 0; i < trial.choices.length; i++) {
      display_element.querySelector('#jspsych-html-button-response-button-' + i).addEventListener('click', function(e){
        var choice = e.currentTarget.getAttribute('data-choice'); // don't use dataset for jsdom compatibility
        after_response(choice);
      });
    }

    //add event listener to final submission button
    display_element.querySelector('#html-button-response-catact-submit').addEventListener('click', function(e){
      end_trial();

    });


    // store response
    var response = {
      rt: null,
      button: null,
      selection_array: null,
      selection_index: null,
      rt_array: null,
      selection_type_array: null,
      final_choice_array:null
    };

    //temporary arrays to hold selection information
    var selections=[];
    var selection_indices=[];
    var selection_rts=[];
    var selection_types=[];
    var final_choices=[];

    function toggle_response(choice) {

      //add RT
      var cur_time = performance.now();
      cur_rt=cur_time-start_time;
      selection_rts.push(cur_rt);

      //add selection
      var image_choice=trial.images[choice];
      selections.push(image_choice);
      selection_indices.push(choice);

      //console.log(selections);
      //console.log(selection_indices);
      //console.log(selection_rts);

      if (document.getElementById('jspsych-html-button-response-button-'+choice).style.color=="red") {
        document.getElementById('jspsych-html-button-response-button-'+choice).style.color="#333";
        document.getElementById('jspsych-html-button-response-button-'+choice).style.border="5px solid transparent";
        //update data storage
        //update selection array type
        selection_types.push("unselect");
        //remove from final choice array
        var index = final_choices.indexOf(image_choice);
        if (index > -1) {
          final_choices.splice(index, 1);
        }
      } else {
        document.getElementById('jspsych-html-button-response-button-'+choice).style.color="red";
        document.getElementById('jspsych-html-button-response-button-'+choice).style.border="5px solid";
        //update data storage
        //update selection array type
        selection_types.push("select");
        //add to final choice array
        final_choices.push(image_choice);
      }

      //console.log(selection_types);
     //console.log(final_choices);
    }

    // function to handle responses by the subject
    function after_response(choice) {

      // measure rt
      //var end_time = performance.now();
      //var rt = end_time - start_time;
      //response.button = parseInt(choice);
      //response.rt = rt;

      toggle_response(choice);

      // after a valid response, the stimulus will have the CSS class 'responded'
      // which can be used to provide visual feedback that a response was recorded
      display_element.querySelector('#jspsych-html-button-response-stimulus').className += ' responded';


      if (trial.response_ends_trial) {
        end_trial();
      }
    };

    // function to end trial when it is time
    function end_trial() {
      // measure rt
      var end_time = performance.now();
      var rt = end_time - start_time;
      response.rt = rt;

      response.rt_array=selection_rts;
      response.selection_array=selections;
      response.selection_index=selection_indices;
      response.selection_type_array=selection_types;
      response.final_choice_array=final_choices;

      // kill any remaining setTimeout handlers
      jsPsych.pluginAPI.clearAllTimeouts();

      // gather the data to store for the trial
      var trial_data = {
        selection_array: response.selection_array,
        selection_index: response.selection_index,
        rt_array: response.rt_array,
        selection_type_array: response.selection_type_array,
        final_choice_array: response.final_choice_array,
        final_rt: response.rt,
        //stimulus: trial.stimulus,
        //response: response.button
      };

      // clear the display
      display_element.innerHTML = '';

      // move on to the next trial
      jsPsych.finishTrial(trial_data);
    };

    // hide image if timing is set
    if (trial.stimulus_duration !== null) {
      jsPsych.pluginAPI.setTimeout(function() {
        display_element.querySelector('#jspsych-html-button-response-stimulus').style.visibility = 'hidden';
      }, trial.stimulus_duration);
    }

    // end trial if time limit is set
    if (trial.trial_duration !== null) {
      jsPsych.pluginAPI.setTimeout(function() {
        end_trial();
      }, trial.trial_duration);
    }

  };

  return plugin;
})();

