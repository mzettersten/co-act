/**
 * jsPsych plugin for test trials in CoAct study
 *
 * Martin Zettersten
 *
 * documentation: docs.jspsych.org
 *
 */

jsPsych.plugins['coact-test-prompt'] = (function() {

  var plugin = {};

  jsPsych.pluginAPI.registerPreload('coact-test-prompt', 'images', 'image');

  plugin.info = {
    name: 'coact-test-prompts',
    description: '',
    parameters: {
      images: {
        type: jsPsych.plugins.parameterType.IMAGE,
        pretty_name: 'Images',
        default: undefined,
        array: true,
        description: 'Images to display.'
      },
      canvas_size: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Canvas size',
        array: true,
        default: [1200,850],
        description: 'Array specifying the width and height of the area that the animation will display in.'
      },
      back_rect_size: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Back Rectangle Size',
        array: true,
        default: [1100,830],
        description: 'Array specifying the width and height of rectangle background.'
      },
      image_size: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Image size',
        array: true,
        default: [175,175],
        description: 'Array specifying the width and height of the images to show.'
      },
    }
  }

  plugin.trial = function(display_element, trial) {

    
    // variable to keep track of timing info and responses
    // start timing
    var start_time = performance.now();
    // store response
    var response = {
      rt: null,
      choice: null,
      chosen_image: null
    };



    display_element.innerHTML = "<svg id='jspsych-coact-grid-canvas' width=" + trial.canvas_size[0] + " height=" + trial.canvas_size[1] + "></svg>";

    var s = Snap("#jspsych-coact-grid-canvas");

    var imageLocations = [
    [75, 230],
      [870, 230],
      [870, 445],
      [75, 445],
      [325, 45],
      [620, 45],
      [325, 630],
      [620, 630]
    ];
    
    var back = s.rect(10, 10, trial.back_rect_size[0], trial.back_rect_size[1],10,10);
    back.attr({
      fill: "#D3D3D3",
      stroke: "#000",
      strokeWidth: 5

    });
    var square1 = s.rect(imageLocations[0][0], imageLocations[0][1],trial.image_size[0],trial.image_size[1],2,2);
    square1.attr({
      fill: "#FFFFFF",
      stroke: "#000",
      strokeWidth: 2
    });

    var square2 = s.rect(imageLocations[1][0], imageLocations[1][1],trial.image_size[0],trial.image_size[1],2,2);
    square2.attr({
      fill: "#FFFFFF",
      stroke: "#000",
      strokeWidth: 2
    });

    var square3 = s.rect(imageLocations[2][0], imageLocations[2][1],trial.image_size[0],trial.image_size[1],2,2);
    square3.attr({
      fill: "#FFFFFF",
      stroke: "#000",
      strokeWidth: 2
    });

    var square4 = s.rect(imageLocations[3][0], imageLocations[3][1],trial.image_size[0],trial.image_size[1],2,2);
    square4.attr({
      fill: "#FFFFFF",
      stroke: "#000",
      strokeWidth: 2
    });

    var square5 = s.rect(imageLocations[4][0], imageLocations[4][1],trial.image_size[0],trial.image_size[1],2,2);
    square5.attr({
      fill: "#FFFFFF",
      stroke: "#000",
      strokeWidth: 2
    });

    var square6 = s.rect(imageLocations[5][0], imageLocations[5][1],trial.image_size[0],trial.image_size[1],2,2);
    square6.attr({
      fill: "#FFFFFF",
      stroke: "#000",
      strokeWidth: 2
    });

    var square7 = s.rect(imageLocations[6][0], imageLocations[6][1],trial.image_size[0],trial.image_size[1],2,2);
    square7.attr({
      fill: "#FFFFFF",
      stroke: "#000",
      strokeWidth: 2
    });

    var square8 = s.rect(imageLocations[7][0], imageLocations[7][1],trial.image_size[0],trial.image_size[1],2,2);
    square8.attr({
      fill: "#FFFFFF",
      stroke: "#000",
      strokeWidth: 2
    });

    //var center_trigger = s.rect(275, 225, 575, 380,10,10);
    var center_trigger = s.circle(trial.back_rect_size[0]/2+10,trial.back_rect_size[1]/2+10,50)
    center_trigger.attr({
      fill: "#ff7373",
      stroke: "#000",
      strokeWidth: 4

    });

    var image1 = s.image(trial.images[0], imageLocations[0][0], imageLocations[0][1], trial.image_size[0],trial.image_size[1]);
    var image2 = s.image(trial.images[1], imageLocations[1][0], imageLocations[1][1], trial.image_size[0],trial.image_size[1]);
    var image3 = s.image(trial.images[2], imageLocations[2][0], imageLocations[2][1],trial.image_size[0],trial.image_size[1]);
    var image4 = s.image(trial.images[3], imageLocations[3][0], imageLocations[3][1], trial.image_size[0],trial.image_size[1]);
    var image5 = s.image(trial.images[4], imageLocations[4][0], imageLocations[4][1], trial.image_size[0],trial.image_size[1]);
    var image6 = s.image(trial.images[5], imageLocations[5][0], imageLocations[5][1], trial.image_size[0],trial.image_size[1]);
    var image7 = s.image(trial.images[6], imageLocations[6][0], imageLocations[6][1],trial.image_size[0],trial.image_size[1]);
    var image8 = s.image(trial.images[7], imageLocations[7][0], imageLocations[7][1], trial.image_size[0],trial.image_size[1]);

      center_trigger.click(function() {
        center_trigger.animate({
          fill: "#008080"
        },300);

        center_trigger.unclick();
        // measure rt
        var end_time = performance.now();
        var rt = end_time - start_time;
        response.rt = rt;
          endTrial();
        });

    function endTrial() {

      display_element.innerHTML = '';
      var trial_data = {
        images: trial.images,
        rt: response.rt
      };

      jsPsych.finishTrial(trial_data);
    }
  };

  return plugin;
})();
