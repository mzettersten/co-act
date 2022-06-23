/**
 * jsPsych plugin for second choice in CoAct study
 *
 * Martin Zettersten
 *
 * documentation: docs.jspsych.org
 *
 */

jsPsych.plugins['coact-grid-choice'] = (function() {

  var plugin = {};

  jsPsych.pluginAPI.registerPreload('coact-grid-choice', 'images', 'image');

  plugin.info = {
    name: 'vsl-animate-occlusion',
    description: '',
    parameters: {
      images: {
        type: jsPsych.plugins.parameterType.IMAGE,
        pretty_name: 'Images',
        default: undefined,
        array: true,
        description: 'Images to display.'
      },
      central_images: {
        type: jsPsych.plugins.parameterType.IMAGE,
        pretty_name: 'Central Images',
        default: undefined,
        array: true,
        description: 'Central choice images'
      },
      canvas_size: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Canvas size',
        array: true,
        default: [1200,850],
        description: 'Array specifying the width and height of the area that the animation will display in.'
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
      [875, 230],
      [875, 430],
      [75, 430],
      [325, 25],
      [575, 25],
      [325, 630],
      [575, 630]
    ];

    var center_image_locations = [
      [325,315], [600,315]
    ];

    var back = s.rect(10, 10, 1100, 830,10,10);
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

    var center_back = s.rect(275, 225, 575, 380,10,10);
    center_back.attr({
      fill: "#FFFFFF",
      stroke: "#000",
      strokeWidth: 4

    });

    var center_square_1 = s.rect(center_image_locations[0][0], center_image_locations[0][1],trial.image_size[0]+25,trial.image_size[1]+25,2,2);
    center_square_1.attr({
      fill: "#FFFFFF",
      stroke: "#000",
      strokeWidth: 2
    });

    var center_square_2 = s.rect(center_image_locations[1][0], center_image_locations[1][1],trial.image_size[0]+25,trial.image_size[1]+25,2,2);
    center_square_2.attr({
      fill: "#FFFFFF",
      stroke: "#000",
      strokeWidth: 2
    });

    var image1 = s.image(trial.images[0], imageLocations[0][0], imageLocations[0][1], trial.image_size[0],trial.image_size[1]);
    var image2 = s.image(trial.images[1], imageLocations[1][0], imageLocations[1][1], trial.image_size[0],trial.image_size[1]);
    var image3 = s.image(trial.images[2], imageLocations[2][0], imageLocations[2][1],trial.image_size[0],trial.image_size[1]);
    var image4 = s.image(trial.images[3], imageLocations[3][0], imageLocations[3][1], trial.image_size[0],trial.image_size[1]);
    var image5 = s.image(trial.images[4], imageLocations[4][0], imageLocations[4][1], trial.image_size[0],trial.image_size[1]);
    var image6 = s.image(trial.images[5], imageLocations[5][0], imageLocations[5][1], trial.image_size[0],trial.image_size[1]);
    var image7 = s.image(trial.images[6], imageLocations[6][0], imageLocations[6][1],trial.image_size[0],trial.image_size[1]);
    var image8 = s.image(trial.images[7], imageLocations[7][0], imageLocations[7][1], trial.image_size[0],trial.image_size[1]);

    image1.attr({
          opacity: "0.1"
        });
      image2.attr({
          opacity: "0.1"
        });
      image3.attr({
          opacity: "0.1"
        });
      image4.attr({
          opacity: "0.1"
        });
      image5.attr({
          opacity: "0.1"
        });
      image6.attr({
          opacity: "0.1"
        });
      image7.attr({
          opacity: "0.1"
        });
      image8.attr({
          opacity: "0.1"
        });
      square1.attr({
          opacity: "0.1"
        });
      square2.attr({
          opacity: "0.1"
        });
      square3.attr({
          opacity: "0.1"
        });
      square4.attr({
          opacity: "0.1"
        });
      square5.attr({
          opacity: "0.1"
        });
      square6.attr({
          opacity: "0.1"
        });
      square7.attr({
          opacity: "0.1"
        });
      square8.attr({
          opacity: "0.1"
        });

      var center_image_1 = s.image(trial.central_images[0], center_image_locations[0][0], center_image_locations[0][1], trial.image_size[0]+25,trial.image_size[1]+25);
      var center_image_2 = s.image(trial.central_images[1], center_image_locations[1][0], center_image_locations[1][1], trial.image_size[0]+25,trial.image_size[1]+25);
     
      center_image_1.click(function() {
        center_image_2.animate({
          opacity: "0.1"
        },300);
        inputEvent(0);
        center_square_1.attr({
        fill: "#5ec37f"
      });
        });

      center_image_2.click(function() {
        center_image_1.animate({
          opacity: "0.1"
        },300);
        inputEvent(1);
        center_square_2.attr({
        fill: "#5ec37f"
      });
        });

    function inputEvent(imChoice) {
    center_image_1.unclick();
        center_image_2.unclick();
        // measure rt
        var end_time = performance.now();
        var rt = end_time - start_time;
        response.rt = rt;
        response.choice=imChoice;
        response.chosen_image=trial.central_images[imChoice];
        endTrial();
    }

    function endTrial() {

      display_element.innerHTML = '';
      var trial_data = {
        images: trial.images,
        rt: response.rt,
        choices: response.choices,
        chosen_image:response.chosen_image,
        central_images: trial.central_images
      };

      jsPsych.finishTrial(trial_data);
    }
  };

  return plugin;
})();
