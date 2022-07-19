/**
 * jsPsych plugin for showing grid for CoAct study
 *
 * Martin Zettersten
 *
 * documentation: docs.jspsych.org
 *
 */

jsPsych.plugins['coact-grid'] = (function() {

  var plugin = {};

  jsPsych.pluginAPI.registerPreload('coact-grid', 'images', 'image');

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
      image_stimuli_names: {
        type:jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Image Stimuli Names',
        default: undefined,
        array: true,
        description: 'Names of the displayed image.'
      },
      image_audio_names: {
        type:jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Image Audio Names',
        default: undefined,
        array: true,
        description: 'Corresponding audio files for the displayed image.'
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
      first_choice_location: {
        type:jsPsych.plugins.parameterType.STRING,
        pretty_name: 'First Choice Location',
        default: "left",
        description: "Parameter specifying the location of the first image selected"
      }
    }
  }

  plugin.trial = function(display_element, trial) {
    
    // variable to keep track of timing info and responses
    // start timing
    var start_time = performance.now();
    // store response
    var response = {
      rt: null,
      choices: [],
      chosen_images: [],
      chosen_items: [],
      chosen_audio: []
    };

    var number_choices = 0;



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



    image1.click(function() {
      square1.attr({
        fill: "#5ec37f"
      });
      image1.unclick();
      inputEvent(0)
    });
    image2.click(function() {
      square2.attr({
        fill: "#5ec37f"
      });
      image2.unclick();
      inputEvent(1)
    });
    image3.click(function() {
      square3.attr({
        fill: "#5ec37f"
      });
      image3.unclick();
      inputEvent(2)
    });
    image4.click(function() {
      image4.unclick();
      square4.attr({
        fill: "#5ec37f"
      });
      inputEvent(3)
    });
    image5.click(function() {
      image5.unclick();
      square5.attr({
        fill: "#5ec37f"
      });
      inputEvent(4)
    });
    image6.click(function() {
      image6.unclick();
      square6.attr({
        fill: "#5ec37f"
      });
      inputEvent(5)
    });
    image7.click(function() {
      image7.unclick();
      square7.attr({
        fill: "#5ec37f"
      });
      inputEvent(6)
    });
    image8.click(function() {
      image8.unclick();
      square8.attr({
        fill: "#5ec37f"
      });
      inputEvent(7)
    });

    function inputEvent(imChoice) {
      number_choices = number_choices + 1;
      // measure rt
      var end_time = performance.now();
      var rt = end_time - start_time;
      response.rt = rt;
      response.choices.push(imChoice);
      response.chosen_images.push(trial.images[imChoice]);
      response.chosen_items.push(trial.image_stimuli_names[imChoice]);
      response.chosen_audio.push(trial.image_audio_names[imChoice]);

      if (number_choices > 1) {
        image1.unclick();
        image2.unclick();
        image3.unclick();
        image4.unclick();
        image5.unclick();
        image6.unclick();
        image7.unclick();
        image8.unclick();

        centerChoices(response.chosen_images);
      }


      
    }

    function centerChoices(choice_images) {
      image1.animate({
          opacity: "0.1"
        },300);
      image2.animate({
          opacity: "0.1"
        },300);
      image3.animate({
          opacity: "0.1"
        },300);
      image4.animate({
          opacity: "0.1"
        },300);
      image5.animate({
          opacity: "0.1"
        },300);
      image6.animate({
          opacity: "0.1"
        },300);
      image7.animate({
          opacity: "0.1"
        },300);
      image8.animate({
          opacity: "0.1"
        },300);
      square1.animate({
          opacity: "0.1"
        },300);
      square2.animate({
          opacity: "0.1"
        },300);
      square3.animate({
          opacity: "0.1"
        },300);
      square4.animate({
          opacity: "0.1"
        },300);
      square5.animate({
          opacity: "0.1"
        },300);
      square6.animate({
          opacity: "0.1"
        },300);
      square7.animate({
          opacity: "0.1"
        },300);
      square8.animate({
          opacity: "0.1"
        },300);

      if (trial.first_choice_location == "left") {
        var center_image_1 = s.image(choice_images[0], center_image_locations[0][0], center_image_locations[0][1], trial.image_size[0]+25,trial.image_size[1]+25);
        var center_image_2 = s.image(choice_images[1], center_image_locations[1][0], center_image_locations[1][1], trial.image_size[0]+25,trial.image_size[1]+25);
      } else {
        var center_image_1 = s.image(choice_images[1], center_image_locations[0][0], center_image_locations[0][1], trial.image_size[0]+25,trial.image_size[1]+25);
        var center_image_2 = s.image(choice_images[0], center_image_locations[1][0], center_image_locations[1][1], trial.image_size[0]+25,trial.image_size[1]+25);
      }

      
      center_image_1.animate({
          opacity: "1"
        },300);
      center_image_2.animate({
          opacity: "1"
        },300);

      setTimeout(function() {endTrial()},300);

    }

    function endTrial() {

      display_element.innerHTML = '';
      var trial_data = {
        images: trial.images,
        rt: response.rt,
        choices: response.choices,
        chosen_images: response.chosen_images,
        chosen_items: response.chosen_items,
        chosen_audio_items: response.chosen_audio,
        first_choice_location: trial.first_choice_location
      };

      jsPsych.finishTrial(trial_data);
    }
  };

  return plugin;
})();
