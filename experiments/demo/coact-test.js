/**
 * jsPsych plugin for test trials in CoAct study
 *
 * Martin Zettersten
 *
 * documentation: docs.jspsych.org
 *
 */

jsPsych.plugins['coact-test'] = (function() {

  var plugin = {};

  jsPsych.pluginAPI.registerPreload('coact-test', 'images', 'image');
  jsPsych.pluginAPI.registerPreload('coact-test', 'test_audio', 'audio');

  plugin.info = {
    name: 'coact-test',
    description: '',
    parameters: {
      images: {
        type: jsPsych.plugins.parameterType.IMAGE,
        pretty_name: 'Images',
        default: undefined,
        array: true,
        description: 'Images to display.'
      },
      test_audio: {
        type: jsPsych.plugins.parameterType.AUDIO,
        pretty_name: 'Test Audio',
        default: undefined,
        array: false,
        description: 'Test audio to be played'
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
      trial_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Trial duration',
        default: null,
        description: 'The maximum duration to wait for a response.'
      },
      trial_ends_after_audio: {
        type: jsPsych.plugins.parameterType.BOOL,
        pretty_name: 'Trial ends after audio',
        default: true,
        description: 'If true, then the trial will end as soon as the audio file finishes playing.'
      },
    }
  }

  plugin.trial = function(display_element, trial) {

    // setup stimulus
    var context = jsPsych.pluginAPI.audioContext();
    var audio;
    // record webaudio context start time
    var startTime;

    // store response
    var response = {
      rt: null,
      choice: null,
      chosen_image: null
    };

    // load audio file
    jsPsych.pluginAPI.getAudioBuffer(trial.test_audio)
      .then(function (buffer) {
        if (context !== null) {
          audio = context.createBufferSource();
          audio.buffer = buffer;
          audio.connect(context.destination);
        } else {
          audio = buffer;
          audio.currentTime = 0;
        }
        setupTrial();
      })
      .catch(function (err) {
        console.error(`Failed to load audio file "${trial.test_audio}". Try checking the file path. We recommend using the preload plugin to load audio files.`)
        console.error(err)
      });

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
      fill: "#008080",
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

    function setupTrial() {
     // set up end event if trial needs it
      if (trial.trial_ends_after_audio) {
        audio.addEventListener('ended', endTrial);
      }

      // start time
      startTime = performance.now();

      // start audio
      if (context !== null) {
        startTime = context.currentTime;
        audio.start(startTime);
      } else {
        audio.play();
      }

      // launch image choices
      image_choices()

    // end trial if time limit is set
      if (trial.trial_duration !== null) {
        jsPsych.pluginAPI.setTimeout(function () {
          endTrial();
        }, trial.trial_duration);
      }
    }

    function image_choices() {
          image1.click(function() {
      square1.attr({
        fill: "#40e0d0"
      });
      inputEvent(0)
    });
    image2.click(function() {
      square2.attr({
        fill: "#40e0d0"
      });
      inputEvent(1)
    });
    image3.click(function() {
      square3.attr({
        fill: "#40e0d0"
      });
      inputEvent(2)
    });
    image4.click(function() {
      square4.attr({
        fill: "#40e0d0"
      });
      inputEvent(3)
    });
    image5.click(function() {
      square5.attr({
        fill: "#40e0d0"
      });
      inputEvent(4)
    });
    image6.click(function() {
      square6.attr({
        fill: "#40e0d0"
      });
      inputEvent(5)
    });
    image7.click(function() {
      square7.attr({
        fill: "#40e0d0"
      });
      inputEvent(6)
    });
    image8.click(function() {
      square8.attr({
        fill: "#40e0d0"
      });
      inputEvent(7)
    });
    }

    function inputEvent(imChoice) {
      // measure rt
      var endTime = performance.now();
      var rt = endTime - startTime;
      response.rt = rt;
      response.choice = imChoice;
      response.chosen_image = trial.images[imChoice];
      console.log(response.choice);
      console.log(trial.images);
      console.log(response.chosen_image);

      image1.unclick();
      image2.unclick();
      image3.unclick();
      image4.unclick();
      image5.unclick();
      image6.unclick();
      image7.unclick();
      image8.unclick();

      setTimeout(function() {endTrial()},1000);
      
    }

    function endTrial() {

      // stop the audio file if it is playing
      // remove end event listeners if they exist
      if (context !== null) {
        audio.stop();
      } else {
        audio.pause();
      }
      audio.removeEventListener('ended', endTrial);

      // gather the data to store for the trial
      var trial_data = {
        rt: response.rt,
        images: trial.images,
        test_audio: trial.test_audio,
        choice: response.choice,
        chosen_image: response.chosen_image
      };

      display_element.innerHTML = '';
      jsPsych.finishTrial(trial_data);
    }
  };

  return plugin;
})();
