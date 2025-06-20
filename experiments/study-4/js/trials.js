function makeTrials(condition_number, jsPsych) {
  var regularTrials = {
    timeline: [
      // First question
      {
        type: jsPsychSurveyMultiChoice,
        preamble: function() {
          var html = `<h2>First time</h2>\
          <p><span class="vignette">${jsPsych.timelineVariable("vignette")}</span></p><hr>`
          return html;
        },
        questions: [
          {
            prompt: function() {
              var html = `<div class="container"><div class="text-box"><p>${jsPsych.timelineVariable("first_q")}</p></div></div>\
              `;
              return html;
            },
            name: "first_q",
            options: function() {
              var options = [
                `${jsPsych.timelineVariable("alice")}`,
                `${jsPsych.timelineVariable("bob")}`,
              ];
              return options;
            },
            required: true,
            horizontal: true,
          },
        ],
        data: {
          type: "response",
          stage: "first",
          // correct: jsPsych.timelineVariable("first_meeting") === data.response.first_q,
          story: jsPsych.timelineVariable("story"),
          altruistic_status: jsPsych.timelineVariable("altruistic_status"),
          first_meeting: jsPsych.timelineVariable("first_meeting"),
          first_actual: jsPsych.timelineVariable("first_actual"),
          vignette: jsPsych.timelineVariable("vignette"),
          answer_labels: {
            alice: jsPsych.timelineVariable("alice"),
            bob: jsPsych.timelineVariable("bob"),
          },
        },
        on_finish: function (data) {
          data.response.answer = data.response.first_q == jsPsych.timelineVariable("alice") ? "alice" : "bob"
          data.correct = jsPsych.timelineVariable("first_meeting") == data.response.answer

          var response_status = function() {
            if (data.correct === true) {
              return jsPsych.timelineVariable("altruistic_status")
            } else if (jsPsych.timelineVariable("altruistic_status") === "equal") {
              return jsPsych.timelineVariable("altruistic_status")
            } else {
              const statuses = ["more", "less"]
              const result = statuses.filter(elt => elt !== jsPsych.timelineVariable("altruistic_status"))[0]
              return result
            }
          }

          data.response.status = response_status()
          // console.log(data)
        },
        button_label: "Submit",
      },
      // Submitted response
      {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: "Submitting response...",
        choices: "NO_KEYS",
        trial_duration: function () {
          return jsPsych.randomization.sampleWithoutReplacement(
            [1500, 1750, 2000, 2300],
            1
          )[0];
        },
      },
      // Feedback
      {
        type: jsPsychHtmlButtonResponse,
        stimulus: function () {
          var html = `<h2>First time actual</h2>\
          <p><span class="vignette">${jsPsych.timelineVariable("vignette")}</span></p>\
          <div class="container"><div class="text-box">
          <p><strong>What actually happened the first time: </strong>${jsPsych.timelineVariable("first_actual")}</p></div></div> <br>`
          return html;
        },
        choices: ["Continue"],
      },

      // Second question
      {
        type: jsPsychSurveyMultiChoice,
        preamble: function() {
          var html = `<h2>Second time</h2>\
          <p><span class="vignette">${jsPsych.timelineVariable("vignette")}</span></p>\
          <p>The <strong>first time</strong>, ${jsPsych.timelineVariable("first_actual")}</p>
          <p><strong>Now that you know what happened the first time, what do you think happened the second time?</strong></p>
          <hr>`
          return html;
        },
        questions: [
          {
            prompt: function() {
              var html = `<div class="container"><div class="text-box"><p>${jsPsych.timelineVariable("second_q")}</p></div></div>`;
              return html;
            }
            ,
            name: "second_q",
            options: [
              jsPsych.timelineVariable("alice"),
              jsPsych.timelineVariable("bob"),
            ],
            required: true,
            horizontal: true,
          },
        ],
        data: {
          type: "response",
          stage: "second",
          story: jsPsych.timelineVariable("story"),
          altruistic_status: jsPsych.timelineVariable("altruistic_status"),
          first_meeting: jsPsych.timelineVariable("first_meeting"),
          first_actual: jsPsych.timelineVariable("first_actual"),
          vignette: jsPsych.timelineVariable("vignette"),
          answer_labels: {
            alice: jsPsych.timelineVariable("alice"),
            bob: jsPsych.timelineVariable("bob"),
          },
        },
        button_label: "Submit",
        on_finish: function (data) {
          var curr_progress_bar_value = jsPsych.getProgressBarCompleted();
          jsPsych.setProgressBar(
            curr_progress_bar_value +
              1 / fetchTrialParams(condition_number).length
          );
          data.strategy = jsPsych.data.get().last(4).values()[0].first_actual === data.response.second_q ? "repeating" : "alternating"
          data.response.answer = data.response.second_q === jsPsych.timelineVariable("alice") ? "alice" : "bob"

          var response_status = function() {
            if (jsPsych.timelineVariable("first_meeting") == data.response.answer) {
              return jsPsych.timelineVariable("altruistic_status")
            } else if (jsPsych.timelineVariable("altruistic_status") === "equal") {
              return jsPsych.timelineVariable("altruistic_status")
            } else {
              const statuses = ["more", "less"]
              const result = statuses.filter(elt => elt !== jsPsych.timelineVariable("altruistic_status"))[0]
              return result
            }
          }

          data.response_status = response_status()
          // console.log(data)
        },
      },

      {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: "Thank you for your responses! Next scenario...",
        choices: "NO_KEYS",
        trial_duration: function () {
          return jsPsych.randomization.sampleWithoutReplacement(
            [1500, 1750, 2000, 2300],
            1
          )[0];
        },
      },
    ],
    timeline_variables: fetchTrialParams(condition_number),
    randomize_order: true,
  };

  var attentionTrial = {
    timeline: [
      // First question
      {
        type: jsPsychSurveyMultiChoice,
        preamble: function() {
          var html = `<h2>First time</h2>\
          <p><span class="vignette">${jsPsych.timelineVariable("vignette")}</span></p><hr>`
          return html;
        },
        questions: [
          {
            prompt: function() {
              var html = `<div class="container"><div class="text-box"><p>${jsPsych.timelineVariable("first_q")}</p></div></div>\
              `;
              return html;
            },
            name: "first_q",
            options: function() {
              var options = [
                `${jsPsych.timelineVariable("alice")}`,
                `${jsPsych.timelineVariable("bob")}`,
              ];
              return options;
            },
            required: true,
            horizontal: true,
          },
        ],
        data: {
          type: "attention",
          stage: "first",
          story: jsPsych.timelineVariable("story"),
          altruistic_status: jsPsych.timelineVariable("altruistic_status"),
          first_meeting: jsPsych.timelineVariable("first_meeting"),
          first_actual: jsPsych.timelineVariable("first_actual"),
          vignette: jsPsych.timelineVariable("vignette"),
          answer_labels: {
            alice: jsPsych.timelineVariable("alice"),
            bob: jsPsych.timelineVariable("bob"),
          },
        },
        on_finish: function (data) {
          data.correct = jsPsych.timelineVariable("first_meeting") === data.response.first_q

          console.log(data)
        },
        button_label: "Submit",
      },
      // Submitted response
      {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: "Submitting response...",
        choices: "NO_KEYS",
        trial_duration: function () {
          return jsPsych.randomization.sampleWithoutReplacement(
            [1500, 1750, 2000, 2300],
            1
          )[0];
        },
      },
      // Feedback
      {
        type: jsPsychHtmlButtonResponse,
        stimulus: function () {
          var html = `<h2>First time actual</h2>\
          <p><span class="vignette">${jsPsych.timelineVariable("vignette")}</span></p>\
          <div class="container"><div class="text-box">
          <p><strong>What actually happened the first time: </strong>${jsPsych.timelineVariable("first_actual")}</p></div></div> <br>`
          return html;
        },
        choices: ["Continue"],
      },

      // Second question
      {
        type: jsPsychSurveyMultiChoice,
        preamble: function() {
          var html = `<h2>Second time</h2>\
          <p><span class="vignette">${jsPsych.timelineVariable("vignette")}</span></p>\
          <p><strong>Now that you know what happened the first time, what do you think happened the second time?</strong></p>
          <hr>`
          return html;
        },
        questions: [
          {
            prompt: function() {
              var html = `<div class="container"><div class="text-box"><p>${jsPsych.timelineVariable("second_q")}</p></div></div>`;
              return html;
            }
            ,
            name: "second_q",
            options: [
              jsPsych.timelineVariable("alice"),
              jsPsych.timelineVariable("bob"),
            ],
            required: true,
            horizontal: true,
          },
        ],
        data: {
          type: "response",
          stage: "second",
          story: jsPsych.timelineVariable("story"),
          altruistic_status: jsPsych.timelineVariable("altruistic_status"),
          first_meeting: jsPsych.timelineVariable("first_meeting"),
          first_actual: jsPsych.timelineVariable("first_actual"),
          vignette: jsPsych.timelineVariable("vignette"),
          answer_labels: {
            alice: jsPsych.timelineVariable("alice"),
            bob: jsPsych.timelineVariable("bob"),
          },
        },
        button_label: "Submit",
        on_finish: function (data) {
          var curr_progress_bar_value = jsPsych.getProgressBarCompleted();
          jsPsych.setProgressBar(
            curr_progress_bar_value +
              1 / fetchTrialParams(condition_number).length
          );
          data.passAttention = (data.response.second_q == jsPsych.timelineVariable("bob")) && (jsPsych.data.get().last(4).values()[0].response.first_q == jsPsych.timelineVariable("alice"))
          console.log(data.passAttention)
        },
      },

      {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: "Thank you for your responses!",
        choices: "NO_KEYS",
        trial_duration: function () {
          return jsPsych.randomization.sampleWithoutReplacement(
            [1500, 1750, 2000, 2300],
            1
          )[0];
        },
      },
    ],
    timeline_variables: fetchAttentionTrialParams(),
  };

  return [regularTrials, attentionTrial];
  // return [regularTrials]
}
