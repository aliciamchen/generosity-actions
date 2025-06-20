function makeTrials(condition_number, jsPsych) {
  var regularTrials = {
    timeline: [
      {
        type: jsPsychSurveyLikert,
        preamble: jsPsych.timelineVariable("vignette"),
        questions: [
          {
            prompt: jsPsych.timelineVariable("repeating"),
            name: "repeating",
            labels: params.likertLabels,
          },
          {
            prompt: jsPsych.timelineVariable("alternating"),
            name: "alternating",
            labels: params.likertLabels,
          },
          {
            prompt: jsPsych.timelineVariable("none"),
            name: "none",
            labels: params.likertLabels,
          }
        ],
        data: {
          type: "response",
          story: jsPsych.timelineVariable("story"),
          relationship: jsPsych.timelineVariable("relationship"),
          vignette: jsPsych.timelineVariable("vignette"),
          answer_labels: {
            repeating: jsPsych.timelineVariable("repeating"),
            alternating: jsPsych.timelineVariable("alternating"),
            none: jsPsych.timelineVariable("none")
          },
        },
        randomize_question_order: false,
        button_label: "Submit",
        on_finish: function (data) {
          var curr_progress_bar_value = jsPsych.getProgressBarCompleted();
          jsPsych.setProgressBar(
            curr_progress_bar_value +
              1 / fetchTrialParams(condition_number).length
          );

          if (data.relationship == "attention") {
            if (
              data.response.repeating == 6 &&
              data.response.alternating == 0 &&
              data.response.none == 6
            ) {
              data.passAttentionCheck = true;
            } else {
              data.passAttentionCheck = false;
            }
          }
        },
      },
      {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: "Next scenario",
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

  var attentionParams = fetchAttentionTrialParams();

  var attentionTrial = {
    type: jsPsychSurveyLikert,
    preamble: attentionParams.vignette,
    questions: [
      {
        prompt: attentionParams.repeating,
        name: "repeating",
        labels: params.likertLabels,
      },
      {
        prompt: attentionParams.alternating,
        name: "alternating",
        labels: params.likertLabels,
      },
      {
        prompt: attentionParams.none,
        name: "none",
        labels: params.likertLabels,
      }
    ],
    data: {
      type: "response",
      story: attentionParams.story,
      relationship: attentionParams.relationship,
      vignette: attentionParams.vignette,
      answer_labels: {
        repeating: attentionParams.repeating,
        alternating: attentionParams.alternating,
        none: attentionParams.none
      },
    },
    randomize_question_order: false,
    button_label: "Submit",
    on_finish: function (data) {
        if (
          data.response.repeating == 6 &&
          data.response.alternating == 0 &&
          data.response.none == 6
        ) {
          data.passAttentionCheck = true;
        } else {
          data.passAttentionCheck = false;
        }
    },
  };

  return [regularTrials, attentionTrial];
}
