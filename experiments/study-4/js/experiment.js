$(function () {
  $("#templates").hide();

  /* set up jsPsych, save data */

  var jsPsych = initJsPsych({
    on_finish: function () {
      if (local_testing) {
        jsPsych.data.get().localSave("json", "testdata.json");
      }
    },
    show_progress_bar: true,
    auto_update_progress_bar: false,
  });

  console.log(nCombinations);

  var subject_id =
    local_testing || jsPsych.data.getURLVariable("PROLIFIC_PID") == undefined
      ? jsPsych.randomization.randomID(12)
      : jsPsych.data.getURLVariable("PROLIFIC_PID");
  var study_id = jsPsych.data.getURLVariable("STUDY_ID");
  var session_id = jsPsych.data.getURLVariable("SESSION_ID");

  jsPsych.data.addProperties({
    subject_id: subject_id,
    study_id: study_id,
    session_id: session_id,
    params: params,
    url: window.location.href,
  });

  const save_data = {
    type: jsPsychPipe,
    action: "save",
    experiment_id: "ZTyycp2zb4zs",
    filename: `${subject_id}.json`,
    data_string: () => jsPsych.data.get().json(),
  };

  /* Experiment */
  async function createExperiment() {
    const condition_number = await jsPsychPipe.getCondition("ZTyycp2zb4zs");
    jsPsych.data.addProperties({
      condition_number: condition_number,
    });
    console.log("Condition number: " + condition_number);
    var timeline = [];

    timeline.push(intro(params));
    timeline.push(instructions(condition_number, params));
    timeline.push(beginning(jsPsych));
    timeline.push(makeTrials(condition_number, jsPsych));
    timeline.push(exitSurvey(jsPsych));
    timeline.push(save_data);

    timeline.push(debrief());

    jsPsych.run(timeline.flat());
  }

  createExperiment();
});
