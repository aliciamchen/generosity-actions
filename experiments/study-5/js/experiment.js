// Main experiment file

const local_testing = false;
let failCount = 0;
let basePay = 4;
let possiblePoints = 14;
let pointMultiplier = 0.214;


const jsPsych = initJsPsych({
  on_finish: function () {
    if (local_testing) {
      jsPsych.data.displayData();
      jsPsych.data.get().localSave("json", "testdata.json");
    }
  },
  show_progress_bar: true,
});

var subject_id =
  local_testing || jsPsych.data.getURLVariable("PROLIFIC_PID") == undefined
    ? jsPsych.randomization.randomID(12)
    : jsPsych.data.getURLVariable("PROLIFIC_PID");
var study_id = jsPsych.data.getURLVariable("STUDY_ID");
var session_id = jsPsych.data.getURLVariable("SESSION_ID");

var timeline = [];

makeTrials(jsPsych).then((trials) => {
  timeline.push(trials);
  jsPsych.run(timeline.flat());
});
