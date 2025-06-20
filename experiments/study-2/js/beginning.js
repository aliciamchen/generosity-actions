function beginning(jsPsych) {
    var beginning = {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: 'First scenario',
        choices: "NO_KEYS",
        trial_duration: 2000,
        on_start: function() {
            // set progress bar to 0 at the start of experiment
            jsPsych.setProgressBar(0);
        }
    };

    return beginning;
}
