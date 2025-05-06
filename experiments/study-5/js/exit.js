// exit survey and debrief
async function createExit(jsPsych) {
  try {
    // Exit survey
    const response = await fetch("html/exit-survey.html");
    const text = await response.text();
    const exit = [];
    const exit_survey = {
      type: jsPsychSurveyHtmlForm,
      preamble: `
        <p>You have reached the end of the experiment!</p>
        <p><strong>To collect your pay, please complete the following questions. Your answer to these questions will not affect your pay, so please answer honestly.</strong></p>
        `,
      html: text,
      button_label: ["Continue, save data, and collect pay!"],
      data: { task: "exit-survey", type: "response" },
    };
    exit.push(exit_survey);


    const save_data = {
      type: jsPsychPipe,
      action: "save",
      experiment_id: "MFgivQ7NLbx7",
      filename: `${subject_id}.json`, // TODO: save date and time??
      data_string: () => jsPsych.data.get().json(),
    };

    if (!local_testing) {
      exit.push(save_data);
    }

    // Debrief
    const debrief = {
      type: jsPsychHtmlKeyboardResponse,
      stimulus: `<p>Thanks for participating in the experiment!</p>
      <p>Note that in this experiment, you were interacting with simulated agents, rather than real people.</p>
                    <p><a href="PROLIFIC LINK">Click here to return to Prolific and complete the study</a>.</p>
                    <p>It is now safe to close the window. Your pay will be delivered within a few days.</p>
                    `,
      choices: "NO_KEYS",
    };
    exit.push(debrief);

    return exit;
  } catch (error) {
    console.error("Error creating exit survey:", error);
    throw error;
  }
}
