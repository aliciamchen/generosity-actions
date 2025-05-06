// put all the trials together
async function makeTrials(jsPsych) {
  try {
    const condition_id = await jsPsychPipe.getCondition("MFgivQ7NLbx7");

    jsPsych.data.addProperties({
      subject_id: subject_id,
      study_id: study_id,
      session_id: session_id,
      condition_id: condition_id,
      url: window.location.href,
    });

    const timeline = [];

    const preload = {
      type: jsPsychPreload,
      auto_preload: true,
    };
    timeline.push(preload);

    // // consent
    // const consent = await createConsent();
    // timeline.push(consent);

    // // instructions + comprehension check loop
    // const comprehensionLoop = await createComprehensionLoop(jsPsych);
    // timeline.push(comprehensionLoop);

    // selection phase
    const selectionTrials = await createSelectionTrials(condition_id, jsPsych);
    timeline.push(selectionTrials);

    // exit survey
    const exit = await createExit(jsPsych);
    timeline.push(exit);

    return timeline.flat();
  } catch (error) {
    console.error("Error loading trials:", error);
    throw error;
  }
}
