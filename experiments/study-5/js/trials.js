// put all the trials together
async function makeTrials(jsPsych) {
  try {
    const condition = await jsPsychPipe.getCondition("MFgivQ7NLbx7");
    const item_id = Math.floor(condition / 2) % 3;
    const counterbalance = condition % 2 === 0 ? 'a' : 'b';

    console.log(condition, item_id, counterbalance);

    jsPsych.data.addProperties({
      subject_id: subject_id,
      study_id: study_id,
      session_id: session_id,
      condition: condition,
      item_id: item_id,
      counterbalance: counterbalance,
      url: window.location.href,
    });

    const timeline = [];

    const preload = {
      type: jsPsychPreload,
      auto_preload: true,
    };
    timeline.push(preload);

    // consent
    const consent = await createConsent();
    timeline.push(consent);

    // instructions + comprehension check loop
    const comprehensionLoop = await createComprehensionLoop(item_id, jsPsych);
    timeline.push(comprehensionLoop);

    // // observation phase
    // const videoTrials = await createVideoTrials(item_id, counterbalance, jsPsych);
    // timeline.push(videoTrials);

    // // selection phase
    // const selectionTrials = await createSelectionTrials(item_id, counterbalance, jsPsych);
    // timeline.push(selectionTrials);

    // exit survey
    const exit = await createExit(item_id, jsPsych);
    timeline.push(exit);

    return timeline.flat();
  } catch (error) {
    console.error("Error loading trials:", error);
    throw error;
  }
}
