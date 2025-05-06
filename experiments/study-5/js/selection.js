// selection.js - Handles the selection trials phase

// Function to create the selection trials
async function createSelectionTrials(condition_id, jsPsych) {
  try {
    // Load the stimuli
    const response = await fetch("json/stimuli.json");
    const stimuli = await response.json();

    // Separate attention checks from regular stimuli
    const attentionChecks = stimuli.filter(
      (stimulus) => stimulus.id === "attention"
    );
    const regularStimuli = stimuli.filter(
      (stimulus) => stimulus.id !== "attention"
    );

    // Group regular stimuli by ID (scenario type)
    const scenarioGroups = {};
    regularStimuli.forEach((stimulus) => {
      if (!scenarioGroups[stimulus.id]) {
        scenarioGroups[stimulus.id] = [];
      }
      scenarioGroups[stimulus.id].push(stimulus);
    });

    // Assign relationship conditions to scenarios based on condition id
    // We have 6 partner types and 6 scenarios
    const conditionAssignment = await assignConditions(condition_id);

    // Create trials
    const trials = [];

    // Instructions for the selection phase
    const selectionInstructions = {
      type: jsPsychHtmlButtonResponse,
      stimulus: `
        <h2>Social Interaction Phase</h2>
        <div class="align-left">
          <p>Congrats, you have passed the comprehension check!</p>
          <p>You will now interact with 6 different partners from this society.</p>
          <p>Remember, you will interact with each partner twice.</p>
          <p>For each interaction, you will choose an action, and then see what your partner chose.</p>
          <p>If you and your partner choose <em>different actions</em>, the interaction will go smoothly.</p>
          <p>If you and your partner choose the <em>same action</em>, the interaction will be awkward.</p>
          <p>Your goal is to have as many smooth interactions as possible.</p>
        </div>
      `,
      choices: ["Begin"],
    };
    trials.push(selectionInstructions);

    // Create regular trials
    const allTrials = [];

    for (const [scenarioId, condition] of Object.entries(conditionAssignment)) {
      const scenarioTrials = scenarioGroups[scenarioId];

      // Sort by time (first, second)
      scenarioTrials.sort((a, b) => (a.time === "first" ? -1 : 1));

      // Get the partner name from the first trial (same for both)
      const partnerName = scenarioTrials[0].partner_name;

      // Get relationship condition (higher, equal, lower)
      const relationship = condition.relationship;

      // Create an object representing this partner interaction
      allTrials.push({
        scenarioTrials: scenarioTrials,
        partnerName: partnerName,
        relationship: relationship,
        condition: condition,
        isAttentionCheck: false,
      });
    }

    // Add attention checks as equal status partners
    attentionChecks.sort((a, b) => (a.time === "first" ? -1 : 1));
    allTrials.push({
      scenarioTrials: attentionChecks,
      partnerName: attentionChecks[0].partner_name,
      relationship: "equal", // Always equal status for attention checks
      condition: { relationship: "equal", first_action: "give" },
      isAttentionCheck: true,
    });
    console.log(allTrials);
    // Randomize the order of all trials
    const shuffledTrials = jsPsych.randomization.shuffle(allTrials);

    // Create actual trial sequence
    for (const trialGroup of shuffledTrials) {
      // Add relationship information before first trial
      const relationshipInfo = {
        type: jsPsychHtmlButtonResponse,
        stimulus: `
          <h3>New Partner: ${trialGroup.partnerName}</h3>
          <p>This partner has <strong>${getRelationshipText(
            trialGroup.relationship
          )}</strong> power/status/influence than you.</p>
        `,
        choices: ["Continue"],
      };
      trials.push(relationshipInfo);

      // Create the two interaction trials (first and second)
      for (let i = 0; i < trialGroup.scenarioTrials.length; i++) {
        const stimulus = trialGroup.scenarioTrials[i];
        const isFirstTrial = stimulus.time === "first";

        // Selection trial
        const selectionTrial = {
          type: jsPsychHtmlButtonResponse,
          stimulus: `
            <div class="align-left">
              <h3>Interaction with ${stimulus.partner_name}</h3>
              <p>${stimulus.scenario}</p>
              <p>What will you choose to do?</p>
            </div>
          `,
          choices: [stimulus.options.receive, stimulus.options.give],
          data: {
            task: "selection",
            scenario_id: stimulus.id,
            partner_name: stimulus.partner_name,
            time: stimulus.time,
            relationship: trialGroup.relationship,
            type: "choice",
            is_attention_check: trialGroup.isAttentionCheck,
          },
          on_finish: function (data) {
            // Record participant's choice - 0 is "receive", 1 is "give"
            data.participant_choice = data.response === 0 ? "receive" : "give";

            // Check if attention check was passed
            if (trialGroup.isAttentionCheck) {
              // First attention check should select "give" (option 1)
              if (isFirstTrial) {
                data.passed_attention = data.response === 1;
              }
              // Second attention check should select "receive" (option 0)
              else {
                data.passed_attention = data.response === 0;
              }
            }

            // Determine partner's choice based on relationship and time for non-attention check trials
            if (!trialGroup.isAttentionCheck) {
              data.partner_choice = getPartnerChoice(
                trialGroup.relationship,
                data.participant_choice,
                isFirstTrial,
                trialGroup.condition.first_action
              );

              // Determine if interaction was smooth (different choices) or awkward (same choices)
              data.smooth_interaction =
                data.participant_choice !== data.partner_choice;

              // Record points
              data.points = data.smooth_interaction ? 1 : 0;
            } else {
              // For attention checks, partner choice is always opposite to the correct choice
              // This ensures a smooth interaction if the attention check is passed
              if (isFirstTrial) {
                data.partner_choice = "receive"; // Opposite of correct "give"
              } else {
                data.partner_choice = "give"; // Opposite of correct "receive"
              }

              data.smooth_interaction = data.passed_attention;
              data.points = data.passed_attention ? 1 : 0;
            }

            // Save information about attention checks to jsPsych's data for later analysis
            if (trialGroup.isAttentionCheck) {
              jsPsych.data.addProperties({
                attention_check_passed: jsPsych.data
                  .get()
                  .filter({ is_attention_check: true })
                  .select("passed_attention")
                  .values.every(Boolean),
              });
            }
          },
        };
        trials.push(selectionTrial);

        // Feedback trial
        const feedbackTrial = {
          type: jsPsychHtmlButtonResponse,
          stimulus: function () {
            const lastTrialData = jsPsych.data.getLastTrialData().values()[0];
            const participantChoice = lastTrialData.participant_choice;
            const partnerChoice = lastTrialData.partner_choice;
            const isSmooth = lastTrialData.smooth_interaction;

            let feedbackText = "";
            if (
              trialGroup.isAttentionCheck &&
              !lastTrialData.passed_attention
            ) {
              feedbackText = `<p class="red-bold">This was an attention check. You were asked to select "${
                isFirstTrial ? stimulus.options.give : stimulus.options.receive
              }".</p>`;
            }

            return `
              <div class="align-left">
                <h3>Interaction Result</h3>
                <p>You chose: <strong>${
                  participantChoice === "give"
                    ? stimulus.options.give
                    : stimulus.options.receive
                }</strong></p>
                <p>${stimulus.partner_name} chose: <strong>${
              partnerChoice === "give"
                ? stimulus.options.give
                : stimulus.options.receive
            }</strong></p>
                <hr>
                ${feedbackText}
                ${
                  isSmooth
                    ? `<p class="blue-bold">The interaction went smoothly! You earned 1 point.</p>`
                    : `<p class="red-bold">The interaction was awkward! You earned 0 points.</p>`
                }
              </div>
            `;
          },
          choices: ["Continue"],
          data: {
            task: "selection",
            type: "feedback",
            is_attention_check: trialGroup.isAttentionCheck,
          },
        };
        trials.push(feedbackTrial);
      }
    }

    // Add a trial at the end to save attention check status
    const attentionSummary = {
      type: jsPsychCallFunction,
      func: function () {
        const attentionTrials = jsPsych.data
          .get()
          .filter({ is_attention_check: true, type: "choice" });
        const totalAttentionChecks = attentionTrials.count();
        const passedAttentionChecks = attentionTrials
          .filter({ passed_attention: true })
          .count();

        // Add summary to data
        jsPsych.data.addProperties({
          total_attention_checks: totalAttentionChecks,
          passed_attention_checks: passedAttentionChecks,
          all_attention_checks_passed:
            totalAttentionChecks === passedAttentionChecks,
        });
      },
    };
    trials.push(attentionSummary);

    return trials;
  } catch (error) {
    console.error("Error creating selection trials:", error);
    throw error;
  }
}

function getPartnerChoice(
  relationship,
  participantChoice,
  isFirstTrial,
  firstAction
) {
  // Higher and lower status partners have consistent patterns
  if (relationship === "higher" || relationship === "lower") {
    // higher status partners always choose the same action — what they chose on the first trial
    return firstAction;
  } else if (relationship === "equal") {
    // Equal status partners alternate based on first interaction
    if (isFirstTrial) {
      // In first interaction, they use the assigned first action
      return firstAction;
    } else {
      // In second interaction, they take the opposite of their first action
      return firstAction === "give" ? "receive" : "give";
    }
  }

  // Default fallback (should not reach here)
  return participantChoice === "give" ? "receive" : "give";
}

async function assignConditions(condition_id) {
  // If counterbalance is not provided, use a default assignment
  if (!condition_id) {
    condition_id = 0;
  }
  try {
    const response = await fetch("json/counterbalancing.json");
    const text = await response.text();
    const conditions = JSON.parse(text);
    const assignment = conditions[condition_id];
    return assignment;
  }
  catch (error) {
    console.error("Error loading conditions:", error);
    throw error;
  }
}

function getRelationshipText(relationship) {
  switch (relationship) {
    case "higher":
      return "MORE";
    case "equal":
      return "the SAME AMOUNT OF";
    case "lower":
      return "LESS";
    default:
      return "";
  }
}
