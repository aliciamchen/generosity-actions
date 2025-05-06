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
        <h2>Social Interaction</h2>
        <div class="align-left">
          <p>Congrats, you have passed the comprehension check!</p>
          <p>You will now interact with 6 different partners from this society.</p>
          <p>Remember, you will interact with each partner twice.</p>
          <p>For each interaction, you will choose an action, and then see what your partner chose.</p>
          <p>If you and your partner choose <em>different actions</em>, the interaction will go smoothly. ☺️</p>
          <p>If you and your partner choose the <em>same action</em>, the interaction will be awkward. 😫</p>
          <p>Your goal is to have as many smooth interactions as possible. Please press "Begin" to start the study.</p>
        </div>
      `,
      choices: ["Begin"],
    };
    trials.push(selectionInstructions);

    // Array to hold paired interactions (first and second)
    const allPairedInteractions = [];

    // Create paired interactions for each scenario
    for (const [scenarioId, condition] of Object.entries(conditionAssignment)) {
      const scenarioTrials = scenarioGroups[scenarioId];

      // Sort by time (first, second)
      scenarioTrials.sort((a, b) => (a.time === "first" ? -1 : 1));

      // First and second interaction for this scenario
      const firstInteraction = scenarioTrials[0];
      const secondInteraction = scenarioTrials[1];

      // Get the partner name (same for both)
      const partnerName = firstInteraction.partner_name;

      // Get relationship condition (higher, equal, lower)
      const relationship = condition.relationship;

      // Create a paired interaction object
      allPairedInteractions.push({
        scenarioId: scenarioId,
        partnerName: partnerName,
        relationship: relationship,
        condition: condition,
        isAttentionCheck: false,
        first: {
          stimulus: firstInteraction,
        },
        second: {
          stimulus: secondInteraction,
        },
      });
    }

    // Add attention checks as paired interactions
    attentionChecks.sort((a, b) => (a.time === "first" ? -1 : 1));
    allPairedInteractions.push({
      scenarioId: "attention",
      partnerName: attentionChecks[0].partner_name,
      relationship: "equal", // Always equal status for attention checks
      condition: { relationship: "equal", first_action: "give" },
      isAttentionCheck: true,
      first: {
        stimulus: attentionChecks[0],
      },
      second: {
        stimulus: attentionChecks[1],
      },
    });

    // Create a fully randomized sequence of all interactions
    // For each paired interaction, we'll randomly insert trials into the sequence
    // while ensuring first comes before second
    const allInteractionTrials = [];

    // For each paired interaction
    allPairedInteractions.forEach((pairedInteraction) => {
      // Create trial groups for first and second interactions
      const firstTrialGroup = createTrialGroup(
        pairedInteraction,
        true,
        jsPsych
      );
      const secondTrialGroup = createTrialGroup(
        pairedInteraction,
        false,
        jsPsych
      );

      // Randomly place the second interaction somewhere after the first
      if (allInteractionTrials.length === 0) {
        // If this is the first scenario, just place them in order
        allInteractionTrials.push(firstTrialGroup);
        allInteractionTrials.push(secondTrialGroup);
      } else {
        // Place the first interaction randomly among existing trials
        const firstIndex = Math.floor(
          Math.random() * (allInteractionTrials.length + 1)
        );
        allInteractionTrials.splice(firstIndex, 0, firstTrialGroup);

        // Place the second interaction randomly after the first
        const secondIndex =
          Math.floor(
            Math.random() * (allInteractionTrials.length - firstIndex)
          ) +
          firstIndex +
          1;
        allInteractionTrials.splice(secondIndex, 0, secondTrialGroup);
      }
    });

    // Flatten all trials into a single sequence
    for (const trialGroup of allInteractionTrials) {
      trials.push(...trialGroup);
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

// Helper function to find data from the first interaction for a specific scenario and partner
function getFirstInteractionData(jsPsych, scenarioId, partnerName) {
  // Get all data from first interactions that are not attention checks
  const firstInteractionData = jsPsych.data.get().filter({
    scenario_id: scenarioId,
    partner_name: partnerName,
    time: "first",
    type: "choice",
  });

  if (firstInteractionData.count() > 0) {
    return firstInteractionData.values()[0];
  }

  return null;
}

// Helper function to create a group of trials for an interaction (intro + selection + feedback)
function createTrialGroup(pairedInteraction, isFirstTrial, jsPsych) {
  const trialGroup = [];
  const interaction = isFirstTrial
    ? pairedInteraction.first
    : pairedInteraction.second;
  const stimulus = interaction.stimulus;

  // waiting for partner screen with random duration
  const waitingScreen = {
    type: jsPsychHtmlKeyboardResponse,
    stimulus: "Waiting for partner...",
    trial_duration: function () {
      return jsPsych.randomization.sampleWithoutReplacement(
        [500, 1000, 2300, 2500, 3000, 3500, 4000, 4500, 5000],
        1
      )[0];
    },
    choices: "NO_KEYS",
  };

  trialGroup.push(waitingScreen);

  // Intro/reminder screen
  const introScreen = {
    type: jsPsychHtmlButtonResponse,
    stimulus: `
      <h3>${isFirstTrial ? "New Partner" : "Second Interaction with"}: ${
      pairedInteraction.partnerName
    }</h3>
      <p>${pairedInteraction.partnerName} has <strong>${getRelationshipText(
      pairedInteraction.relationship
    )}</strong> power, status, or influence ${
      pairedInteraction.relationship == "equal" ? "as" : "than"
    } you.</p>
    `,
    choices: ["Continue"],
    data: {
      scenario_id: pairedInteraction.scenarioId,
      partner_name: pairedInteraction.partnerName,
      is_attention_check: pairedInteraction.isAttentionCheck,
      time: isFirstTrial ? "first" : "second",
    },
  };
  trialGroup.push(introScreen);

  // Selection trial
  const selectionTrial = {
    type: jsPsychHtmlButtonResponse,
    stimulus: function () {
      let reminderHTML = "";

      // For second interactions, check if we have data from the first interaction
      if (!isFirstTrial) {
        // Get data from the first interaction with this partner for this scenario
        const firstInteractionData = getFirstInteractionData(
          jsPsych,
          pairedInteraction.scenarioId,
          pairedInteraction.partnerName
        );

        if (firstInteractionData) {
          // Create reminder text based on first interaction data
          const participantFirstChoice =
            firstInteractionData.participant_choice === "give"
              ? stimulus.options.give
              : stimulus.options.receive;

          const partnerFirstChoice =
            firstInteractionData.partner_choice === "give"
              ? stimulus.options.give
              : stimulus.options.receive;

          const firstOutcome = firstInteractionData.coordination
            ? "smoothly"
            : "awkwardly";

          reminderHTML = `
            <div class="reminder-box">
              <h4>Reminder of your first interaction:</h4>
              <p>Last time you interacted with ${stimulus.partner_name}, you chose to <strong>${participantFirstChoice}</strong>.</p>
              <p>${stimulus.partner_name} chose to <strong>${partnerFirstChoice}</strong>.</p>
              <p>The interaction went <strong>${firstOutcome}</strong>.</p>
            </div>
          `;
        }
      }

      return `
        <div class="align-left">
          <h3>Interaction with ${stimulus.partner_name}</h3>
          <p><em>${stimulus.partner_name} has <strong>${getRelationshipText(
        pairedInteraction.relationship
      )}</strong> power, status, or influence ${
        pairedInteraction.relationship == "equal" ? "as" : "than"
      } you.</em></p>
          ${reminderHTML}
          <p>${stimulus.scenario}</p>
          <p>What do you choose to do?</p>
        </div>
      `;
    },
    choices: [stimulus.options.receive, stimulus.options.give],
    data: {
      task: "selection",
      scenario_id: stimulus.id,
      partner_name: stimulus.partner_name,
      time: stimulus.time,
      relationship: pairedInteraction.relationship,
      type: "choice",
      is_attention_check: pairedInteraction.isAttentionCheck,
    },
    on_finish: function (data) {
      // Record participant's choice - 0 is "receive", 1 is "give"
      data.participant_choice = data.response === 0 ? "receive" : "give";

      // Check if attention check was passed
      if (pairedInteraction.isAttentionCheck) {
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
      if (!pairedInteraction.isAttentionCheck) {
        data.partner_choice = getPartnerChoice(
          pairedInteraction.relationship,
          data.participant_choice,
          isFirstTrial,
          pairedInteraction.condition.first_action
        );

        // Determine if interaction was smooth (different choices) or awkward (same choices)
        data.coordination = data.participant_choice !== data.partner_choice;

        // Record points
        data.points = data.coordination ? 1 : 0;
        console.log(data);
      } else {
        // For attention checks, partner choice is always opposite to the correct choice
        // This ensures a smooth interaction if the attention check is passed
        if (isFirstTrial) {
          data.partner_choice = "receive"; // Opposite of correct "give"
        } else {
          data.partner_choice = "give"; // Opposite of correct "receive"
        }

        data.coordination = data.passed_attention;
        data.points = data.passed_attention ? 1 : 0;
      }

      // Save information about attention checks to jsPsych's data for later analysis
      if (pairedInteraction.isAttentionCheck) {
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
  trialGroup.push(selectionTrial);

  trialGroup.push(waitingScreen);

  // Feedback trial
  const feedbackTrial = {
    type: jsPsychHtmlButtonResponse,
    stimulus: function () {
      // Get the choice trial data (two trials back, since the last trial is a waiting screen)
      const lastTrialData = jsPsych.data.get().filter({type: 'choice'}).last(1).values()[0];
      const participantChoice = lastTrialData.participant_choice;
      const partnerChoice = lastTrialData.partner_choice;
      const didCoordinate = lastTrialData.coordination;

      let feedbackText = "";
      if (
        pairedInteraction.isAttentionCheck &&
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
            didCoordinate
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
      is_attention_check: pairedInteraction.isAttentionCheck,
      scenario_id: pairedInteraction.scenarioId,
      time: isFirstTrial ? "first" : "second",
    },
  };
  trialGroup.push(feedbackTrial);

  return trialGroup;
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
  } catch (error) {
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
