function instructions(condition_number) {
    return {
        type: jsPsychInstructions,
        pages: [
            instructionsPage1(condition_number),
            instructionsPage2(),
            instructionsPage3()
        ],
        show_clickable_nav: true,
        show_page_number: true
    }
}

function instructionsPage1(condition_number) {
    return `
    <h2>Instructions</h2>
    <p>
    In this study we are interested in how we can make inferences about other people's behavior when we know about their social relationships.
    </p>
    <p>
    You will read about <strong>${fetchTrialParams(condition_number).length}</strong> scenarios, each one describing a <strong>social relationship</strong> and a <strong>recurrent social interaction</strong> between two people. You will be asked to answer questions about each scenario.
    </p>
    <p>
    We are interested in how people use just brief observation to guess how social relationships guide people's expectations in social interactions.
    </p>
    `
}

function instructionsPage2() {
    return `
    <h2>Instructions</h2>
    <p>
    For each scenario, we will first ask you about what you think happened the <strong>first</strong> time the social interaction occurred.
    </p>
    <p>
    Then, we will tell you what <strong>actually</strong> happened the first time the social interaction occurred.
    </p>
    <p>
    Then, we will ask you about what you think happened the <strong>second</strong> time the social interaction occurred, given what you now know about what happened the first time it occurred.
    </p>
 `
}

function instructionsPage3() {
    return `
    <h2>Instructions</h2>
    <p>
    You will receive $${params.basePay} if you successfully complete this study.
    </p>
    <p>Please make sure to read each scenario and all the responses carefully! ☺️</p>
    <p style="color: red;">
    ⚠️ Press 'next' to begin the study. ⚠️
    </p>
    `
}
