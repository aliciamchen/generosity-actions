function intro(params) {
    return {
        timeline: [
            welcome(),
            consent(params)
        ]
    }
}

function welcome() {
    return {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: 'Welcome to the experiment. Press any key to begin.',
        trial_duration: 10000
    }
}

function consent(params) {
    return {
        type: jsPsychHtmlButtonResponse,
        stimulus: sprintf($('#consent').html(),
            params.completionMinutes,
            params.completionMinutes,
            params.basePay),
        choices: ['I consent']
    }
}