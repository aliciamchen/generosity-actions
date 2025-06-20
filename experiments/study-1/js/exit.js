function exitSurvey() {
    return {
        type: jsPsychSurveyHtmlForm,
        preamble: `
        <p>You have reached the end of the experiment! To collect your pay, please complete the following questions. </p>
        `,
        html: $('#exitSurvey').html(),
        button_label: ['Continue, save data, and collect pay'],
        data: { type: 'response' }
    }
}

function debrief(){
    return {
        type: jsPsychHtmlKeyboardResponse,
        stimulus: function () {
            return `
                    <p>Thanks for participating in the experiment!</p>
                    <p><a href="https://app.prolific.co/submissions/complete?cc=C1E1PWV8">Click here to return to Prolific and complete the study</a>.</p>
                    <p>Press any key to exit. Your pay will be delivered within a few days.</p>
                    `;

        }
    }
}
