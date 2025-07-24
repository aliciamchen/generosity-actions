# Codebook

This folder contains data and demographic info for each experiment.

## Data files

The below entries correspond to the non-processed wide-form data (tidy data with labels modified and organized is also provided under the `{}_tidy_data.csv` labels)

- `study-1_data.csv` collected Jan 30, 2023
    - `subject_id`: anonymized participant ID
    -  `story`: label for each of the 18 scenarios
    - `relationship`: what relationship are the two characters in? (either `no_info`, `asymmetric`, or `symmetric`)
    - Three participant responses for each trial (all 0-indexed, on a 7 point Likert scale)
        - `repeating`: likelihood of character repeating the generous act
        - `alternating`: likelihood of other character performing the generous act
        - `none`: likelihood of no future interaction
    - `understood`: did the participant indicate that they understood the instructions?
    - `pass_attention`: did the participant pass the attention check?
- `study-2_data.csv` collected Apr 12, 2023
    - `subject_id`: anonymized participant ID
    -  `story`: label for each of the 18 scenarios
    - `relationship`: what relationship are the two characters in? (corresponds to relative status of generous character: `more`, `equal`, or `less`)
    - Three participant responses for each trial (all 0-indexed, on a 7 point Likert scale)
        - `repeating`: likelihood of character repeating the generous act
        - `alternating`: likelihood of other character performing the generous act
        - `none`: likelihood of no future interaction
    - `understood`: did the participant indicate that they understood the instructions?
    - `pass_attention`: did the participant pass the attention check?
- `study-3_data.csv`: collected April 30 and May 1, 2025
    - `subject_id`: anonymized participant ID
    -  `story`: label for each of the 18 scenarios
    - `relationship`: what relationship are the two characters in? (corresponds to relative status of generous character: `more`, `equal`, or `less`)
    - Three participant responses for each trial (all 0-indexed, on a 7 point Likert scale)
        - `repeating`: likelihood of character repeating the generous act
        - `alternating`: likelihood of other character performing the generous act
        - `none`: likelihood of no future interaction
    - `understood`: did the participant indicate that they understood the instructions?
    - `pass_attention`: did the participant pass the attention check?
- `study-4_data.csv` collected Aug 5, 2023
    - `subject_id`: anonymized participant ID
    -  `story`: label for each of the 18 scenarios
    - `altruistic_status`: what relationship are the two characters in? (corresponds to relative status of first-time experimenter-manipulated generous character: `more`, `equal`, or `less`); this corresponds to 'observed first time' in the manuscript
    - `first_meeting`: out of `alice` and `bob`, who was generous the first time? (labeling each character as `alice` or `bob` is so that we can keep track of counterbalancing between the actual names in the scenario)
    - `stage`: is this the participant's `first` response (i.e. implicit coordination response), or their `second` response (i.e. what they predicted happened the second time the two people interacted)?
    - `response`: who did the participant think was generous (either `alice` or `bob`)
    - `response_status`: did whoever they picked for their response have `more`, `equal`, or `less` power/status/influence?
    - `strategy`: did people expect `repeating` or `alternating` actions?
    - `understood`: did the participant indicate that they understood the instructions?
    - `pass_attention`: did the participant pass the attention check?
- `study-5_data.csv` (collected May 16th 2025) and `study-6_data.csv` (collected May 17th 2025)
    - `subject_id`: anonymized participant ID
    - `trial_num`: corresponds to the order the trials were displayed to participants
    - `scenario_id`: label for each of the 6 scenarios
    - `partner_status`: relative rank of partner
    - `time`: was it the `first` time the two partners interacted or was it the `second` time? 
    - `partner_choice`: whether the partner chose to `give` or `receive`
    - `participant_choice`: whether the participant chose to `give` or `receive`
    - `coordination`: did the two people select the same choices, leading to miscoordination, or did they choose complementary choices, leading to successful coordination? 
    - `passed_attention_checks`: how many attention checks (out of 2) did they pass? 


### Validation experiments (measuring relative benefit/effort in each scenario)

- `validation_benefit_data.csv` collected May 24 and July 11, 2023
    - `subject_id`: anonymized participant ID
    -  `story`: label for each of the 16 scenarios
    - participant responses (0-indexed, on a 7 point Likert scale)
        - `expected_high_benefit`: how much was the (assigned) target of generosity expected to benefit, compared to not interacting
        - `expected_low_benefit`: how much was the (assigned) generous actor expected to benefit, compared to not interacting
    - `understood`: did the participant indicate that they understood the instructions?
    - `pass_attention`: did the participant pass the attention check?

- `validation_effort_data.csv` collected July 12, 2023
    - `subject_id`: anonymized participant ID
    -  `story`: label for each of the 16 scenarios
    - participant responses (0-indexed, on a 7 point Likert scale)
        - `expected_high_effort`: how much was the (assigned) target of generosity expected to put in effort, compared to not interacting
        - `expected_low_effort`: how much was the (assigned) generous actor expected to put in effort, compared to not interacting
    - `understood`: did the participant indicate that they understood the instructions?
    - `pass_attention`: did the participant pass the attention check?

`scenarios_benefit.csv` and `scenarios_effort.csv` contain computed mean benefit/effort values for each scenario, and `scenarios_diff.csv` contain their difference. 