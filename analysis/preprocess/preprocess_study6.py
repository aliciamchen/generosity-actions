#!/usr/bin/env python3
import os
import json
import csv
import glob

# Define the input and output directories
RAW_DIR = "../../data/study-6/raw"
OUTPUT_DIR = "../../data/study-6/non_anonymized"

# Ensure output directory exists
os.makedirs(OUTPUT_DIR, exist_ok=True)

# Define output file paths
data_csv_path = os.path.join(OUTPUT_DIR, "study-6_data.csv")
exit_survey_csv_path = os.path.join(OUTPUT_DIR, "study-6_demographics.csv")
bonus_csv_path = os.path.join(OUTPUT_DIR, "study-6_bonuses.csv")

# Define the headers for each CSV file
data_headers = [
    "subject_id",
    "trial_num",
    "scenario_id",
    "partner_status",
    "time",
    "partner_choice",
    "participant_choice",
    "coordination",
    "passed_attention_checks"
]

exit_survey_headers = [
    "subject_id",
    "gender",
    "age",
    "understood",
    "realPeople",
    "comments",
    "passed_attention_checks",
    "total_points",
    "total_pay"
]

bonus_headers = [
    "subject_id",
    "bonus"
]

# Initialize lists to store data
data_rows = []
exit_survey_rows = []
bonus_rows = []

# Function to process each JSON file
def process_json_file(file_path):
    # Extract subject_id from filename
    subject_id = os.path.basename(file_path).replace('.json', '')
    
    # Read and parse the JSON file
    with open(file_path, 'r') as f:
        trials = json.load(f)
    
    # Gather all choice trials (excluding attention checks)
    choice_trials = []
    for trial in trials:
        # Only process selection trials with choices
        if "task" in trial and trial["task"] == "selection" and "type" in trial and trial["type"] == "choice":
            if "is_attention_check" in trial and trial["is_attention_check"] == True:
                # Skip attention check trials
                continue
            
            choice_trials.append(trial)
    
    # Sort the choice trials by trial_index
    choice_trials.sort(key=lambda x: x.get("trial_index", 0))
    
    # Process trial data with sequential trial_num
    for i, trial in enumerate(choice_trials, 1):  # Start trial_num at 1
        # Extract the required fields
        trial_data = {
            "subject_id": subject_id,
            "trial_num": i,  # Use sequential number instead of trial_index
            "scenario_id": trial.get("scenario_id"),
            "partner_status": trial.get("relationship"),
            "time": trial.get("time"),
            "partner_choice": trial.get("partner_choice"),
            "participant_choice": trial.get("participant_choice"),
            "coordination": trial.get("coordination"),
            "passed_attention_checks": trial.get("passed_attention_checks")
        }
        
        data_rows.append(trial_data)
    
    # Process exit survey data
    exit_survey = None
    for trial in trials:
        if "task" in trial and trial["task"] == "exit-survey":
            response = trial.get("response", {})
            
            # Get the most recent values for passed_attention_checks, total_points, and total_pay
            last_trial = trials[-1]  # The last trial in the array
            
            exit_survey = {
                "subject_id": subject_id,
                "gender": response.get("gender"),
                "age": response.get("age"),
                "understood": response.get("understood"),
                "realPeople": response.get("realPeople"),
                "comments": response.get("comments"),
                "passed_attention_checks": last_trial.get("passed_attention_checks"),
                "total_points": last_trial.get("total_points"),
                "total_pay": last_trial.get("total_pay")
            }
            
            exit_survey_rows.append(exit_survey)
            break
    
    # If we didn't find an exit survey, try to extract the information from the last trial
    if exit_survey is None and len(trials) > 0:
        last_trial = trials[-1]
        
        # Create a default exit survey with available information
        exit_survey = {
            "subject_id": subject_id,
            "gender": "",
            "age": "",
            "understood": "",
            "realPeople": "",
            "comments": "",
            "passed_attention_checks": last_trial.get("passed_attention_checks"),
            "total_points": last_trial.get("total_points"),
            "total_pay": last_trial.get("total_pay")
        }
        
        exit_survey_rows.append(exit_survey)
    
    # Add bonus calculation
    if exit_survey:
        total_pay = float(exit_survey.get("total_pay", 0))
        bonus = round(total_pay - 3, 2)
        bonus_rows.append({
            "subject_id": subject_id,
            "bonus": bonus
        })

# Find and process all JSON files in the raw directory
json_files = glob.glob(os.path.join(RAW_DIR, "*.json"))
for file_path in json_files:
    process_json_file(file_path)

# Write data to CSV files
with open(data_csv_path, 'w', newline='') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=data_headers)
    writer.writeheader()
    writer.writerows(data_rows)

with open(exit_survey_csv_path, 'w', newline='') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=exit_survey_headers)
    writer.writeheader()
    writer.writerows(exit_survey_rows)

with open(bonus_csv_path, 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    for row in bonus_rows:
        writer.writerow([row['subject_id'], row['bonus']])

print(f"Processed {len(json_files)} participant files")
print(f"Wrote {len(data_rows)} interaction trials to {data_csv_path}")
print(f"Wrote {len(exit_survey_rows)} exit surveys to {exit_survey_csv_path}")
print(f"Wrote {len(bonus_rows)} bonus calculations to {bonus_csv_path}")