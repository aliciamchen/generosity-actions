import json
import random

# Load stimuli and extract unique scenario IDs (excluding 'attention')
with open("../json/stimuli.json", "r") as f:
    stimuli = json.load(f)

unique_ids = sorted(set(s["id"] for s in stimuli if s["id"] != "attention"))

# Define the 6 partner types
partner_types = [
    {"relationship": "higher", "first_action": "give"},
    {"relationship": "higher", "first_action": "receive"},
    {"relationship": "equal", "first_action": "give"},
    {"relationship": "equal", "first_action": "receive"},
    {"relationship": "lower", "first_action": "give"},
    {"relationship": "lower", "first_action": "receive"},
]

# Generate 504 counterbalancing assignments
assignments = []
while len(assignments) < 504:
    shuffled = partner_types[:]
    random.shuffle(shuffled)
    for i in range(6):
        rotated = shuffled[i:] + shuffled[:i]
        assignments.append(rotated)
        if len(assignments) == 504:
            break

# Create the final assignment mapping for each condition ID
final_assignments = []
for condition_id, partner_assignment in enumerate(assignments, start=1):
    scenario_assignment = {}
    for sid, partner in zip(unique_ids, partner_assignment):
        scenario_assignment[sid] = partner
        print(scenario_assignment)
    final_assignments.append(scenario_assignment)

with open("../json/counterbalancing.json", "w") as out_file:
    json.dump(final_assignments, out_file, indent=2)

print("Done")
