import json
import random
random.seed(111)

all_stories = [
    "conversation",
    "coffee",
    "gifts",
    "housemates",
    "family meals",
    "police",
    "concerts",
    "architects",
    "feedback",
    "meeting location",
    "group project",
    "meeting prep",
    "restaurant",
    "groceries",
    "weekend",
    "scheduling",
    "babysitting",
    "fundraising",
]

all_conditions = [
    ["more", "alice"],
    ["equal", "alice"],
    ["less", "alice"],
    ["more", "bob"],
    ["equal", "bob"],
    ["less", "bob"],
    ["more", "alice"],
    ["equal", "alice"],
    ["less", "alice"],
    ["more", "bob"],
    ["equal", "bob"],
    ["less", "bob"],
    ["more", "alice"],
    ["equal", "alice"],
    ["less", "alice"],
    ["more", "bob"],
    ["equal", "bob"],
    ["less", "bob"]
]


def make_trial_sequence(story_list, condition_list):
    assert len(story_list) == len(condition_list)
    return list(
        map(
            lambda story, condition: {"story": story, "altruistic_status": condition[0], "first_meeting": condition[1]},
            story_list,
            condition_list,
        )
    )


# loop through stories, assign to conditions. should be a list of 18
def make_counterbalancing_once(stories):
    counterbalance_seq = []
    for trial_idx in range(len(stories)):
        stories_temp = stories[trial_idx:] + stories[:trial_idx]
        this_trial_seq = make_trial_sequence(stories_temp, all_conditions)
        counterbalance_seq.append(this_trial_seq)
    return counterbalance_seq


first_eighteen = make_counterbalancing_once(all_stories)
random.shuffle(all_stories)
second_eighteen = make_counterbalancing_once(all_stories)
random.shuffle(all_stories)
third_eighteen = make_counterbalancing_once(all_stories)

counterbalancing = first_eighteen + second_eighteen + third_eighteen

with open("../json/full_counterbalancing.json", "w") as f:
    json.dump(counterbalancing, f)
