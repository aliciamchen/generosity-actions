var nCombinations = null;

$.ajax({
  async: false,
  global: false,
  url: "json/full_counterbalancing.json",
  dataType: "json",
  success: function (data) {
    nCombinations = data.length;
  },
});

function fetchTrialParams(condition_number) {
  var thisParticipantTrialParams = null;

  $.ajax({
    async: false,
    global: false,
    url: "json/full_counterbalancing.json",
    dataType: "json",
    success: function (data) {
      thisParticipantTrialParams = data[condition_number - 1];
    },
  });


  var trials = null;

  $.ajax({
    async: false,
    global: false,
    url: "json/stimuli.json",
    dataType: "json",
    success: function (data) {
      function returnTrial(trialParams) {
        return _.filter(data, trialParams)[0];
      }

      trials = _.map(thisParticipantTrialParams, returnTrial);
    },
  });

  return trials;
}

function fetchAttentionTrialParams() {
  var trial = null;

  $.ajax({
    async: false,
    global: false,
    url: "json/stimuli.json",
    dataType: "json",
    success: function (data) {
      trial = data.filter((x) => x.relationship == "attention")[0]
    },
  });

  return trial;
}