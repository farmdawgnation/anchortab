db.tabs.find({"service": {"$exists": false}}).forEach(function(tab) {
  var user = db.users.findOne({"_id": tab.userId});

  if (user === null) {
    db.tabs.remove({userId: tab.userId});
  } else {
    tab.service = {
      "jsonClass" : "LeadGenerationServiceWrapper",
      "targetEmail" : user.email,
    }

    db.tabs.save(tab);
  }
});
