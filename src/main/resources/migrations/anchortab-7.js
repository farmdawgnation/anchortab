db.tabs.find().forEach(function(tab) {
  tab.service.userId = tab.userId;

  if (tab.service.jsonClass == "LeadGenerationServiceWrapper") {
    tab.service.tabId = tab._id;
  }

  db.tabs.save(tab);
});
