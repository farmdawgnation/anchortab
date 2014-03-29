db.tabs.find().forEach(function(tab) {
  tab.appearance.customSubmitButtonText = "Submit";
  tab.appearance.customMobileTabText = "Subscribe";
  db.tabs.save(tab);
});
