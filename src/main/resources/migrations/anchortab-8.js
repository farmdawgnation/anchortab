db.tabs.find().forEach(function(tab) {
  tab.appearance.colorScheme.textColor = "#FFFFFF";
  db.tabs.save(tab);
});
