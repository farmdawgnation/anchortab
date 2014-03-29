db.tabs.find().forEach(function(tab) {
  tab.appearance.textColor = "#FFFFFF";
  db.tabs.save(tab);
});
