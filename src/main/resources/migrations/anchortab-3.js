db.tabs.update({}, {"$unset": {"subscribers": true}}, false, true);
